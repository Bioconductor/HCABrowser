## helper functions for iterating through paged responses

## Currently supports only one per field, may be more than one
.field_sub_directory <- list(
    biomaterial_json = 'biomaterials',
    file_json = 'files',
    links_json = 'links',
    process_json = 'processes',
    project_json = character(), ## no subdirectory
    protocol_json = 'protocols'
)

.ignore_fields <- c(
    'links_json', 'project_json'
#    'analysis_process_json',
#    'process_json'
)

#' @importFrom plyr rbind.fill
#' @importFrom stringr str_sub
#' @importFrom tidyr crossing
.parse_postSearch_results <- function(results)
{
    browser()
    json_files <- lapply(seq_along(results), function(i) {
        field_names <- names(results[[i]][["metadata"]][["files"]])
        field_names <- field_names[!field_names %in% .ignore_fields]
        field_names <- field_names[grepl('file_json', field_names)]
        dfs <- lapply(field_names, function(x) {
            .obtain_files(results[[i]], x)
        })
        names(dfs) <- field_names
        dfs
    })

    json_files <- lapply(seq_along(json_files), function(i) {
        x <- do.call(plyr::rbind.fill, json_files[[i]])
        #x <- as.data.frame(json_files[[i]])
        if (is.null(x))
            return(data.frame(matrix(nrow=1, ncol=0)))
        if (!nrow(x) == 0) {
            x <- mutate_if(x, is.factor, as.character)
 #           x[order(x['file_core.file_name']), , drop=FALSE]
            x
        }
        else
            data.frame(matrix(nrow=1, ncol=0))
    })

    bundle_files <- lapply(seq_along(results), function(i) {
        if (is.null(results[[i]][['metadata']][['manifest']][['files']]))
            return(NULL)
        a <- do.call(rbind.data.frame, results[[i]][['metadata']][['manifest']][['files']])
        if (length(json_files[[i]]) > 0)
            a <- a[a$name %in% json_files[[i]]$file_core.file_name,]
#        a <- a[order(a$name), , drop = FALSE]
        names <- names(a)
        names <- paste0('manifest.files.', names)
        names(a) <- names
        a
    })

    ## Correct potential offset if more json_files exist than meta data files
    bundle_files <- lapply(seq_along(json_files), function(i) {
        if (is.null(bundle_files[[i]]))
            return(data.frame(matrix(nrow=1, ncol=0)))
        if (length(json_files[[i]]) == 0)
            return(bundle_files[[i]])
        offset <- nrow(json_files[[i]]) - nrow(bundle_files[[i]])
        if (length(offset) == 0)
            offset <- 0
        dd <- as.data.frame(matrix(NA, offset, ncol(bundle_files[[i]])))
        colnames(dd) <- colnames(bundle_files[[i]])
        rbind(bundle_files[[i]], dd)
    })

    bundle_else <- lapply(seq_along(results), function(i) {
        values <- results[[i]][['metadata']][['manifest']]
        values[['files']] <- NULL
        names <- names(values)
        names <- paste0('manifest.', names)
        reps <- length(bundle_files[[i]][['name']])
        if (reps == 0)
            reps <- 1
        if (length(values) == 0)
            return(data.frame(matrix(nrow=1, ncol=0)))
        else {
            df <- as.data.frame(values)
            df <- df[rep(seq_along(nrow(df)), reps),]
            names(df) <- names
            df
        }
    })

    bundle_processes <- lapply(seq_along(results), function(i) {
        reps <- length(bundle_files[[i]][['name']])
        if (reps == 0)
            reps <- 1
        field_names <- names(results[[i]][["metadata"]][["files"]])
        field_names <- field_names[!field_names %in% .ignore_fields]
        field_names <- field_names[grepl('process_json', field_names)]
        dfs <- lapply(field_names, function(x) {
            .obtain_process_files(results[[i]], x, reps)
        })
        names(dfs) <- field_names
        if (length(dfs) == 0)
            dfs <- data.frame(nrow=1, ncol=0)
        dfs
    })

#    bundle_projects <- lapply(seq_along(results), function(i) {
#        reps <- length(bundle_files[[i]][['name']])
#        if (reps == 0)
#            reps <- 1
#        field_names <- names(results[[i]][["metadata"]][["files"]])
#        field_names <- field_names[!field_names %in% .ignore_fields]
#        field_names <- field_names[grepl('project_json', field_names)]
#        dfs <- lapply(field_names, function(x) {
#            .obtain_project_files(results[[i]], x, reps)
#        })
#        names(dfs) <- field_names
#        if (length(dfs) == 0)
#            dfs <- data.frame(nrow=1, ncol=0)
#        dfs
#    })

    ## aquire bundle ids
    bundle_fqids <- lapply(seq_along(results), function(i) {
        reps <- length(bundle_files[[i]][['name']])
        if (reps == 0)
            reps <- 1
        a <- rep(results[[i]][["bundle_fqid"]], reps)
        a <- as.data.frame(a)
        names(a) <- 'bundle_fqid'
        a
    })

    ## aquire bundles urls
    bundle_urls <- lapply(seq_along(results), function(i) {
        reps <- length(bundle_files[[i]][['name']])
        if (reps == 0)
            reps <- 1
        a <- rep(results[[i]][["bundle_url"]], reps)
        a <- as.data.frame(a)
        names(a) <- 'bundle_url'
        a
    })

    ## acquire rest of json schema information for bundles
    json_bundles <- lapply(seq_along(results), function(i) {
        browser()
        n <- NULL
        if(!is.null(bundle_files[[i]]))
            n <- nrow(bundle_files[[i]])
        if (is.null(n))
            n <- 1
        field_names <- names(results[[i]][["metadata"]][["files"]])
        field_names <- field_names[!field_names %in% .ignore_fields]
        field_names <- field_names[!grepl('file_json|process_json', field_names)]
        dfs <- lapply(field_names, function(x) {
            .obtain_content(results[[i]], x, n)
        })
        #names(dfs) <- field_names
        dfs
    })

    json_bundles <- lapply(seq_along(json_bundles), function(i) {
        if (is.null(json_bundles[[i]]))
            return(NULL)
        do.call(cbind, json_bundles[[i]])
    })

    all_files <- lapply(seq_along(bundle_files), function(i) {
        do.call(tidyr::crossing, c(list(bundle_files[[i]], bundle_fqids[[i]],
            bundle_urls[[i]], bundle_else[[i]], bundle_processes[[i]]), json_files[[i]],
            json_bundles[[i]]))
    })
    
    all_files <- do.call(plyr::rbind.fill, all_files)

    if (is.null(all_files))
        all_files <- data.frame()

    all_files
}

.obtain_files <- function(results, field_name)
{
    content_dir <- results[["metadata"]][["files"]][[field_name]]
    files <- lapply(seq_along(content_dir), function(i) {
        data.frame(as.list(unlist(content_dir[[i]])))
    })
    x <- do.call(plyr::rbind.fill, files)
    x <- x[!duplicated(x), , drop=FALSE]
    x
}

.obtain_process_files <- function(results, field_name, n)
{
    content_dir <- results[["metadata"]][["files"]][[field_name]]
    x <- data.frame(nrow=0, ncol=1)
    outputs <- content_dir[[1]][["outputs"]]
    if (!is.null(outputs)) {
        x <- do.call(rbind.fill, lapply(outputs, as.data.frame))
        names(x) <- paste0('outputs.', names(x))
        ## order?
        content_dir[[1]][["outputs"]] <- NULL
    }
    a <- as.data.frame(content_dir)
    if (!is.null(outputs))
        n <- length(outputs)
    if(!missing(n)) {
        a <- a[rep(seq_len(nrow(a)), n), , drop=FALSE]
    }
    cbind(x, a)
}

.obtain_project_files <- function(results, field_name, n)
{
    x <- results[["metadata"]][["files"]][[field_name]]
    x <- unlist(x)
    x <- as.data.frame(as.list(x), stringsAsFactors=FALSE)
#    x <- unique(as.data.frame(split(unname(x), names(x)), stringsAsFactors=FALSE))
#    names <- names(x)
#    names <- paste0(field_name, '.', names)
#    names(x) <- names
    #x <- as.data.frame(as.list(x), stringsAsFactors=FALSE)
    x[rep(seq_len(nrow(x)), n), , drop=FALSE]
}

.obtain_content <- function(results, field_name, n)
{
    x <- results[["metadata"]][["files"]][[field_name]]
    x <- unlist(x)
#    x <- as.data.frame(as.list(x), stringsAsFactors=FALSE)
    spl <- split(unname(x), names(x))
    x <- do.call(tidyr::crossing, spl)
    #x <- unique(as.data.frame(split(unname(x), names(x)), stringsAsFactors=FALSE))
    names <- names(x)
    names <- paste0(field_name, '.', names)
    names(x) <- names
    #x <- as.data.frame(as.list(x), stringsAsFactors=FALSE)
    x[rep(seq_len(nrow(x)), n), , drop=FALSE]
}
