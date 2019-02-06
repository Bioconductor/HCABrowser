## helper functions for iterating through paged responses

.ignore_fields <- c(
#    'links_json'#, 'project_json'
)

#' @importFrom dplyr full_join
.composite_join <- function(x, y)
{
    x_name <- colnames(x)
    x_name <- x_name[grepl('file_name|manifest.files.name', x_name)]
    x_name <- x_name[1]
    y_name <- colnames(y)
    y_name <- y_name[grepl('file_name|manifest.files.name', y_name)]
    y_name <- y_name[1]

    by <- c(y_name)
    names(by) <- x_name
    res <- suppressWarnings(full_join(x, y, by=by))

    res[[y_name]] <- res[[x_name]]
    res
}

#' @importFrom plyr rbind.fill
#' @importFrom stringr str_sub
#' @importFrom tidyr crossing
.parse_postSearch_results <- function(results)
{
    all_files <- lapply(seq_along(results), function(i) {
        ## files.files_json
        field_names <- names(results[[i]][["metadata"]][["files"]])
        field_names <- field_names[grepl('file_json', field_names)]
        json_files <- lapply(field_names, function(x) {
            oo <- .obtain_files(results[[i]], x)
            names(oo) <- paste0(x, '.', names(oo))
            oo
        })
        json_files <- Reduce(.composite_join, json_files)
        #names(json_files) <- field_names
        
        ## manifest.files
        manifest_files <- results[[i]][['metadata']][['manifest']][['files']]
        if (!is.null(manifest_files)) {
            manifest_files <- do.call(rbind.data.frame, c(manifest_files, list(stringsAsFactors=FALSE)))
            names(manifest_files) <- paste0('manifest.files.', names(manifest_files))
        }
        else
            manifest_files <- list()
        
        ## files.*_process_json
        field_names <- names(results[[i]][["metadata"]][["files"]])
        field_names <- field_names[grepl('process_json', field_names)]
        json_processes <- lapply(field_names, function(x) {
            oo <- .obtain_process_files(results[[i]], x)
            names(oo) <- paste0(x, '.', names(oo))
            oo
        })
        json_processes <- do.call(cbind, json_processes)
        
        concat_values <- list()
        if (length(manifest_files) > 0)
            concat_values <- c(concat_values, list(manifest_files))
        if (length(json_files) > 0)
            concat_values <- c(concat_values, list(json_files))
        if (length(json_processes) > 0)
            concat_values <- c(concat_values, list(json_processes))
        res <- as.data.frame(matrix(ncol = 0, nrow = 1))
        if (length(concat_values) > 0)
            res <- Reduce(.composite_join, concat_values)
        res
    })

    ## aquire bundle ids
    bundle_fqids <- lapply(seq_along(results), function(i) {
        reps <- nrow(all_files[[i]])
        if (reps == 0)
            reps <- 1
        a <- rep(results[[i]][["bundle_fqid"]], reps)
        a <- as.data.frame(a)
        names(a) <- 'bundle_fqid'
        a
    })

    ## aquire bundles urls
    bundle_urls <- lapply(seq_along(results), function(i) {
        reps <- nrow(all_files[[i]])
        if (reps == 0)
            reps <- 1
        a <- rep(results[[i]][["bundle_url"]], reps)
        a <- as.data.frame(a)
        names(a) <- 'bundle_url'
        a
    })

    ## acquire other manifest entries
    bundle_else <- lapply(seq_along(results), function(i) {
        values <- results[[i]][['metadata']][['manifest']]
        values[['files']] <- NULL
        names <- names(values)
        names <- paste0('manifest.', names)
        reps <- nrow(all_files[[i]])
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

    ## acquire rest of json schema information for bundles
    json_bundles <- lapply(seq_along(results), function(i) {
        n <- NULL
        if(!is.null(all_files[[i]]))
            n <- nrow(all_files[[i]])
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

    all_files <- lapply(seq_along(results), function(i) {
        do.call(cbind.data.frame, c(list(bundle_fqids[[i]], bundle_urls[[i]],
            all_files[[i]], bundle_else[[i]], json_bundles[[i]])))
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
    x <- cbind(x, a, stringsAsFactors = FALSE)
    x
}

.obtain_content <- function(results, field_name, n)
{
    x <- results[["metadata"]][["files"]][[field_name]]
    x <- unlist(x)
    x <- as.data.frame(as.list(x), stringsAsFactors=FALSE)
#    spl <- split(unname(x), names(x))
#    x <- do.call(tidyr::crossing, spl)
    #x <- unique(as.data.frame(split(unname(x), names(x)), stringsAsFactors=FALSE))
    names <- names(x)
    names <- paste0(field_name, '.', names)
    names(x) <- names
    #x <- as.data.frame(as.list(x), stringsAsFactors=FALSE)
    x[rep(seq_len(nrow(x)), n), , drop=FALSE]
    x
}
