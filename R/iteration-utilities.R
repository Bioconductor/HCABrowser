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
#    'file_json',
    'links_json'
)

#' @importFrom plyr rbind.fill
#' @importFrom stringr str_sub
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
        if (!is.null(x))
            x[order(x['file_core.file_name']),]
        else
            data.frame(matrix(nrow=1, ncol=0))
    })

    bundle_files <- lapply(seq_along(results), function(i) {
        if (is.null(results[[i]][['metadata']][['manifest']]))
            return(NULL)
        a <- do.call(rbind.data.frame, results[[i]][['metadata']][['manifest']][['files']])
        if (length(json_files[[i]]) > 0)
            a <- a[a$name %in% json_files[[i]]$file_core.file_name,]
        a[order(a$name), , drop = FALSE]
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

    ## aquire bundle ids
    bundle_fqids <- lapply(seq_along(results), function(i) {
        reps <- length(bundle_files[[i]][['name']])
        if(reps == 0)
            reps <- 1
        a <- rep(results[[i]][["bundle_fqid"]], reps)
        a <- as.data.frame(a)
        names(a) <- 'bundle_fqid'
        a
    })

    ## aquire bundles urls
    bundle_urls <- lapply(seq_along(results), function(i) {
        reps <- length(bundle_files[[i]][['name']])
        if(reps == 0)
            reps <- 1
        a <- rep(results[[i]][["bundle_url"]], reps)
        a <- as.data.frame(a)
        names(a) <- 'bundle_url'
        a
    })

    ## acquire rest of json schema information for bundles
    json_bundles <- lapply(seq_along(results), function(i) {
        n <- NULL
        if(!is.null(bundle_files[[i]]))
            n <- nrow(bundle_files[[i]])
        if (is.null(n))
            n <- 1
        field_names <- names(results[[i]][["metadata"]][["files"]])
        field_names <- field_names[!field_names %in% .ignore_fields]
        field_names <- field_names[!grepl('file_json', field_names)]
        dfs <- lapply(field_names, function(x) {
            .obtain_content(results[[i]], x, n)
        })
        names(dfs) <- field_names
        dfs
    })

    json_bundles <- lapply(seq_along(json_bundles), function(i) {
        if (is.null(json_bundles[[i]]))
            return(NULL)
        do.call(cbind, json_bundles[[i]])
    })

    all_files <- lapply(seq_along(bundle_files), function(i) {
        do.call(cbind.data.frame, c(list(bundle_files[[i]], bundle_fqids[[i]],
            bundle_urls[[i]]), json_files[[i]], json_bundles[[i]]))
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
    x <- x[!duplicated(x),]
}

.obtain_content <- function(results, field_name, n)
{
    x <- results[["metadata"]][["files"]][[field_name]]
    x <- unlist(x)
    x <- as.data.frame(as.list(x), stringsAsFactors=FALSE)
    x[rep(seq_len(nrow(x)), n), , drop=FALSE]
}
