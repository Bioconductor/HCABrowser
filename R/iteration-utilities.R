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

.exclude_filter <- c(
    'describedBy'
)

#' @importFrom plyr rbind.fill
#' @importFrom stringr str_sub
.parse_postSearch_results <- function(results)
{
    #results <- results(results)

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
    })

    bundle_files <- lapply(seq_along(results), function(i) {
        if (is.null(results[[i]][['metadata']][['manifest']]))
            return(NULL)
        a <- do.call(rbind.data.frame, results[[i]][['metadata']][['manifest']][['files']])
        if (!is.null(json_files[[i]]))
            a <- a[a$name %in% json_files[[i]]$file_core.file_name,]
        a[order(a$name),]
    })

    bundle_files <- lapply(seq_along(json_files), function(i) {
        if (is.null(bundle_files[[i]]))
            return(NULL)
        offset <- nrow(json_files[[i]]) - nrow(bundle_files[[i]])
        if (length(offset) == 0)
            offset <- 0
        dd <- as.data.frame(matrix(NA, offset, ncol(bundle_files[[i]])))
        colnames(dd) <- colnames(bundle_files[[i]])
        rbind(bundle_files[[i]], dd)
    })

    ## aquire bundle ids
    bundle_fqids <- lapply(seq_along(results), function(i) {
        a <- rep(results[[i]][["bundle_fqid"]], length(bundle_files[[i]][['name']]))
        a <- as.data.frame(a)
        names(a) <- 'bundle_fqid'
        a
    })

    ## aquire bundles urls
    bundle_urls <- lapply(seq_along(results), function(i) {
        a <- rep(results[[i]][["bundle_url"]], length(bundle_files[[i]][['name']]))
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
            return(NULL)
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
#    browser()
    content_dir <- results[["metadata"]][["files"]][[field_name]]
    files <- lapply(seq_along(content_dir), function(i) {
        data.frame(as.list(unlist(content_dir[[i]])))
    })
    x <- do.call(plyr::rbind.fill, files)
    x <- x[!duplicated(x),]
}

.obtain_content <- function(results, field_name, n)
{
#    sub <- .field_sub_directory[[field_name]]
#    if (field_name %in% c('project_json')) {
        x <- results[["metadata"]][["files"]][[field_name]]
        x <- unlist(x)
#        nam <- paste0(basename(x['describedBy']), '.', names(x))
#        names(x) <- nam
        x <- as.data.frame(as.list(x), stringsAsFactors=FALSE)
        x <- x[rep(seq_len(nrow(x)), n), , drop=FALSE]
        x
#    }
#    else {
#        content_dir <- results[["metadata"]][["files"]][[field_name]][[sub]]
#        res <- lapply(content_dir, function(x) {
#            x <- unlist(x)
#            nam <- paste0(basename(x['content.describedBy']), '.', names(x))
#            names(x) <- nam
#            x
#        })
#        res <- as.data.frame(as.list(unlist(res)))
#        res[rep(seq_len(nrow(res)), n), ]
#    }
}
