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
    'file_json',
    'links_json'
)

.exclude_filter <- c(
    'describedBy'
)

#' @importFrom stringr str_sub
.dataframe_from_postSearch_results <- function(results)
{
    results <- results(results)

    ## aquire bundle ids
    bundle_fqids <- vapply(results, function(i) {
        i[["bundle_fqid"]]
    }, character(1))

    ## aquire bundles urls
    bundle_urls <- vapply(results, function(i) {
        i[["bundle_url"]]
    }, character(1))

    bundle_files <- lapply(results, function(i) {
        a <- do.call(rbind.data.frame, i[['metadata']][['manifest']][['files']])
        a[!grepl('.json$', a$name),]
    })

    ## acquire rest of json schema information for bundles
    json_bundles <- lapply(seq_along(results), function(i) {
        field_names <- names(results[[i]][["metadata"]][["files"]])
        field_names <- field_names[!field_names %in% .ignore_fields]
        dfs <- lapply(field_names, function(x) {
            .obtain_content(results[[i]], x, length(bundle_files[[i]]))
        })
        names(dfs) <- field_names
        dfs
    })

    browser()

    lapply(bundle_files, function(jsons) {
        lapply(jsons, function(i) {
            
        })
    })

    ## combine evertyhing to one table
#    json_bundles['bundle_fqids'] <- rep(bundle_fqids, each=permutation_val)
#    json_bundles['bundle_urls'] <- rep(bundle_urls, each=permutation_val)

    json_bundles
}

.obtain_content <- function(results, field_name, n)
{
    sub <- .field_sub_directory[[field_name]]
#    if (field_name %in% 'file_json')
#        do.call(rbind.data.frame, content_dir)
    if (field_name %in% c('project_json')) {
        x <- results[["metadata"]][["files"]][[field_name]]
        x <- unlist(x)
        nam <- paste0(basename(x['describedBy']), '.', names(x))
        names(x) <- nam
        x <- as.data.frame(as.list(x))
        x <- x[rep(seq_len(nrow(x)), n), ]
    }
    else {
        content_dir <- results[["metadata"]][["files"]][[field_name]][[sub]]
        lapply(content_dir, function(x) {
            x <- unlist(x)
            nam <- paste0(basename(x['content.describedBy']), '.', names(x))
            names(x) <- nam
            x <- as.data.frame(as.list(x))
            x <- x[rep(seq_len(nrow(x)), n), ]
            browser()
            do.call(cbind, x)
        })
    }
}
