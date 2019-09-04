
#' @importFrom plyr rbind.fill
.parse_project_get <- function(project, results)
{
    upfront <- lapply(results[[1]], function(x) {
        temp <- x
#        temp["fileTypeSummaries"] <- NULL
#        temp["projectSummary"] <- NULL
        #temp <- unlist(temp)
        temp <- lapply(temp, function(z) lapply(z, function(x) lapply(x, function(y) paste(y, collapse=", "))))
        temp <- unlist(temp)
        df <- as.data.frame(t(matrix(temp)))
        names(df) <- names(temp)
#        names(df) <- names(temp)
#        df
        df
    })
    project@terms <- results[['termFacets']]
    upfront <- do.call(rbind.fill, upfront)
    project@results <- as_tibble(upfront)
    project@search_after <- curl::curl_escape(results[[2]][["search_after"]])
    project@search_after_uid <- curl::curl_escape(results[[2]][["search_after_uid"]])
    project
}

