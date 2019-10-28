
.init_HCABrowser <- function(hca)
{
    select(hca, .initial_source)
#    postSearch(hca, 'aws', 'raw', per_page=10)
}

.init_ProjectBrowser <- function(project)
{
    res <- filter(project)
    res
}

#' @importFrom tibble tibble
#' @importFrom dplyr %>%
setOldClass('tbl_df')
.SearchResult <- setClass("SearchResult",
    slots = c(
        first_hit = 'integer',
        last_hit = 'integer',
        total_hits = 'integer',
        results = 'tbl_df',
        link = 'character'
    )
)

setOldClass('quosure')
setOldClass('quosures')

#' The HCABrowser Class
#'
#' @author Daniel Van Twisk
#'
#' @param url character(1) the url of the Human Cell Atlas resource.
#' @param fields_path character(1) path to the fields json file.
#' @param per_page numeric(1) numbers of pages to view at a time.
#' @exportClass HCABrowser
.HCABrowser <- setClass("HCABrowser",
    slots = c(
        activated = "character",
        url = "character",
        fields_path = "character",
        supported_fields = "tbl_df",
        es_query = "quosures",
        es_source = "quosures",
        search_term = "list",
        results = "SearchResult",
        per_page = "numeric"
    )
)

#' @importFrom dplyr mutate_if
.get_supportedFields <- function(hca)
{
    if(is.null(hca))
        field_names=names(jsonlite::fromJSON(system.file("extdata", "fields_and_values.json", package="HCABrowser")))
    else
        field_names <- names(jsonlite::fromJSON(hca@fields_path))
    names_split <- strsplit(field_names, '[.]')
    abbreviated_names <- vapply(names_split, function(x) {
        short_name <- c()
        for(i in rev(seq_along(x))){
            if(i != length(x))
                short_name <- paste0(x[i], '.', short_name)
            else
                short_name <- x[i]
            uni <- field_names[grepl(paste0('[.]', short_name, '$'), field_names)]
            if (length(uni) == 1) {
#                if (i + 1 == length(x)) {
#                    second_split <- strsplit(short_name, '[.]')[[1]]
#                    uni <- field_names[grepl(paste0('[.]', second_split, '[.]'), field_names)]
#                    if (length(uni) == 1)
#                        short_name <- second_split[1]
#                }
                return(short_name)
            }
        }
        short_name
    }, character(1))
    df <- cbind(abbreviated_names, field_names)

    manifest_full <- .manifest_fields
    manifest_full <- c(manifest_full, 'manifest.creator_uid', 'manifest.format', 'manifest.version')
    manifest_full <- data.frame(abbreviated_names = manifest_full, field_names = manifest_full)
    df <- rbind(df, manifest_full)
    df <- mutate_if(df, is.factor, as.character)

    df <- df[order(df[,1]),]

    as_tibble(df)
}

#' The HCABrowser Class
#'
#' @author Daniel Van Twisk
#'
#' @param url character(1) the url of the Human Cell Atlas resource.
#' @param fields_path character(1) path to the fields json file.
#' @param per_page numeric(1) numbers of pages to view at a time.
#'
#' @return an HCABrowser object
#'
#' @examples
#' hca <- HCABrowser()
#' hca
#' @importFrom methods new
#' @export
HCABrowser <-
    function(url='https://dss.data.humancellatlas.org/v1',
             fields_path=system.file("extdata", "fields_and_values.json", package="HCABrowser"),
             per_page=10)
{
    hca <- .HCABrowser(url=url, fields_path=fields_path, per_page=per_page, activated="bundles", search_term=list(), es_query=quos(), es_source=quos(), supported_fields = tibble())
    hca@supported_fields <- .get_supportedFields(hca)
    .init_HCABrowser(hca)
}
