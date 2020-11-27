
#' @importFrom dplyr %>%
.SearchResult <- setClass("SearchResult",
    slots = c(
        es_query = 'list',
        results = 'list',
        total_hits = 'integer',
        first_hit = 'integer',
        last_hit = 'integer',
        link = 'character'
    )
)

#' The SearchResults Class
#'
#' @description A glass generated after parsing a search query with
#'  the method parseToSearchResults. Contains a list of all information
#'  gleaned from the search query.
#'
#' @param es_query A quosure of the current es_query.
#' @param results A list of all result from the qeury.
#' @param first_hit numeric(1) the first bundle currently shown.
#' @param last_hit numeric(1) the last bundle currently shown.
#' @param total_hits numeric(1) the number of bundles that can be shown.
#'
#' @return A Search Result object
#'
#' @examples
#'  sr <- new("SearchResult")
#'  sr
#'
#' @export
SearchResult <-
    function(es_query, results, first_hit, last_hit, total_hits)
{
    .SearchResult(es_query=es_query, results=results, first_hit = first_hit,
                  last_hit = last_hit, total_hits=total_hits)
}

setOldClass('quosure')
setOldClass('quosures')

#' The HCABrowser Class
#'
#' @author Daniel Van Twisk
#'
#' @param per_page numeric(1) numbers of pages to view at a time.
#' @param host character(1) path to hca-dcp server
#' @param api_url character(1) path to schema
#'
#' @return An HCABrowser object.
#'
#' @examples
#'  hca <- HCABrowser()
#'  hca
#'
#' @exportClass HCABrowser
.HCABrowser <- setClass("HCABrowser",
    contains=c("Service"),
    slots = c(
        es_query = "quosures",
        es_source = "quosures",
        search_term = "list",
        results = "SearchResult",
        per_page = "numeric"
    )
)

#' The HCABrowser Class
#'
#' @author Daniel Van Twisk
#'
#' @param per_page numeric(1) numbers of pages to view at a time.
#' @param host character(1) path to hca-dcp server
#' @param api_url character(1) path to hca-dcp api file
#' @param authenticate logical(1) authenticate? required for actions
#'     that update the HCA data repostiory (rare).
#'
#' @return An HCABrowser object.
#'
#' @examples
#'  hca <- HCABrowser()
#'  hca
#'
#' @export
HCABrowser <-
    function(host='dss.data.humancellatlas.org',
             api_url='https://dss.data.humancellatlas.org/v1/swagger.json',
             per_page=10,
             authenticate = FALSE)
{
    stopifnot(
        is.character(host), length(host) == 1L, !is.na(host),
        is.character(api_url), length(api_url) == 1L, !is.na(api_url),
        is.numeric(per_page), length(per_page) == 1L, !is.na(per_page),
        is.logical(authenticate), length(authenticate) == 1L,
        !is.na(authenticate)
    )
    .HCABrowser(
        Service(
            service = "HCA",
            config = httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L),
            host = host,
            api_url = api_url,
            authenticate = authenticate
        ),
        es_query=quos(),
        es_source=quos()
    )
}
