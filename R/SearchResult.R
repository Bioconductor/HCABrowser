#' Results from searching the HCA
#'
#' @rdname SearchResult
#'
#' @description A 'SearchResult' is returned by `searchBundles()`, and
#'     represents the results for the search. Use `results()` to view
#'     current results, and `nextResults()` to page through results.
#'
#' @import methods
#'
#' @exportClass SearchResult
.SearchResult <- setClass(
    "SearchResult",
    slots = c(
        es_query = 'list',
        results = 'list',
        total_hits = 'integer',
        first_hit = 'integer',
        last_hit = 'integer',
        output_format = "character",
        replica = "character",
        per_page = "integer",
        link = "character"
    )
)

## not exported
SearchResult <-
    function(es_query, results, first_hit, last_hit, total_hits,
             output_format, replica, per_page, link = character(0))
{
    .SearchResult(
        es_query=es_query, results=results, first_hit = first_hit,
        last_hit = last_hit, total_hits=total_hits,
        output_format = output_format, replica = replica, per_page = per_page,
        link = link
    )
}

.first_hit <- function(x) x@first_hit

.last_hit <- function(x) x@last_hit

.total_hits <- function(x) x@total_hits

.es_query <- function(x) x@es_query

.link <- function(x) x@link

.output_format <- function(x) x@output_format

.replica <- function(x) x@replica

.per_page <- function(x) x@per_page

.results <- function(x) x@results

#' @rdname SearchResult
#'
#' @return `nextResults()` returns a `SearchResult` representing the
#'     next 'page' of results from the original query.
#'
#' @importFrom jsonlite toJSON
#'
#' @importFrom httr content_type_json POST
#'
#' @examples
#' bundles <- searchBundles(HCABrowser())  # Find all bundles...
#' bundles                                 # ... but retrieve first 100
#'
#' nextResults(bundles)                    # ... and then next 100
#'
#' @export
nextResults <-
    function(x)
{
    stopifnot(is(x, "SearchResult"))

    link <- .link(x)
    if (!length(link)) {
        message("no more results available")
        return(x)
    }
    body <- toJSON(.es_query(x))

    re <- '^<(.*)>; rel="([[:alpha:]]+)"$'
    url <- sub(re, "\\1", link)
    rel <- sub(re, "\\2", link)

    response <- POST(url, content_type_json(), body = body)
    .stop_for_status(response, "nextResults()")

    .as_SearchResult(
        response, .es_query(x),
        .output_format(x), .replica(x), .per_page(x),
        first_hit = .last_hit(x) + 1L
    )
}

#' @rdname SearchResult
#'
#' @param x A SearchResult returned by `searchBundles()`.
#'
#' @return `results()` returns a list-of-lists, with each element
#'     representing a distinct results of the HCABrowser query.
#'
#' @examples
#' bundles %>%
#'     ## results() returns a list-of-lists
#'     results() %>%
#'     ## this is the structure of the first result
#'     head(1) %>%
#'     str()
#'
#' @export
results <- .results

#' @importFrom tibble tibble as_tibble
.as_tibble_SearchResult_summary <-
    function(x)
{
    search_score <- vapply(x, function(elt) {
        elt <- elt[["search_score"]]
        if (is.null(elt)) NA_real_ else elt
    }, numeric(1))

    tibble(
        bundle_fqid = vapply(x, `[[`, character(1), "bundle_fqid"),
        bundle_url = vapply(x, `[[`, character(1), "bundle_url"),
        search_score = search_score
    )
}

.as_tibble_SearchResult_raw <-
    function(x)
{
    stop("'as_tibble()' on 'raw' search results not yet implemented")
}

#' @rdname SearchResult
#'
#' @param ... additional parameters (unused)
#' 
#' @return The `as_tibble` method for `SearchResult` returns a tibble
#'     representing the search result, with each row corresponding to
#'     a distinct result.
#'
#' @examples
#' bundles %>% as_tibble()
#'
#' @export
as_tibble.SearchResult <-
    function(x, ...)
{
    fmt <- .output_format(x)
    results <- results(x)
    switch(
        fmt,
        summary = .as_tibble_SearchResult_summary(results),
        raw = .as_tibble_SearchResult_raw(results),
        default = 
            stop(
                "'as_tibble()' 'SearchResults' output format not supported: ",
                fmt
            )
    )
}

#' @rdname SearchResult
#'
#' @param object a SearchResult object.
#'
#' @importFrom methods show
#'
#' @export
setMethod('show', 'SearchResult', function(object) {
    bundle_msg <- paste(
        .first_hit(object), "-", .last_hit(object), "of", .total_hits(object)
    )
    cat(
        "class: ", class(object), "\n",
        "bundles: ", bundle_msg, "\n",
        "output_format: ", .output_format(object), "\n",
        "replica: ", .replica(object), "\n",
        sep = ''
    )
})
