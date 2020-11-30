setOldClass('quosure')

setOldClass('quosures')

#' Browse the HCA
#'
#' @rdname HCABrowser
#' @md
#'
#' @author Daniel Van Twisk
#'
#' @importFrom AnVIL Service
#'
#' @exportClass HCABrowser
.HCABrowser <- setClass(
    "HCABrowser",
    contains = "Service",
    slots = c(
        es_query = "quosures",
        es_source = "quosures",
        search_term = "list",
        per_page = "numeric"
    )
)

#' @rdname HCABrowser
#' @md
#'
#' @description Use `HCABrowser()` to create an object to connect with
#'     the Human Cell Atlas Data Coordination Platform Data Storage
#'     System (HCA DCP DSS).
#'
#' @param host character(1) path to hca-dcp server
#'
#' @param api_url character(1) path to hca-dcp api file
#'
#' @param per_page numeric(1) numbers of pages to view at a time.
#'
#' @param authenticate logical(1) authenticate? required for actions
#'     that update the HCA data repostiory (rare).
#'
#' @return `HCABrowser()` returns an object representing the
#'     connection to the HCA DCP DSS.
#'
#' @examples
#' HCABrowser()
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

#' @rdname HCABrowser
#' @md
#'
#' @details `getEsQuery()`, `undoEsQuery()`, and `resetEsQuery()`
#'     retrieve, remove the most recent, or remove all filters added
#'     to an `HCABrowser` object via `filter()`.
#'
#' @param x An HCABrowser object
#'
#' @return `getEsQuery() returns a JSON object representing the
#'     elastic search query formed by application of `filter()` and
#'     `select()` to an HCABrowser object.
#'
#' @examples
#' hca <-
#'     HCABrowser() %>%
#'     filter(organ.text == brain) %>%
#'     filter(organ.text == heart)
#'
#' hca
#'
#' str(getEsQuery(hca))
#'
#' @export
getEsQuery <-
    function(x)
{
    stopifnot(is(x, "HCABrowser"))

    query <- x@es_query
    if (length(query) == 0)
        query <- list(es_query =NULL)
    else
        query <- .temp(query)
    query
}

#' @rdname HCABrowser
#' @md
#'
#' @param n integer(1) the number of filter queries to undo
#'
#' @return `undoEsQuery()` returns an HCABrowser object with n fewer
#'     queries.
#'
#' @importFrom utils head
#'
#' @examples
#' hca %>%
#'     undoEsQuery()
#'
#' @export
undoEsQuery <-
    function(x, n = 1L)
{
    n <- as.integer(n)
    stopifnot(
        is(x, "HCABrowser"),
        .is_scalar_integer(n)
    )

    check <- length(x@es_query) - n
    x@search_term <- list()
    if (check < 1) {
        resetEsQuery(x)
    } else {
        x@es_query <- head(x@es_query, -n)
        x@es_source <- head(x@es_source, -n)
        filter(x)
    }
}

#' @rdname HCABrowser
#'
#' @return `resetEsQuery()` returns an HCABrowser object with no
#'     queries.
#'
#' @examples
#' hca %>%
#'     resetEsQuery()
#'
#' @export
resetEsQuery <-
    function(x)
{
    stopifnot(is(x, "HCABrowser"))

    x@search_term <- list()
    x@es_query <- quos()
    x@es_source <- quos()
    x
}

#' @importFrom httr content headers
.as_SearchResult <-
    function(response, es_query, output_format, replica, per_page,
             first_hit = 1L)
{
    result <- content(response)
    n <- as.integer(length(result[['results']]))
    link <- headers(response)$link
    if (is.null(link))
        link <- character()

    SearchResult(
        output_format = output_format,
        replica = replica,
        per_page = per_page,
        es_query = es_query,
        results=result$results,
        first_hit = first_hit,
        last_hit = first_hit + n - 1L,
        total_hits=result$total_hits,
        link = link
    )
}

#' @rdname HCABrowser
#' @md
#'
#' @description `searchBundles()` searches the HCA DCP DSS for bundles
#'     matching the filters applied to the HCA object.
#'
#' @details `searchBundles()` can return `summary` or `raw`
#'     results. `summary` results include the `bundle_fqid` (unique
#'     identifier) and `bundle_url` (location of a JSON file with
#'     complete bundle information). Up to 500 `summary` or 10 `raw`
#'     results can be returned per query; use `nextResults()` to
#'     'page' through subsequent bundles.
#'
#' @param output_format character(1). Specifies the output
#'     format. Either "summary" or "raw". The default format,
#'     "summary", is a list of UUIDs for bundles that match the
#'     query. Set this parameter to "raw" to get the verbatim JSON
#'     metadata for bundles that match the query.
#'
#' @param replica character(1). A replica to fetch form. Can either be
#'     set to "aws" or "gcp".  DEFAULT is "aws".
#'
#' @param per_page numeric(1). Max number of results to return per page.
#'
#' @examples
#' searchBundles(HCABrowser(), per_page = 10L) # All records!
#' HCABrowser() %>%
#'     filter(files.specimen_from_organism_json.organ.text == "brain") %>%
#'     searchBundles() %>%
#'     as_tibble()
#'
#' @export
searchBundles <-
    function(x, output_format = c('summary', 'raw'), replica = c('aws', 'gcp'),
             per_page = 100L)
{
    per_page <- as.integer(per_page)
    output_format <- match.arg(output_format)
    replica <- match.arg(replica)
    stopifnot(
        is(x, "HCABrowser"),
        .is_scalar_integer(per_page)
    )

    FUN <- x$Find_bundles_by_searching_their_metadata_with_an_Elasticsearch_query
    json_request_body <- getEsQuery(x)

    response <- FUN(
        json_request_body = json_request_body, output_format = output_format,
        replica = replica, per_page = per_page
    )
    response <- .stop_for_status(response, "searchBundles()")

    .as_SearchResult(response, getEsQuery(x), output_format, replica, per_page)
}

#' @rdname HCABrowser
#'
#' @param object An HCAbrowser object to show
#'
#' @export
setMethod('show', 'HCABrowser', function(object) {
    query <- vapply(.es_query(object), function(query) {
        deparse(rlang::quo_get_expr(query), width.cutoff = 500)
    }, character(1))

    selection <- character()
    ##    for(i in object@es_source)
    ##        cat(
    ##            paste(strwrap(paste0('"', unname(rlang::eval_tidy(i)), '"', collapse = ', '), indent = 2, exdent = 2), collapse = "\n"),
    ##        "\n")
    cat(
        "class: ", class(object), "\n",
        "Using x-dcp at: ", object@api$host, "\n",
        "Current query:", paste(c("", query), collapse = "\n  "), "\n",
        "Current selection: ", paste(c("", selection), collapse = "\n  "), "\n",
        sep = ""
    )
})
