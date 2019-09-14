
#' @importFrom dplyr %>%
.SearchResult <- setClass("SearchResult",
    slots = c(
        es_query = 'list',
        results = 'list',
        total_hits = 'integer'
        #first_hit = 'integer',
        #last_hit = 'integer',
        #link = 'character'
    )
)

#' @export
SearchResult <-
    function(es_query, results, total_hits)
{
    .SearchResult(es_query=es_query, results=results, total_hits=total_hits)
}

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
    function(host='dss.data.humancellatlas.org',
             api_url='https://dss.data.humancellatlas.org/v1/swagger.json',
             per_page=10)
{
    hca <- .HCABrowser(
        Service(
            service = "HCABrowser",
            host = host,
            api_url=api_url,
            config = httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L, http_version = 0L),
            package = "HCABrowser",
            schemes = "https"
        ),
        es_query=quos(),
        es_source=quos()
    )

    hca
}

.first_hit <- function(object) object@first_hit
.last_hit <- function(object) object@last_hit
.total_hits <- function(object) object@total_hits
.es_query <- function(object) object@es_query
.priv_results <- function(object) object@results
.link <- function(object) object@link

setGeneric('first_hit', function(object, ...) standardGeneric('first_hit'))
setGeneric('last_hit', function(object, ...) standardGeneric('last_hit'))
setGeneric('total_hits', function(object, ...) standardGeneric('total_hits'))
setGeneric('es_query', function(object, ...) standardGeneric('es_query'))
setGeneric('results', function(object, ...) standardGeneric('results'))
setGeneric('link', function(object, ...) standardGeneric('link'))

setGeneric('getEsQuery', function(object, ...) standardGeneric('getEsQuery'))

setMethod('first_hit', 'SearchResult', .first_hit)
setMethod('last_hit', 'SearchResult', .last_hit)
setMethod('total_hits', 'SearchResult', .total_hits)
setMethod('es_query', 'SearchResult', .es_query)

.getEsQuery <-
    function(object)
{
    query <- object@es_query
    if (length(query) == 0)
        query <- list(es_query =NULL)
    else
        query <- .temp(query)
    query
}

#' Get Elastic Search query as JSON
#'
#' @param hca An HCABrowser object
#'
#' @return A json object of the elastic search query in the HCABrowser object
#' @export
setMethod('getEsQuery', 'HCABrowser', .getEsQuery)

#' Get results of SearchResult object
#'
#' @param object A Searchresult to obtain the result slot value from
#'
#' @return tibble of the results of the HCABrowser query
#'
#' @export
setMethod('results', 'SearchResult', .priv_results)
setMethod('link', 'SearchResult', .link)

#' @export
setGeneric('undoEsQuery', function(hca, ...) standardGeneric('undoEsQuery'))
setGeneric('resetEsQuery', function(hca, ...) standardGeneric('resetEsQuery'))

setGeneric('per_page', function(hca, ...) standardGeneric('per_page'))

.set_per_page <-
    function(hca, n)
{
    if (n > 10)
        message('The HCABrowser is unable to show bundle results of more than 10 per_page')
    hca@per_page <- n
    select(hca, c())
}

#' Set per_page argument of an HCABrowser object
#'
#' @description note that no more than 10 pages can be displayed at once
#'
#' @param hca a HCABrowser object
#' @param n the new per_page value
#'
#' @return a HCABrowser with updated per_page value
#'
#' @name per_page
#' @aliases per_page,HCABrowser-method
#' @docType methods
#'
#' @examples
#' hca <- HCABrowser()
#' #hca <- per_page(hca, 5)
#' hca
#' @importFrom utils head
#' @export
setMethod('per_page', 'HCABrowser', .set_per_page)

.undo_esquery <-
    function(hca, n = 1L)
{
    check <- length(hca@es_query) - n
    hca@search_term <- list()
    if (check < 1)
        resetEsQuery(hca)
    else{
        hca@es_query <- head(hca@es_query, -c(n))
        hca@es_source <- head(hca@es_source, -c(n))
        filter(hca)
    }
}

#' Undo previous filter queries on a HCABrowser object
#'
#' @param hca A HCABrowser object
#' @param n integer(1) the number of filter queries to undo
#'
#' @return A HCABrowser object with n fewer queries
#'
#' @name undoEsQuery
#' @aliases undoEsQuery,HCABrowser-method
#' @docType methods
#'
#' @examples
#' hca <- HCABrowser()
#' hca <- hca %>% filter(organ.text == brain)
#' hca <- hca %>% filter(organ.text == heart)
#' hca <- hca %>% filter(organ.text != brain)
#' #hca <- hca %>% undoEsquery(n = 2)
#' hca
#' @export
setMethod('undoEsQuery', 'HCABrowser', .undo_esquery)

.reset_esquery <-
    function(hca)
{
    hca@search_term <- list()
    hca@es_query <- quos()
    hca@es_source <- quos()
    select(hca, .initial_source)  
}

#' Reset the query of a HCABrowser object to the default query
#'
#' @param hca A HCABrowser object
#' 
#' @return A HCABrowser object with the search reset
#'
#' @name resetEsQuery
#' @aliases resetEsQuery,HCABrowser-method
#' @docType methods
#'
#' @examples
#' hca <- HCABrowser()
#' hca <- hca %>% filter(organ.text == brain)
#' hca <- hca %>% filter(organ.text != brain)
#' hca <- hca %>% resetEsQuery
#' hca
#' @importFrom dplyr pull
#' @export
setMethod('resetEsQuery', 'HCABrowser', .reset_esquery)

.show_HCABrowser <- function(object)
{
    cat('class:', class(object), '\n')
    cat('Using hca-dcp at:\n ', object@url, '\n\n')
    cat('Current Query:\n')
    for(i in object@es_query)
        cat(' ', deparse(rlang::quo_get_expr(i), width.cutoff = 500), '\n')
    cat('\n')
    cat('Current Selection:\n')
#    for(i in object@es_source)
#        cat(
#            paste(strwrap(paste0('"', unname(rlang::eval_tidy(i)), '"', collapse = ', '), indent = 2, exdent = 2), collapse = "\n"),
#        "\n")
    cat('\n')
    cat('class: ', class(object@results), "\n", 
        "  bundle ", first_hit(object@results), " - ", last_hit(object@results), " of ",
            total_hits(object@results), "\n",
        "  link: ", length(link(object@results))>0, "\n",
        sep = ''
    )
    cat('\n')
    cat('Showing', object@activated, 'with', object@per_page ,'results per page\n')
    print(results(object))
}
