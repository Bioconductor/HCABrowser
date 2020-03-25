#' @import S4Vectors curl jsonlite plyr stringr tibble tidygraph tidyr
#' @importFrom methods new
#' @importFrom BiocFileCache bfcquery
#' @importFrom AnVIL Service

.init_HCABrowser <- function(x)
{
    select(x, .initial_source)
    #    postSearch(x, 'aws', 'raw', per_page=10)
}

.first_hit <- function(x) x@first_hit
.last_hit <- function(x) x@last_hit
.total_hits <- function(x) x@total_hits
.es_query <- function(x) x@es_query
.priv_results <- function(x) x@results
.link <- function(x) x@link

setMethod('first_hit', 'SearchResult', .first_hit)
setMethod('last_hit', 'SearchResult', .last_hit)
setMethod('total_hits', 'SearchResult', .total_hits)
setMethod('es_query', 'SearchResult', .es_query)

#' Get results of SearchResult object
#'
#' @param x A Searchresult to obtain the result slot value from
#'
#' @return list of the results of the HCABrowser query
#'
#' @examples
#'  sr <- new("SearchResult")
#'  results(sr)
#'
#' @name results
#' @aliases results,SearchResult-method
#'
#' @export
setMethod('results', 'SearchResult', .priv_results)
setMethod('link', 'SearchResult', .link)

.getEsQuery <-
    function(x)
{
    query <- x@es_query
    if (length(query) == 0)
        query <- list(es_query =NULL)
    else
        query <- .temp(query)
    query
}

#' Get Elastic Search query as JSON
#'
#' @param x An HCABrowser object
#'
#' @return A json object of the elastic search query in the HCABrowser object
#' @export
setMethod('getEsQuery', 'HCABrowser', .getEsQuery)

.set_per_page <- function(x, n)
{
    x@per_page <- n
    x
}

#' Set per_page argument of an HCABrowser object
#'
#' @description note that no more than 10 pages can be displayed at once
#'
#' @param x an HCABrowser object
#' @param n the new per_page value
#'
#' @return a HCABrowser with updated per_page value
#'
#' @name per_page
#' @aliases per_page,HCABrowser-method
#' @docType methods
#'
#' @examples
#' x <- HCABrowser()
#' #x <- per_page(x, 5)
#' x
#' @importFrom utils head
#' @export
setMethod('per_page', 'HCABrowser', .set_per_page)

.undo_esquery <-
function(x, n = 1L)
{
    check <- length(x@es_query) - n
    x@search_term <- list()
    if (check < 1)
    resetEsQuery(x)
    else{
        x@es_query <- head(x@es_query, -c(n))
        x@es_source <- head(x@es_source, -c(n))
        filter(x)
    }
}

#' Undo previous filter queries on a HCABrowser object
#'
#' @param x A HCABrowser object
#' @param n integer(1) the number of filter queries to undo
#'
#' @return A HCABrowser object with n fewer queries
#'
#' @name undoEsQuery
#' @aliases undoEsQuery,HCABrowser-method
#' @docType methods
#'
#' @examples
#' x <- HCABrowser()
#' x <- x %>% filter(organ.text == brain)
#' x <- x %>% filter(organ.text == heart)
#' x <- x %>% filter(organ.text != brain)
#' #x <- x %>% undoEsquery(n = 2)
#' x
#' @export
setMethod('undoEsQuery', 'HCABrowser', .undo_esquery)

.reset_esquery <-
function(x)
{
    x@search_term <- list()
    x@es_query <- quos()
    x@es_source <- quos()
    select(x, .initial_source)
}

#' Reset the query of a HCABrowser object to the default query
#'
#' @param x A HCABrowser object
#'
#' @return A HCABrowser object with the search reset
#'
#' @name resetEsQuery
#' @aliases resetEsQuery,HCABrowser-method
#' @docType methods
#'
#' @examples
#' x <- HCABrowser()
#' x <- x %>% filter(organ.text == brain)
#' x <- x %>% filter(organ.text != brain)
#' x <- x %>% resetEsQuery
#' x
#' @importFrom dplyr pull
#' @export
setMethod('resetEsQuery', 'HCABrowser', .reset_esquery)

.show_SearchResult <- function(object)
{
    cat('class: ', class(object), "\n",
    "  bundle ", first_hit(object), " - ", last_hit(object), " of ",
    total_hits(object), "\n",
    "  link: ", length(link(object))>0, "\n",
    sep = ''
    )
}

#' Show Search Result
#'
#' @param object a SearchResult object to show
#'
#' @return outputs a text represntation of the object
#'
#' @importFrom methods show
#'
#' @examples
#'  sr <- new('SearchResult')
#'  sr
#'
#' @export
setMethod('show', 'SearchResult', .show_SearchResult)

.show_HCABrowser <- function(object)
{
    cat('class:', class(object), '\n')
    cat('Using x-dcp at:\n ', object@api$host, '\n\n')
    cat('Current Query:\n')
    for(i in object@es_query)
    cat(' ', deparse(rlang::quo_get_expr(i), width.cutoff = 500), '\n')
    cat('\n')
    cat('Current Selection:\n')
    #    for(i in object@es_source)
    #        cat(
    #            paste(strwrap(paste0('"', unname(rlang::eval_tidy(i)), '"', collapse = ', '), indent = 2, exdent = 2), collapse = "\n"),
    #        "\n")
#    cat('\n')
#    cat('class: ', class(object@results), "\n")#,
#    "  bundle ", first_hit(object@results), " - ", last_hit(object@results), " of ",
#    total_hits(object@results), "\n",
#    "  link: ", length(link(object@results))>0, "\n",
#    sep = ''
#    )
#    cat('\n')
#    cat('Showing', object@activated, 'with', object@per_page ,'results per page\n')
#    print(results(object))
}

#' Show HCABrowser object
#'
#' @param object An HCAbrowser object to show
#'
#' @return outputs a text represntation of the object
#'
#' @export
setMethod('show', 'HCABrowser', .show_HCABrowser)

