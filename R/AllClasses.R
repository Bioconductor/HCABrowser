###' @importFrom tibble tibble
.HumanCellAtlas <- setClass("HumanCellAtlas",
#    contains = "tibble",
    slots = c(
        url = "character"
    )
)

HumanCellAtlas <-
    function(url='https://dss.integration.data.humancellatlas.org/v1')
{
    .HumanCellAtlas(url=url)
}

.SearchResult <- setClass("SearchResult",
    slots = c(
        first_hit = 'integer',
        last_hit = 'integer',
        total_hits = 'integer',
        es_query = 'list',
        results = 'list',
        link = 'character'
    )
)

.first_hit <- function(object) object@first_hit
.last_hit <- function(object) object@last_hit
.total_hits <- function(object) object@total_hits
.es_query <- function(object) object@es_query
.results <- function(object) object@results
.link <- function(object) object@link

setGeneric('first_hit', function(object, ...) standardGeneric('first_hit'))
setGeneric('last_hit', function(object, ...) standardGeneric('last_hit'))
setGeneric('total_hits', function(object, ...) standardGeneric('total_hits'))
setGeneric('es_query', function(object, ...) standardGeneric('es_query'))
setGeneric('results', function(object, ...) standardGeneric('results'))
setGeneric('link', function(object, ...) standardGeneric('link'))

setMethod('first_hit', 'SearchResult', .first_hit)
setMethod('last_hit', 'SearchResult', .last_hit)
setMethod('total_hits', 'SearchResult', .total_hits)
setMethod('es_query', 'SearchResult', .es_query)
setMethod('results', 'SearchResult', .results)
setMethod('link', 'SearchResult', .link)

.show_SearchResult <- function(object)
{
    cat('class: ', class(object), "\n",
        #"  es_query: ", es_query(object)[[1]], "\n",
        "  bundle ", first_hit(object), " - ", last_hit(object), " of ",
            total_hits(object), "\n",
        #"  results: ", length(results(object)), "\n",
        "  link: ", length(link(object))>0, "\n",
        sep = ''
    )
}

setMethod('show', 'SearchResult', .show_SearchResult)
