
.EsElement <- setClass("EsElement",
    contains = "VIRTUAL"
)

.Term <- setClass("Term",
    contains = "EsElement",
    slots = c(
        entries = "list"
    )
)

.Range <- setClass("Range",
    contains = "EsElement",
    slots = c(
        entries = "list"
    )
)

## Score agnostic
.Filter <- setClass("Filter",
    contains = "EsElement",
    slots = c(
        entries = "list"
    )
)

## Score agnostic
.MustNot <- setClass("MustNot",
    contains = "EsElement",
    slots = c(
        entries = "list"
    )
)

.Bool <- setClass("Bool",
    contains = "EsElement",
    slots = c(
        must = "Filter",
        must_not = "MustNot"
    )
)

.Query <- setClass("Query",
    contains = "EsElement",
    slots = c(
        bool = "Bool"
    )
)

.EsSource <- setClass("EsSource",
    contains = "EsElement",
    slots = c(
        entries = "character"
    )
)

.EsQuery <- setClass("EsQuery",
    contains = "EsElement",
    slots = c(
        query = 'Query',
        es_source = 'EsSource'
    )
)

.init_EsQuery <- function()
{
    
}

.SearchResult <- setClass("SearchResult",
    slots = c(
        first_hit = 'integer',
        last_hit = 'integer',
        total_hits = 'integer',
        es_query = 'list',
        results = 'data.frame',
        link = 'character'
    )
)

#' @importFrom tibble tibble
.HumanCellAtlas <- setClass("HumanCellAtlas",
#    contains = "tibble",
    slots = c(
        url = "character",
        es_query = "EsQuery",
        results = "SearchResult"
    )
)

HumanCellAtlas <-
    function(url='https://dss.integration.data.humancellatlas.org/v1')
{
    hca <- .HumanCellAtlas(url=url)
    hca@results <- postSearch(hca, 'aws', 'raw', es_query=list(query=NULL), per_page=10)
    hca
}

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

setMethod('results', 'HumanCellAtlas', .results)

setMethod('first_hit', 'SearchResult', .first_hit)
setMethod('last_hit', 'SearchResult', .last_hit)
setMethod('total_hits', 'SearchResult', .total_hits)
setMethod('es_query', 'SearchResult', .es_query)
setMethod('results', 'SearchResult', .results)
setMethod('link', 'SearchResult', .link)

.update_es_query <- function(object){message('hi')}
.reset_es_query <- function(object) object@es_query_filter <- NULL

setGeneric('.updateEsQuery', function(object, ...) standardGeneric('.updateEsQuery'))
setGeneric('resetEsQuery', function(object, ...) standardGeneric('resetEsQuery'))

setMethod('.updateEsQuery', 'HumanCellAtlas', .update_es_query)
#' @export
setMethod('resetEsQuery', 'HumanCellAtlas', .reset_es_query)


#.show_es_query <- function(object) {
#    cat(
#        '  es_query:\n',
#        '    filter: ', object@es_query_filter, '\n',
#        '    select: ', object@es_query_select, '\n',
#    )
#}

.show_SearchResult <- function(object)
{
    cat('class: ', class(object), "\n", 
#        '  es_query:\n',
#        '    filter: ', object@es_query_filter, '\n',
#        '    select: ', object@es_query_select, '\n',
        "  bundle ", first_hit(object), " - ", last_hit(object), " of ",
            total_hits(object), "\n",
        #"  results: ", length(results(object)), "\n",
        "  link: ", length(link(object))>0, "\n",
        sep = ''
    )
}

setMethod('show', 'SearchResult', .show_SearchResult)
