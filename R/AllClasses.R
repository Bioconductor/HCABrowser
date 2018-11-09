
.Term <- setClass("Term",
    slots = c(
        field = "character",
        operator = "character",
        value = "character"
    )
)

.Terms <- setClass("Terms",
    slots = c(
        field = "character",
        operator = "character",
        value = "character"
    )
)

.Range <- setClass("Range",
    slots = c(
        field = "character",
        operator = "character",
        value = "character"
    )
)

## Score agnostic
.Filter <- setClass("Filter",
    slots = c(
        entries = "list"
    )
)

.TermsSet <- setClass("TermsSet",
    slots = c(
        entries = "list"
    )
)

## Score agnostic
.MustNot <- setClass("MustNot",
    slots = c(
        entries = "list"
    )
)

.Bool <- setClass("Bool",
    slots = c(
        filter = "Filter",
        must_not = "MustNot"
    )
)

.Query <- setClass("Query",
    slots = c(
        bool = "Bool"
    )
)

.EsSource <- setClass("EsSource",
    slots = c(
        entries = "character"
    )
)

.EsQuery <- setClass("EsQuery",
    slots = c(
        query = 'Query',
        es_source = 'EsSource'
    )
)

setMethod('show', 'EsQuery', function(object) {
    filter <- object@query@bool@filter@entries
    must_not <- object@query@bool@must_not@entries
    es_source <- object@es_source@entries
    cat('EsQuery:\n',
        ' Query:\n',
        '   Bool:\n')
    if (length(filter) > 0) {
        cat('      Filter:\n')
        for (i in filter)
            cat(paste0('        ', class(i), ': ', i@field, ' ', i@operator, ' ', i@value, '\n'))
    }
    if (length(must_not) > 0) {
        cat('      Must Not:\n')
        for (i in filter)
            cat('        ', class(i), ':', i@field, i@operator, i@value, '\n')
    }
    if (length(es_source) > 0) {
        cat('  Columns selected:\n')
        for (i in es_source)
            cat('   ', i, '\n')
    }
})

.init_HumanCellAtlas <- function(hca)
{
    select(hca,
           c("manifest.files.name", "manifest.files.uuid",
             "manifest.files.content.type", "manifest.files.size")
    )
    #postSearch(hca, 'aws', 'raw', per_page=10)
}

.parse_term_range <- function(x)
{
    if(is(x, "Term")) {
        a <- list(x@value)
        names(a) <- .convert_names_to_filters(x@field)
        list(term = a)
    }
    else {
        a <- list(x@value)
        names(a) <- .range_ops[[x@operator]]
        a <- list(a)
        names(a) <- .convert_names_to_filters(x@field)
        list(range = a)
    }
}

.convert_to_query <- function(es)
{
    filter <- es@query@bool@filter@entries
    must_not <- es@query@bool@must_not@entries
    es_source <- es@es_source

    filter <- lapply(filter, .parse_term_range)
    must_not <- lapply(must_not, .parse_term_range)

    es_query <-list(query = list(bool = list()))
    if (length(filter) > 0)
        es_query$query$bool$filter <- filter
    if (length(must_not) > 0)
        es_query$query$bool$must_not <- must_not
    if (length(filter) == 0 && length(must_not) == 0)
        es_query <- list(query = NULL)

    es_source <- .convert_names_to_filters(es_source@entries)
    es_source <- as.list(es_source)
    if (length(es_source) > 0)
        es_query$"_source" <- es_source

    es_query
}

.SearchResult <- setClass("SearchResult",
    slots = c(
        first_hit = 'integer',
        last_hit = 'integer',
        total_hits = 'integer',
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
        results = "SearchResult",
        expression = "call"
    )
)

HumanCellAtlas <-
    function(url='https://dss.integration.data.humancellatlas.org/v1')
{
    hca <- .HumanCellAtlas(url=url)
    .init_HumanCellAtlas(hca)
}

#.show_HumanCellAtlas <- function(object)

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

setMethod('results', 'HumanCellAtlas', function(object) {
    object@results@results
})

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


.show_SearchResult <- function(object)
{
    cat('class: ', class(object), "\n", 
        "  bundle ", first_hit(object), " - ", last_hit(object), " of ",
            total_hits(object), "\n",
        "  link: ", length(link(object))>0, "\n",
        sep = ''
    )
}

setMethod('show', 'SearchResult', .show_SearchResult)

.show_HumanCellAtlas <- function(object)
{
    cat('Using hca-dcp at:\n  ', object@url, '\n\n')
    show(object@es_query)
    cat('\n')
    show(object@results)
}

setMethod('show', 'HumanCellAtlas', .show_HumanCellAtlas)
