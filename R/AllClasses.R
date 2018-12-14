
.Term <- setClass("Term",
    slots = c(
        field = "character",
        operator = "character",
        value = "character"
    )
)

.Match <- setClass("Match",
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

.Must <- setClass("Must",
    slots = c(
        entries = "list"
    )
)

.Should <- setClass("Should",
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
        entries = "list"
    )
)

.Query <- setClass("Query",
    slots = c(
        bool =  "Bool"
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

.nested_query_show <- function(object, depth) {
    classname <- as.character(class(object))
    if (classname %in% c("Term", "Terms", "Range"))
        cat(rep(' ', depth), classname, ':', object@field, object@operator, object@value, '\n') 
    else{
        cat(rep(' ', depth), classname, '\n')
        for(i in object@entries)
            .nested_query_show(i, depth + 1)
    }
}

setMethod('show', 'EsQuery', function(object) {
    query <- object@query@bool@entries
    es_source <- object@es_source@entries
    cat('EsQuery:\n',
        ' Query:\n',
        '   Bool:\n')
    
    for (i in query)
        .nested_query_show(i, 3)
    if (length(es_source) > 0) {
        cat('  Columns selected:\n')
        for (i in es_source)
            cat('   ', i, '\n')
    }
})

.init_HumanCellAtlas <- function(hca)
{
    select(hca,
#           c("manifest.files.name", "manifest.files.uuid",
#            "manifest.files.content.type", "manifest.files.size")
           c("project_title", "project_shortname", "organ",
             "library_construction_approach.text",
             "specimen_from_organism_json.genus_species.text",
             "files.donor_organism_json.diseases.text")
#              "specimen_from_organ_json.genus_species.text", "disease")
    )
#    postSearch(hca, 'aws', 'raw', per_page=10)
}

.parse_term_range <- function(x)
{
    if(is(x, "Filter") || is(x, "MustNot") || is(x, "Should")) {
        name <- class(x) #vapply(x@entries, class, character(1))

        names <- vapply(x@entries, class, character(1))
        names <- tolower(names)

        x <- lapply(x@entries, function(y) {
            names <- tolower(as.character(class(y)))
            li <- .parse_term_range(y)
            names(li) <- names
            li
        })
        list(x)
    }
    else if(is(x, "Bool")) {
        name <- class(x) #vapply(x@entries, class, character(1))

        names <- vapply(x@entries, class, character(1))
        names <- tolower(names)

        x <- lapply(x@entries, function(y) {
            li <- .parse_term_range(y)
            name <- tolower(as.character(class(y)))
            name[name == 'mustnot'] <- 'must_not' 
            names(li) <- name
            li
        })
	
        x
    }
    else if (is(x, "Term")) {
        a <- list(x@value)
        names(a) <- .convert_names_to_filters(x@field)
        list(a)
    }
    else if (is(x, "Terms")){
        a <- list(x@value)
        names(a) <- .convert_names_to_filters(x@field)
        list(a)
    }
    else if (is(x, "Range")){
        a <- list(x@value)
        names(a) <- .range_ops[[x@operator]]
        a <- list(a)
        names(a) <- .convert_names_to_filters(x@field)
        a
    }
}

.convert_to_query <- function(es)
{
    bool <- es@query@bool
    es_source <- es@es_source

    es_query <-list(query = list(bool = list()))

    if(length(bool@entries) > 0) {
        bool <- .parse_term_range(bool)
        bool <- bool[[1]]
        es_query$query$bool <- bool #bool$bool$bool$bool
    }
    else
        es_query <- list(query = NULL)

    es_source <- .convert_names_to_filters(es_source@entries)
    es_source <- as.list(es_source)
    if (length(es_source) > 0)
        es_query$"_source" <- es_source

    es_query
}

#' @importFrom tibble tibble
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

.HumanCellAtlas <- setClass("HumanCellAtlas",
    slots = c(
        activated = "logical",
        url = "character",
        es_query = "EsQuery",
        results = "SearchResult",
        per_page = "numeric"
    )
)

HumanCellAtlas <-
    function(url='https://dss.integration.data.humancellatlas.org/v1', per_page=10)
{
    hca <- .HumanCellAtlas(url=url, per_page=per_page, activated=FALSE)
    .init_HumanCellAtlas(hca)
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

#' importFrom dplyr distinct
setMethod('results', 'HumanCellAtlas', function(object) {
    res <- object@results@results
    if (!object@activated)
        res <- res %>% distinct(bundle_fqid, .keep_all = TRUE)
    res
})

#setMethod('per_page', 'HumanCellAtlas', function(hca) {
#    object@per_page
#})

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

setGeneric('showBundles', function(hca, bundle_fqids, ...) standardGeneric('showBundles'))
setGeneric('downloadHCA', function(hca, ...) standardGeneric('downloadHCA'))

setMethod('.updateEsQuery', 'HumanCellAtlas', .update_es_query)
#' @export
setMethod('resetEsQuery', 'HumanCellAtlas', .reset_es_query)

#' @importFrom tidygraph activate
#' @export
activate.HumanCellAtlas <-
    function(object, ...)
{
    if(object@activated) {
        message("Displaying results by bundle")
        object@activated <- FALSE
    }
    else {
        message("Displaying results by file")
        object@activated <- TRUE
    }
    object
}

.download.HumanCellAtlas <-
    function(hca, ...)
{
    res <- results(hca)
    while (!is.null(hca <- nextResults(hca))) {
        res <- rbind.fill(res, results(hca))
    }
    res
}

#' @export
setMethod("downloadHCA", "HumanCellAtlas", .download.HumanCellAtlas)

.showBundles <- function(hca, bundle_fqids, ...)
{
    if(!hca@activated)
        hca@activated <- TRUE
    hca %>% downloadHCA() %>% filter(bundle_fqid %in% bundle_fqids)
}

#' @export
setMethod('showBundles', 'HumanCellAtlas', .showBundles)

.show_SearchResult <- function(object)
{
    cat('class: ', class(object), "\n", 
        "  bundle ", first_hit(object), " - ", last_hit(object), " of ",
            total_hits(object), "\n",
        "  link: ", length(link(object))>0, "\n",
        sep = ''
    )
    print(results(object))
}

setMethod('show', 'SearchResult', .show_SearchResult)

.show_HumanCellAtlas <- function(object)
{
    cat('class:', class(object), '\n')
    cat('Using hca-dcp at:\n  ', object@url, '\n\n')
    show(object@es_query)
    cat('\n')
    sho <- 'bundles'
    if (object@activated)
        sho <- 'files'
    cat('class: ', class(object@results), "\n", 
        "  bundle ", first_hit(object@results), " - ", last_hit(object@results), " of ",
            total_hits(object@results), "\n",
        "  link: ", length(link(object@results))>0, "\n",
        sep = ''
    )
    cat('\n')
    cat('Showing', sho, 'with', object@per_page ,'results per page\n')
    print(results(object))
}

setMethod('show', 'HumanCellAtlas', .show_HumanCellAtlas)

