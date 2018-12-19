
.init_HumanCellAtlas <- function(hca)
{
#    select(hca,
#           c("project_title", "project_shortname", "organ",
#             "library_construction_approach.text",
#             "specimen_from_organism_json.genus_species.text",
#             "files.donor_organism_json.diseases.text")
#    )
    postSearch(hca, 'aws', 'raw', per_page=100)
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

setOldClass('quosure')
setOldClass('quosures')
#' @export
.HumanCellAtlas <- setClass("HumanCellAtlas",
    slots = c(
        activated = "character",
        url = "character",
        es_query = "quosures",
        es_source = "quosures",
        search_term = "list",
        results = "SearchResult",
        per_page = "numeric"
    )
)

#' @export
HumanCellAtlas <-
    function(url='https://dss.data.humancellatlas.org/v1', per_page=10)
{
    hca <- .HumanCellAtlas(url=url, per_page=per_page, activated="bundles", search_term=list(), es_query=quos(), es_source=quos())
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
    if (object@activated == 'bundles')
        res <- res %>% distinct(bundle_fqid, .keep_all = TRUE)
    res
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

setGeneric('pullBundles', function(hca, ...) standardGeneric('pullBundles'))
setGeneric('showBundles', function(hca, bundle_fqids, ...) standardGeneric('showBundles'))
setGeneric('downloadHCA', function(hca, ...) standardGeneric('downloadHCA'))

setMethod('.updateEsQuery', 'HumanCellAtlas', .update_es_query)
setMethod('resetEsQuery', 'HumanCellAtlas', .reset_es_query)

#' @importFrom tidygraph activate
#' @export
activate.HumanCellAtlas <-
    function(object, type=c('bundles', 'files'), ...)
{
    type <- match.arg(type)
    if(type == 'bundles')
        object@activated <- 'bundles'
    else if(type == 'files')
        object@activated <- 'files'
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

.pullBundles <-
    function(hca, ...)
{
    hca %>% downloadHCA() %>% pull('bundle_fqid') %>% as.character()
}

#' @export
setMethod('pullBundles', 'HumanCellAtlas', .pullBundles)

.showBundles <- function(hca, bundle_fqids, ...)
{
    hca <- hca %>% activate('files')
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
    cat('Using hca-dcp at:\n ', object@url, '\n\n')
    cat('Current Query:\n')
    for(i in object@es_query)
        cat(' ', deparse(rlang::quo_get_expr(i), width.cutoff = 500), '\n')
    cat('\n')
    cat('Current Selection:\n')
    for(i in object@es_source)
        cat(' ', deparse(unname(rlang::eval_tidy(i)), width.cutoff = 500), '\n')
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

setMethod('show', 'HumanCellAtlas', .show_HumanCellAtlas)

