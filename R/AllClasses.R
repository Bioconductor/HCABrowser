## TODO columsn won't be displayed if value does not exist for those initial values
.initial_source <- c(
    "project_title", "project_short_name", "organ.text",
    "library_construction_approach.text",
    "specimen_from_organism_json.genus_species.text",
    "disease.text"
)

.init_HumanCellAtlas <- function(hca)
{
    select(hca, .initial_source)
#    postSearch(hca, 'aws', 'raw', per_page=100)
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
        fields_path = "character",
        supported_fields = "tbl_df",
        es_query = "quosures",
        es_source = "quosures",
        search_term = "list",
        results = "SearchResult",
        per_page = "numeric"
    )
)

.get_supportedFields <- function(hca)
{
    if(is.null(hca))
        field_names=names(jsonlite::fromJSON(system.file("extdata", "fields_and_values.json", package="HCABrowser")))
    else
        field_names <- names(jsonlite::fromJSON(hca@fields_path))
    names_split <- strsplit(field_names, '[.]')
    abbreviated_names <- vapply(names_split, function(x) {
        short_name <- c()
        for(i in rev(seq_along(x))){
            if(i != length(x))
                short_name <- paste0(x[i], '.', short_name)
            else
                short_name <- x[i]
            uni <- field_names[grepl(paste0('[.]', short_name, '$'), field_names)]
            if (length(uni) == 1) {
#                if (i + 1 == length(x)) {
#                    second_split <- strsplit(short_name, '[.]')[[1]]
#                    uni <- field_names[grepl(paste0('[.]', second_split, '[.]'), field_names)]
#                    if (length(uni) == 1)
#                        short_name <- second_split[1]
#                }
                return(short_name)
            }
        }
        short_name
    }, character(1))
    df <- cbind(abbreviated_names, field_names)

    manifest_full <- paste0('manifest.', .manifest_fields)
    manifest_full <- data.frame(abbreviated_names = manifest_full, field_names = manifest_full)
    df <- cbind(df, manifest_full)

    df <- df[order(df[,1]),]

    as_tibble(df)
}

#' @export
HumanCellAtlas <-
    function(url='https://dss.data.humancellatlas.org/v1',
             fields_path=system.file("extdata", "fields_and_values.json", package="HCABrowser"),
             per_page=10)
{
    hca <- .HumanCellAtlas(url=url, fields_path=fields_path, per_page=per_page, activated="bundles", search_term=list(), es_query=quos(), es_source=quos(), supported_fields = tibble())
    hca@supported_fields <- .get_supportedFields(hca)
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
    if (nrow(res) > 0 && object@activated == 'bundles')
        res <- res %>% distinct(bundle_fqid, .keep_all = TRUE)
    res
})

setMethod('first_hit', 'SearchResult', .first_hit)
setMethod('last_hit', 'SearchResult', .last_hit)
setMethod('total_hits', 'SearchResult', .total_hits)
setMethod('es_query', 'SearchResult', .es_query)
#' @export
setMethod('results', 'SearchResult', .results)
setMethod('link', 'SearchResult', .link)

setGeneric('undoEsQuery', function(hca, ...) standardGeneric('undoEsQuery'))
setGeneric('resetEsQuery', function(hca, ...) standardGeneric('resetEsQuery'))

setGeneric('per_page', function(hca, ...) standardGeneric('per_page'))

setGeneric('pullBundles', function(hca, ...) standardGeneric('pullBundles'))
setGeneric('showBundles', function(hca, bundle_fqids, ...) standardGeneric('showBundles'))
setGeneric('downloadHCA', function(hca, ...) standardGeneric('downloadHCA'))

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

.set_per_page <-
    function(hca, n)
{
    hca@per_page <- n
    hca
}

#' @export
setMethod('per_page', 'HumanCellAtlas', .set_per_page)

.download.HumanCellAtlas <-
    function(hca, ..., n)
{
    res <- results(hca)
    if (!missing(n)) {
        per_page <- hca@per_page
        times <- floor((n-1)/per_page)
        mod <- n %% per_page
        for(i in seq_len(times)) {
            hca <- nextResults(hca)
            reso <- results(hca)
            if (i == times && mod != 0)
                reso <- reso[seq_len(mod),]
            res <- rbind.fill(res, reso)
        }
    } else {
        res <- results(hca)
        while (!is.null(hca <- nextResults(hca))) {
            res <- rbind.fill(res, results(hca))
        }
    }
    as_tibble(res)
}

#' @export
setMethod("downloadHCA", "HumanCellAtlas", .download.HumanCellAtlas)

.undo_esquery <-
    function(hca, ...)
{
    hca@search_term <- list()
    if (length(hca@es_query) < 2)
        resetEsQuery(hca)
    else{
        hca@es_query[[length(hca@es_query)]] <- NULL
        hca@es_source[[length(hca@es_source)]] <- NULL
        hca
    }
}

#' @export
setMethod('undoEsQuery', 'HumanCellAtlas', .undo_esquery)

.reset_esquery <-
    function(hca, ...)
{
    hca@search_term <- list()
    hca@es_query <- quos()
    hca@es_source <- quos()
    select(hca, .initial_source)  
}

setMethod('resetEsQuery', 'HumanCellAtlas', .reset_esquery)

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
    bundle_fqids <- vapply(strsplit(bundle_fqids, '[.]'), function(x) { x[1] }, character(1))
    hca %>% filter(uuid %in% bundle_fqids)
    #hca %>% downloadHCA() %>% filter(bundle_fqid %in% bundle_fqids)
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

#' @export
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
        cat(strwrap(paste0('"', unname(rlang::eval_tidy(i)), '"', ', ')), "\n")
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

