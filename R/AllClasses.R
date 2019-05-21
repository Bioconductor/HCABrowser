
.init_HCABrowser <- function(hca)
{
    select(hca, .initial_source)
#    postSearch(hca, 'aws', 'raw', per_page=10)
}

#' @importFrom tibble tibble
#' @importFrom dplyr %>%
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

#' The HCABrowser Class
#'
#' @author Daniel Van Twisk
#'
#' @param url character(1) the url of the Human Cell Atlas resource.
#' @param fields_path character(1) path to the fields json file.
#' @param per_page numeric(1) numbers of pages to view at a time.
#' @exportClass HCABrowser
.HCABrowser <- setClass("HCABrowser",
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

#' The Project Browser Class
#'
#' @description A still tentative class that displays Human Cell Atlas
#'  information by projects.
#'
#' @param url character(1) The url of the Human Cell Atlas.
#'
#' @author Daniel Van Twisk
#'
#' @exportClass ProjectBrowser
.ProjectBrowser <- setClass("ProjectBrowser",
    slots = c(
        url = "character",
        project_results = "tbl_df"
    )
)

#' @importFrom dplyr mutate_if
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

    manifest_full <- .manifest_fields
    manifest_full <- c(manifest_full, 'manifest.creator_uid', 'manifest.format', 'manifest.version')
    manifest_full <- data.frame(abbreviated_names = manifest_full, field_names = manifest_full)
    df <- rbind(df, manifest_full)
    df <- mutate_if(df, is.factor, as.character)

    df <- df[order(df[,1]),]

    as_tibble(df)
}

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
    function(url='https://dss.data.humancellatlas.org/v1',
             fields_path=system.file("extdata", "fields_and_values.json", package="HCABrowser"),
             per_page=10)
{
    hca <- .HCABrowser(url=url, fields_path=fields_path, per_page=per_page, activated="bundles", search_term=list(), es_query=quos(), es_source=quos(), supported_fields = tibble())
    hca@supported_fields <- .get_supportedFields(hca)
    .init_HCABrowser(hca)
}

#' The Project Browser Class
#'
#' @description A still tentative class that displays Human Cell Atlas
#'  information by projects.
#'
#' @param url character(1) The url of the Human Cell Atlas.
#'
#' @author Daniel Van Twisk
#'
#' @return a ProjectBrowser object
#'
#' @examples
#' pb <- ProjectBrowser()
#' pb
#' @export
ProjectBrowser <-
    function(url='https://dss.data.humancellatlas.org/v1')
{
    pb <- .ProjectBrowser(url=url, project_results=tibble())
    pb
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

.retrieve_results <-
    function(object)
{
    res <- object@results@results
    if (nrow(res) > 0 && object@activated == 'bundles')
        res <- res %>% distinct(bundle_fqid, .keep_all = TRUE)
    res
}

.results <- function(object, n = object@per_page, all = FALSE, .output_format=c('raw', 'summary')) {
    output_format <- match.arg(.output_format)
    if (output_format == 'summary') {
        object <- object %>% per_page(n=500)
        object <- select(object, c(), .output_format=output_format)
    }
    res <- .retrieve_results(object)
    if (all) {
        res <- .retrieve_results(object)
        while (!is.null(object <- nextResults(object))) {
            res <- rbind.fill(res, .retrieve_results(object))
        }
    } else {
        per_page <- object@per_page
        times <- floor((n-1)/per_page)
        mod <- n %% per_page
        for(i in seq_len(times)) {
            hca <- nextResults(object)
            reso <- .retrieve_results(object)
            if (i == times && mod != 0)
                reso <- reso[seq_len(mod),]
            res <- rbind.fill(res, reso)
        }
        if (times == 0 && mod != 0)
            res <- res[seq_len(mod),]
    }
    as_tibble(res)
}

#' Obtain search results from a HCABrowser Object
#'
#' @description
#'  Returns a tibble either showing bundles or files based on whichever is
#'  activated.
#'
#' @param object A Human Cell Atlas object.
#' @param n numeric(1) number of elements to return.
#' @param all logical(1) whether to return all elements.
#' @param .output_format unused.
#'
#' @return a tibble
#'
#' @name results
#' @aliases results,HCABrowser-method
#' @docType methods
#'
#' @export
#' @importFrom dplyr distinct
setMethod('results', 'HCABrowser', .results)

setMethod('first_hit', 'SearchResult', .first_hit)
setMethod('last_hit', 'SearchResult', .last_hit)
setMethod('total_hits', 'SearchResult', .total_hits)
setMethod('es_query', 'SearchResult', .es_query)

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

setGeneric('pullBundles', function(hca, ...) standardGeneric('pullBundles'))
setGeneric('pullFiles', function(hca, ...) standardGeneric('pullFiles'))
setGeneric('showBundles', function(hca, bundle_fqids, ...) standardGeneric('showBundles'))
setGeneric('downloadHCA', function(hca, ...) standardGeneric('downloadHCA'))
setGeneric('activate', function(hca, ...) standardGeneric('activate'))
setGeneric('getProjects', function(hca, ...) standardGeneric('getProjects'))
setGeneric('showProject', function(hca, ...) standardGeneric('showProject'))
setGeneric('pullProject', function(hca, ...) standardGeneric('pullProject'))

.project_selections <-
    c('project_json.project_core.project_title',
      'project_json.project_core.project_short_name',
      'specimen_from_organism_json.organ.text',
      'library_preparation_protocol.library_construction_approach.text',
      'specimen_from_organism_json.genus_species.text',
      'disease.text'
    )

.getProjects <-
    function(hca)
{
    projects <- values(hca, 'project_json.project_core.project_title')
    projects <- as.character(projects$value)
    res <- lapply(projects, function(x) {
        hca_projects <- hca %>%
            filter("project_title" == x) %>%
            select(.project_selections)
        hca_res <- results(hca_projects)[1,]
        hca_res[,!grepl("[1-9]$", colnames(hca_res))]
    })
    empties <- lengths(res) == 0
    res[empties] <- NULL
    do.call(rbind.fill, res)
}

setMethod('getProjects', 'HCABrowser', .getProjects)

.showProject <-
    function(hca, project)
{
    
}

setMethod('showProject', 'HCABrowser', .showProject)

.pullProject <-
    function(hca, project, n)
{
    hca %>% filter("project_title" == project) %>% pullBundles(n)
}

setMethod('pullProject', 'HCABrowser', .pullProject)

.activate.HCABrowser <-
    function(hca, what=c('bundles', 'files'))
{
    type <- match.arg(what)
    if(what == 'bundles')
        hca@activated <- 'bundles'
    else if(what == 'files')
        hca@activated <- 'files'
    hca
}

#' Activate files or bundles of HCABrowser Object
#'
#' @param hca An HCABrowser object
#' @param what Either "bundles" or "files". Deterimines whether bundles or files
#'  should be shown.
#'
#' @return An HCABrowser with the selected activation
#'
#' @name activate
#' @aliases activate,HCABrowser-method
#' @docType methods
#'
#' @importFrom tidygraph activate
#' @export
setMethod('activate', 'HCABrowser', .activate.HCABrowser)

.set_per_page <-
    function(hca, n)
{
    if (n > 10)
        message('The HCABrowser is unable to show bundle results of more than 10 per_page')
    hca@per_page <- n
    select(hca, c())
}

#' Set per_page argument of HCABrowser object
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

.pullBundles <-
    function(hca, n = hca@per_page)
{
    hca %>% results(n = n, .output_format='summary') %>% pull('bundle_fqid') %>% as.character()
}

#' Obtain bundle fqids from a HCABrowser object
#'
#' @param hca A HCABrowser object
#' @param n integer(1) number of bundle fqids to pull
#'
#' @return character(1) of bundle fqids
#'
#' @name pullBundles
#' @aliases pullBundles,HCABrowser-method
#' @docType methods
#'
#' @examples
#' hca <- HCABrowser()
#' hca <- hca %>% pullBundles
#' @export
setMethod('pullBundles', 'HCABrowser', .pullBundles)

.pullFiles <-
    function(hca, n = 10)
{
    #hca <- hca %>% per_page(500)
    hca <- select(hca, c(), .output_format='summary')
    hca <- hca %>% activate('files')
    res <- hca %>% results(n = n)
    res %>% pull('manifest.files.uuid') %>% as.character()
}

#' Obtain file uuids from a HCABrowser object
#'
#' @param hca A HCABrowser object
#' @param n integer(1) number of files to pull
#'
#' @return character(1) of 
#'
#' @name pullFiles
#' @aliases pullFiles,HCABrowser-method
#' @docType methods
#'
#' @examples
#' hca <- HCABrowser()
#' #hca <- hca %>% pullFiles
#' hca
#' @export
setMethod('pullFiles', 'HCABrowser', .pullFiles)

.showBundles <- function(hca, bundle_fqids)
{
    bundle_fqids <- vapply(strsplit(bundle_fqids, '[.]'), function(x) { x[1] }, character(1))
    hca %>% filter("uuid" %in% bundle_fqids)
    #hca %>% downloadHCA() %>% filter(bundle_fqid %in% bundle_fqids)
}

#' Obtain all bundles from an hca object using there bundle fqids
#'
#' @param hca a HCABrowser object to search for bundles on.
#' @param bundle_fqids a character()
#'
#' @return A HCABrowser object displaying the selected bundles
#'
#' @name showBundles
#' @aliases showBundles,HCABrowser-method
#' @docType methods
#'
#' @examples
#' hca <- HCABrowser()
#' hca_bundles <- hca %>% pullBundles
#' hca2 <- hca %>% showBundles(hca_bundles)
#' hca2
#' @export
setMethod('showBundles', 'HCABrowser', .showBundles)

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

#' Show Search Result
#'
#' @param object a SearchResult object to show
#'
#' @return outputs a text represntation of the object
#'
#' @importFrom methods show
#' @export
setMethod('show', 'SearchResult', .show_SearchResult)

.show_HCABrowser <- function(object)
{
    cat('class:', class(object), '\n')
    cat('Using hca-dcp at:\n ', object@url, '\n\n')
    cat('Current Query:\n')
    for(i in object@es_query)
        cat(' ', deparse(rlang::quo_get_expr(i), width.cutoff = 500), '\n')
    cat('\n')
    cat('Current Selection:\n')
    for(i in object@es_source)
        cat(
            paste(strwrap(paste0('"', unname(rlang::eval_tidy(i)), '"', collapse = ', '), indent = 2, exdent = 2), collapse = "\n"),
        "\n")
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

#' Show HCABrowser object
#'
#' @param object An HCAbrowser object to show
#'
#' @return outputs a text represntation of the object
#'
#' @export
setMethod('show', 'HCABrowser', .show_HCABrowser)

