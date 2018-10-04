.filters <- list(
    organ = 'specimen_from_organism_json.organ.text',
    organ_part = 'files.biomaterial_json.biomaterials.content.organ_part.text',
    genus_species = 'files.biomaterial_json.biomaterials.content.genus_species', # NOT WORKING
    organism_age = 'files.biomaterial_json.biomaterials',
    instrument_manufacturer_model = 'files.protocol_json.protocols.content.instrument_manufacturer_model.text',
    storage_method = 1,
    library_construction_approach = 'files.protocol_json.protocols.content.library_construction_approach.text',
    organism_age_unit = 'files.biomaterial_json.biomaterials',
    biological_sex = 'files.biomaterial_json.biomaterials',
    disease = 'files.biomaterial_json.biomaterials',
    versions = 'manifest.version',
    laboratory = 'files.project_json.projects.content.contact.laboratory',
    protocol = 1,
    file_format = 'files.file_json.files.content.file_core.file_format',
    input_aggregate_cell_count = 1
)

.range_ops = list(
    '<' = "lt",
    '<=' = "lte",
    '>' = 'gt',
    '>=' = 'gte'
)

.match_ops = list(
    '==' = '='
)

.parse_EsQuery <- function(es_query)
{
    if (is.null(es_query))
        return(list(query=NULL))
    
}

.supportedFilters <- function()
{
    names(.filters)
}

#' @export
setMethod("supportedFilters", "missing", .supportedFilters)

#' @importFrom dplyr filter
#' @importFrom rlang quo lang_head lang_tail
#' @export
filter.HumanCellAtlas <- function(hca, ...)
{
    .dots <- quo(...)
    
    q_head <- as.character(lang_head(.dots))
    q_tail_1 <- as.character(lang_tail(.dots)[[1]])
    q_tail_2 <- as.character(lang_tail(.dots)[[2]])

    range <- NULL
    if(q_head %in% names(.range_ops))
        range <- .range_ops[[q_head]]

    filter <- hca@es_query@query@bool@filter
    must_not <- hca@es_query@query@bool@must_not

#    q_tail_1 <- .filters[[q_tail_1]]

    if(is.null(range)) {
        entries <- filter@entries
        term <- .Term(field=q_tail_1, operator=q_head, value=q_tail_2)
#        a <- list(q_tail_2)
#        names(a) <- q_tail_1
#        a <- list(match = a)
        entries <- c(entries, term)
        hca@es_query@query@bool@filter@entries <- entries
    }
    else {
        entries <- filter@entries
        range <- .Range(field=q_tail_1, operator=q_head, value=q_tail_2)
#        a <- list(q_tail_2)
#        names(a) <- range
#        a <- list(a)
#        names(a) <- q_tail_1
        entries <- c(entries, range)
        #hca@es_query$query$bool$must$range <-
        hca@es_query@query@bool@filter@entries <- entries
    }

    hca <- select(hca, q_tail_1)

    postSearch(hca, 'aws', 'raw', per_page=10)

    #hca
}

#' @importFrom dplyr select
#' @export
select.HumanCellAtlas <- function(hca, sources)
{
    #sources <- list(...)

    sources <- as.character(sources)
    hca@es_query@es_source@entries <- c(hca@es_query@es_source@entries, sources)
    #query[['_source']] <- c(query[['_source']], list(...))
    #query
    #hca
    postSearch(hca, 'aws', 'raw', per_page=10)
}

.build_es_query <- function()
{
    
}


