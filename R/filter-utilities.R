.filters_list <- list(
    organ = 'files.specimen_from_organism_json.organ.text',
    organ_part = 'files.specimen_from_organism_json.organ_part.text',
    genus_species = c('files.cell_line_json.genus_species.text',
                      'files.cell_suspension_json.genus_species.text', 
                      'files.donor_organism_json.genus_species.text', 
                      'files.organoid_json.genus_species.text', 
                      'files.specimen_from_organism_json.genus_species.text'),
    organism_age = 'files.donor_organism_json.organism_age_unit.txt',
    instrument_manufacturer_model = 'files.sequencing_protocol_json.instrument_manufacturer_model.text',
    storage_method = 1,
    paired_end = 'files.sequencing_protocol_json.paired_end',
    ncbi_taxon_id = 'files.donor_organism_json.biomaterial_core.ncbi_taxon_id',
    process_type = 'files.analysis_process_json.process_type.text',
    library_construction_approach.ontology = 'files.library_preparation_protocol_json.library_construction_approach.ontology',
    library_construction_approach.text = 'files.library_preparation_protocol_json.library_construction_approach.text',
    organism_age_unit = 'files.donor_organism_json.organism_age_unit.text',
    project_title = 'files.project_json.project_core.project_title',
    project_shortname = 'files.project_json.project_core.project_shortname',
    biological_sex = 'files.donor_organism_json.biological_sex',
    disease = 'files.specimen_from_organism.disease.text',
    diseases = c('files.specimen_from_organism.diseases.text',
                'files.donor_organism_json.diseases.text'),
    versions = 'manifest.version',
    laboratory = 'files.project_json.contributors.laboratory',
    protocol = 1, # not clear if right
    file_format = c('files.analysis_file_json.file_core.file_format',
                    'files.image_file_json.file_core.file_format',
                    'files.reference_file_json.file_core.file_format',
                    'files.sequence_file_json.file_core.file_format',
                    'files.supplementary_file_json.file_core.file_format'),
    input_aggregate_cell_count = 1,
    file_names = 'manifest.files.name',
    file_uuids = 'manifest.files.uuid',
    file_content_type = 'manifest.files.content.type',
    file_sizes = 'manifest.files.size'
)

.manifest_fields <- c(
    'content_type',
    'crc32c',
    'indexed',
    'name',
    's3-etag',
    'sha1',
    'sha256',
    'size',
    'uuid',
    'version'
)

.filters <- as.character(.filters_list)
.filters_unlist <- unlist(.filters_list)
names(.filters) <- names(.filters_list)

.range_ops = list(
    '<' = "lt",
    '<=' = "lte",
    '>' = 'gt',
    '>=' = 'gte'
)

.regexp_ops = c('contains', 'startsWith', 'endsWith')

.range <- c('<', '<=', '>', '>=')

.match_ops = list(
    '==' = '='
)

.supportedFields <- function(hca)
{
    hca@supported_fields
}

#' @export
setMethod("supportedFields", "HumanCellAtlas", .supportedFields)

.availableFields <- function(hca, fields=c())
{
    fields_json <- jsonlite::fromJSON(hca@fields_path)
    fields <- .convert_names_to_filters(hca, fields)
    if (length(fields) > 0)
        fields_json <- fields_json[fields]
    value <- unlist(fields_json, use.names=FALSE)
    field <- rep(names(fields_json), lengths(fields_json))
    fields <- data.frame(field, value)
    as_tibble(fields)
}

#' @export
setMethod("availableFields", "HumanCellAtlas", .availableFields)

.is_bool_connector <- function(x)
{
    names <- names(x)
    names %in% c("filter", "should", "must_not") 
}

.binary_op <- function(sep)
{
    force(sep)
    function(e1, e2) {
        field <- as.character(substitute(e1))

        value <- try({
            e2
        }, silent = TRUE)
        if (inherits(value, "try-error")) {
            value <- as.character(substitute(e2))
            if(value[1] == 'c')
                value <- value[-1]
            value
        }

        fun <- "term"

        if(length(value) > 1)
            fun <- "terms"

        if(sep %in% .range)
            fun <- "range"

        if(sep %in% .regexp_ops) {
            fun <- 'regexp'
            ## TODO parse regex string to catch protected characters
            if(sep == 'contains')
                value <- paste0('.*', value, '.*')
            if(sep == 'startsWith')
                value <- paste0(value, '.*')
            if(sep == 'endsWith')
                value <- paste0('.*', value)
        }

        field <- .convert_names_to_filters(NULL, field)

        leaf <- list(value)
        if(fun == 'range') {
            names(leaf) <- .range[sep]
            leaf <- list(leaf)
        }
        names(leaf) <- field
        leaf <- list(leaf)
        names(leaf) <- fun

        if(sep == "!=")
            leaf <- list(must_not = leaf)

        leaf
    }
}

.not_op <- function(sep)
{
    force(sep)
    function(e1) {
        list(must_not = list(e1))
    }
}

.parenthesis_op <- function(sep)
{
    force(sep)
    function(e1) {
        if(.is_bool_connector(e1))
            list(bool = list(filter = list(bool = e1)))
        else
            list(bool = list(filter = list(e1)))
    }
}

.combine_op <- function(sep)
{
    force(sep)
    function(e1, e2) {
        fun <- "should"
        if (sep == '&')
            fun <- "filter"

        if(.is_bool_connector(e1))
            e1 <- list(bool = e1)
        if(.is_bool_connector(e2))
            e2 <- list(bool = e2)

        con <- list(list(e1, e2))
        names(con) <- fun
        con
    }
}

.get_selections <- function(x, ret_next = FALSE)
{
    if (ret_next)
        return(names(x))
    if(!is.null(names(x)) && names(x) %in% c("term", "terms", "range", "regexp"))
        lapply(x, .get_selections, TRUE)
    else
        lapply(x, .get_selections, FALSE)
}

#' importFrom rlang eval_tidy f_rhs f_env
.hca_filter_loop <- function(li, expr)
{
    res <- rlang::eval_tidy(expr, data= .LOG_OP_REG)
    if(.is_bool_connector(res))
        list(filter = list(c(li, list(bool = res))))
    else
        list(filter = list(c(li, res)))
}

#' @importFrom dplyr filter
#' @importFrom rlang quo_get_expr quos
#' @export
filter.HumanCellAtlas <- function(hca, ...)
{
    dots <- quos(...)
    es_query <- c(hca@es_query, dots)

    res <- Reduce(.hca_filter_loop, dots, init =  list())

    selected <- unlist(.get_selections(res))

    bool <- list(es_query = list(query = list(bool = res)))
    hca@es_query <- es_query
    hca@search_term <- bool
    
    select(hca, selected)
}

#' @importFrom dplyr select
#' @export
select.HumanCellAtlas <- function(hca, ..., .search = TRUE)
{
    sources <- quos(...)
    sources <- c(hca@es_source, sources)
    hca@es_source <- sources
    sources <- unlist(lapply(sources, rlang::eval_tidy))
    sources <- lapply(sources, as.character)
    sources <- unlist(sources)
    if (length(sources) && sources[1] == 'c')
        sources <- sources[-1]
    sources <- unique(sources)

    sources <- .convert_names_to_filters(hca, sources)

    search_term <- hca@search_term
    if(length(search_term) == 0)
        search_term <- list(es_query = list(query = NULL))
    search_term$es_query$"_source" <- sources
    hca@search_term <- search_term

    if (.search)
        postSearch(hca, 'aws', 'raw', per_page = hca@per_page)
    else
        hca
}

.convert_names_to_filters <- function(hca, sources)
{
    if(is.null(hca))
        fields <- .get_supportedFields(NULL)
    else
        fields <- supportedFields(hca)
    fields <- data.frame(fields)[,2]

    sources <- vapply(sources, function(x) {
        name <- fields[grepl(paste0('[.]', x, '$'), fields)]
        if (length(name) > 1) {
            txt <- vapply(name, function(y) {
                paste0(y, '\n')
            }, character(1))
            mes <- paste0('Field "', x, '" matched more than one field. Please select one:\n')  
            txt <- c(mes, txt)
            stop(txt)
        }
        if (length(name) == 0) {
            if (x %in% fields)
                name <- x
            else {
                message(paste0('Field "', x, '" may not be supported.'))
                name <- x
            }
        }
        name
            
    }, character(1))    
    names(sources) <- NULL
    sources
}

.LOG_OP_REG <- list()
## Assign conditions.
.LOG_OP_REG$`==` <- .binary_op("==")
.LOG_OP_REG$`%in%` <- .binary_op("==")
.LOG_OP_REG$`!=` <- .binary_op("!=")
.LOG_OP_REG$`>` <- .binary_op(">")
.LOG_OP_REG$`<` <- .binary_op("<")
.LOG_OP_REG$`>=` <- .binary_op(">=")
.LOG_OP_REG$`<=` <- .binary_op("<=")
## Custom binary operators 
.LOG_OP_REG$`%startsWith%` <- .binary_op("startsWith")
.LOG_OP_REG$`%endsWith%` <- .binary_op("endsWith")
.LOG_OP_REG$`%contains%` <- .binary_op("contains")
## not conditional.
.LOG_OP_REG$`!` <- .not_op("!")
## parenthesis
.LOG_OP_REG$`(` <- .parenthesis_op("(")
## combine filters
.LOG_OP_REG$`&` <- .combine_op("&")
.LOG_OP_REG$`|` <- .combine_op("|")

`%startsWith%` <- function(e1, e2){}
`%endsWith%` <- function(e1, e2){}
`%contains%` <- function(e1, e2){}

