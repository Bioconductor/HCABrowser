filters <- list(
    organ = 'files.biomaterial_json.biomaterials.content.organ.text',
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

.supportedFilters <- function() {
    names(filters)
}

#' @export
setMethod("supportedFilters", "missing", .supportedFilters)

#' @importFrom dplyr filter
#' @export
filter.HumanCellAtlas <- function(query, ...)
{
    message("hi")

}

#' @importFrom dplyr select
#' @export
select.HumanCellAtlas <- function(query=list(), ...)
{
    query[['_source']] <- c(query[['_source']], list(...))
    query
}

.build_es_query <- function()
{
    
}


