filters <- list(
    organ = 1,
    organ_part = 1,
    genus_species = 1,
    organism_age = 1,
    instrument_manufacturer_model = 1,
    storage_method = 1,
    library_construction_approach = 1,
    organism_age_unit = 1,
    biological_sex = 1,
    disease = 1,
    versions = 1,
    laboratory = 1,
    protocol = 1,
    file_format = 'files.file_json.files.content.file_core.file_format',
    input_aggregate_cell_count = 1
)

#' @importFrom dplyr filter
filter.HumanCellAtlas <- function(query, ...)
{

}

#' @importFrom dplyr select
select. <- function(query=list(), ...)
{
    query[['_source']] <- c(query[['_source']], list(...))
    query
}


