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

.manifest_fields <- paste0('manifest.files.', .manifest_fields)

.initial_source <- c(
    "project_title", "project_short_name", "organ.text"#,
#    "library_construction_approach.text",
#    "specimen_from_organism_json.genus_species.text",
#    "disease.text"#, .manifest_fields
)

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

.fields <- function(hca)
{
    hca@supported_fields
}

#' List supported fields of an HCABrowser object
#'
#' @param hca An HCABrowser object.
#'
#' @return A tibble indicating fields that can be queried upon.
#'
#' @name fields
#' @aliases fields,HCABrowser-method
#' @docType methods
#'
#' @examples
#' hca <- ProjectBrowser()
#' hca %>% fields
#'
#' @export
setMethod("fields", "HCABrowser", .fields)

.project_fields <- function(hca)
{
    names(hca@terms)
}

#' List supported fields of an ProjectBrowser object
#'
#' @return A tibble indicating fields that can be queried upon.
#'
#' @name fields
#' @aliases fields,ProjectBrowser-method
#' @docType methods
#'
#' @examples
#' hca <- ProjectBrowser()
#' hca %>% fields
#'
#' @export
setMethod("fields", "ProjectBrowser", .project_fields)

.values <- function(x, fields=c(), ...)
{
    hca <- x
    fields_json <- jsonlite::fromJSON(hca@fields_path)
    fields <- .convert_names_to_filters(hca, fields)
    if (length(fields) > 0)
        fields_json <- fields_json[fields]
    value <- unlist(fields_json, use.names=FALSE)
    field_names <- rep(names(fields_json), lengths(fields_json))
    fields <- data.frame(field_names, value)
    as_tibble(fields)
}

#' List all values for certain fields
#'
#' @param x An HCABrowser Object.
#' @param fields a character vector of fields to display avaiable values for.
#' @param ... Other arguments.
#'
#' @return a list of possible values for a filter
#'
#' @examples
#' hca <- HCABrowser()
#' vals <- hca %>% values
#' vals
#' vals2 <- hca %>% values('organ.text')
#' vals2
#'
#' @importFrom S4Vectors values
#' @export
setMethod("values", "HCABrowser", .values)

.project_values <- function(x, fields)
{
    term <- x@terms
    field <- term[[fields]]
    field <- unlist(field)
    field <- head(field, -3)
    uu <- matrix(field, nrow = 2)
    uu <- t(uu)
    uu <- as.data.frame(uu)
    names(uu) <- c('value', 'hits')
    as_tibble(uu)
}

#' List all values for certain fields in a ProjectBrowser Object
#'
#' @param x A ProjectBrowser Object.
#' @param fields a character vector of fields to display avaiable values for.
#' @param ... Other arguments.
#'
#' @return a list of possible values for a filter
#'
#' @examples
#' hca <- ProjectBrowser()
#' vals <- hca %>% values('organ')
#' vals
#'
#' @importFrom S4Vectors values
#' @export
setMethod("values", "ProjectBrowser", .project_values)

.binary_op_project <- function(sep)
{
    force(sep)
    function(e1, e2) {
        field <- as.character(substitute(e1))
        value <- as.character(substitute(e2))
        
        fun <- "is"

        leaf <- list(value)

        names(leaf) <- fun
        leaf <- list(leaf)
        names(leaf) <- field
        leaf
    }
}

.combine_op_project <- function(sep)
{
    force(sep)
    function(e1, e2) {
        
        con <- list(e1, e2)
        con
    }
}

.is_bool_connector <- function(x)
{
    if (length(x) == 0)
        return(FALSE)
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
            list(bool = list(filter = list(list(bool = e1))))
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

#' @importFrom rlang eval_tidy f_rhs f_env
.hca_filter_loop <- function(li, expr)
{
    res <- rlang::eval_tidy(expr, data = .LOG_OP_REG)
    if(length(li) == 0) {
        if(.is_bool_connector(res))
            list(filter=list(list(bool = res)))
        else
            list(filter=list(res))
    }
    else {
        if (.is_bool_connector(li) & .is_bool_connector(res))
            list(filter = list(c(list(bool = li)), list(bool = res)))
        else if(.is_bool_connector(li))
            list(filter = list(c(list(bool = li)), res))
        else if(.is_bool_connector(res))
            list(filter = list(c(li, list(bool = res))))
        else
            list(filter = list(c(li, res)))
    }
}

.temp <- function(dots)
{
    res <- Reduce(.hca_filter_loop, dots, init =  list())
    list(es_query = list(query = list(bool = res)))
}

#' Filter HCABrowser objects
#'
#' @param .data an HCABrowser object to perform a query on.
#' @param .preserve unused.
#' @param ... further argument to be tranlated into a query to select from.
#'  These arguments can be passed in two ways, either as a single expression or
#'  as a series of expressions that are to be seperated by commas.
#'
#' @return a HCABrowser object containing the resulting query.
#'
#' @examples
#' hca <- HCABrowser()
#' hca2 <- hca %>% filter(organ.text == "brain")
#' hca2
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang quo_get_expr quos
filter.HCABrowser <- function(.data, ..., .preserve)
{
    hca <- .data
    dots <- quos(...)
    es_query <- c(hca@es_query, dots)
    search_term <- .temp(es_query)
    hca@search_term <- search_term
    hca@es_query <- es_query
    
    selected <- unlist(.get_selections(search_term))
    select(hca, selected)
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

.project_filter_loop <- function(li, expr)
{
    res <- rlang::eval_tidy(expr, data = .LOG_OP_REG_PROJECT)
    res
}

.project_temp <- function(dots)
{
    res <- Reduce(.project_filter_loop, dots, init = list())
    res
}

#' @importFrom curl curl_escape
#' @export
filter.ProjectBrowser <- function(.data, ..., .preserve)
{
    dots = quos(...)
    if (length(dots) == 0) {
        project <- .data
        ret <- paste0('filter=', curl::curl_escape('{}'))
        project@current_filter <- ret
        projectGet(project, ret)
    }
    else {
        project <- .data
        es_query <- c(project@es_query, dots)
        search_term <- Reduce(.project_filter_loop, es_query, init = list())
        project@es_query <- es_query
        project@search_term <- search_term
        ret <- paste0('filters=', curl::curl_escape(jsonlite::toJSON(search_term)))
        project@current_filter <- ret
        projectGet(project, ret)
    }
}

#' Select fields from a HCABrowser object
#'
#' @param .data an HCABrowser object to perform a selection on
#' @param ... further argument to be tranlated into an expression to select from.
#'  These arguments can be passed in two ways, either as a character vector or
#'  as a series of expressions that are the fields that are to be selected
#'  seperated by commas.
#' @param .output_format unused.
#'
#' @return a HCABrowser object containing the results of the selection.
#'
#' @examples
#' hca <- HCABrowser()
#' hca2 <- hca %>% select('paired_end')
#' hca2
#'
#' hca3 <- hca %>% select(c('organ.text', 'paired_end'))
#' hca3
#' @export
#' @importFrom dplyr select
#' @importFrom rlang quo_get_expr
select.HCABrowser <- function(.data, ..., .output_format = c('raw', 'summary'))
{
    hca <- .data
    sources <- quos(...)
    output_format <- match.arg(.output_format)
    sources <- c(hca@es_source, sources)
    hca@es_source <- sources
    sources <- lapply(sources, function(x) {
        val <- try ({
            rlang::eval_tidy(x)
        }, silent = TRUE)
        if (inherits(val, "try-error")) {
            val <- as.character(rlang::quo_get_expr(x))
        }
        val
    })
    sources <- unlist(sources)
    if (length(sources) && sources[1] == 'c')
        sources <- sources[-1]

    sources <- .convert_names_to_filters(hca, sources)
    sources <- unique(sources)

    search_term <- hca@search_term
    if(length(search_term) == 0)
        search_term <- list(es_query = list(query = NULL))
    search_term$es_query$"_source" <- sources
    hca@search_term <- search_term

    postSearch(hca, 'aws', output_format = output_format, per_page = hca@per_page)
}

.convert_names_to_filters <- function(hca, sources)
{
    if(is.null(hca))
        fields <- .get_supportedFields(NULL)
    else
        fields <- fields(hca)
    fields <- data.frame(fields)[,2]

    sources <- vapply(sources, function(x) {
        if (x == 'uuid')
            return(x)
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
                #message(paste0('Field "', x, '" may not be supported.'))
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

.LOG_OP_REG_PROJECT <- list()
.LOG_OP_REG_PROJECT$`==` <- .binary_op_project("==")
.LOG_OP_REG_PROJECT$`%in%` <- .binary_op_project("==")
.LOG_OP_REG_PROJECT$`&` <- .combine_op_project("&")

`%startsWith%` <- function(e1, e2){}
`%endsWith%` <- function(e1, e2){}
`%contains%` <- function(e1, e2){}

