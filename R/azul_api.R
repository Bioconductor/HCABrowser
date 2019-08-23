## functions to build requests to HCA azul backend for ProjectBrowser class



.project_content <- function(project, res)
{
    con <- httr::content(res)
    .parse_project_get(project, con)
}

projectGet <- function(project, filter, per_page=15)
{
    url <- project@url
    project@per_page=per_page
    url <- paste0(url, '?', filter, '&size=', per_page, '&sort=projectTitle&order=asc')
    res <- httr::GET(url)
    project <- .project_content(project, res)
    project
}

.nextResults_ProjectBrowser <- function(result)
{
    url <- result@url
    url <- paste0(url, '?', result@current_filter, '&size=', result@per_page,
        '&sort=projectTitle&order=asc&search_after=', result@search_after,
        '&search_after_uid=', result@search_after_uid)
    res <- httr::GET(url)
    project <- .project_content(result, res)
    project
}

#' Next Results
#'
#' Fetch the next set of bundles from a ProjectBrowser Object
#'
#' @return A ProjectBrowser object that displays the next results
#'
#' @author Daniel Van Twisk
#'
#' @name nextResults
#' @aliases nextResults,ProjectBrowser-method
#' @docType methods
#' 
#' @examples
#'
#' hca <- ProjectBrowser()
#' hca <- nextResults(hca)
#' hca
#'
#' @export
setMethod("nextResults", "ProjectBrowser", .nextResults_ProjectBrowser)
