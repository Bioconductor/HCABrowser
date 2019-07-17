## functions to build requests to HCA azul backend for ProjectBrowser class



.project_content <- function(res)
{
    con <- httr::content(res)
    .parse_project_get(con)
}

projectGet <- function(project, filter, per_page=15)
{
    browser()
    url <- project@url
    url <- paste0(url, '?', filter, '&size=', per_page, '&sort=projectTitle&order=asc')
    res <- httr::GET(url)
    project@project_results <- .project_content(res)
    project
}
