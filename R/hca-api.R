## https://dss.integration.data.humancellatlas.org/

#' @importFrom BiocFileCache BiocFileCache bfcnew bfcrpath bfccache
.retrieve_BiocFileCache_dbpath <- function(url)
{
    if (is.null(dbpath))
        dbpath <- BiocFileCache()
    if (methods::is(dbpath, "BiocFileCache")) {
        nrec <- NROW(bfcquery(dbpath, url, "rname", exact = TRUE))
        if (nrec == 0L)
            dbpath <- bfcnew(dbpath, url)
        else if (nrec == 1L)
            dbpath <- bfcrpath(dbpath, url)
        else
            stop(
                "\n  'bfc' contains duplicate record names",
                    "\n      url: ", url,
                    "\n      bfccache(): ", bfccache(dbpath),
                    "\n      rname: ", bfccache(dbpath)$rname
            )
    }
}

#' @importFrom readr read_tsv
.save_as_BiocFileCache <- function(dbpath, url)
{
    fname <- BiocFileCache::bfcrpath(rnames = url)
    readr::read_tsv(fname)
}

#' Parse results from a search query to a Search Results object
#'
#' @param res the results from an HCA query.
#'
#' @return a SearchResults object. 
#'
#' @importFrom httr content
#' @export
parseToSearchResults <-
    function(res)
{
    res <- httr::content(res)
    len <- as.integer(length(res[['results']]))
    first_hit <- 1L
    res <- SearchResult(es_query=res$es_query, results=res$results,
                        first_hit = first_hit, last_hit = len,
                        total_hits=res$total_hits)
    res
}

