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

#' HCA API methods
#'
#' @aliases listBundles, getBundlesCheckout, getBundle, checkoutBundle,
#'          getFile, headFile, searchBundles
#'
#' @description
#'
#' Methods to access the Human Cell Atlas's Data Coordination Platform (HCA DCP)
#' by means of the platform's REST API.
#'
#' @usage
#'
#' listBundles(x, ...)
#' getBunldesCheckout(x, ...)
#' getBundle(x, ...)
#' checkoutBundle(x, ...)
#' getFile(x, ...)
#' headFile(x, ...)
#' searchBundles(x, ...) 
#'
#' @param checkout_job_id character(1). A RFC4122-complliant ID for the checkout
#'  job request.
#'
#' @param contents list. A list of objects describing links to files, bundles,
#'  other collections, and metadata fragments that are part of the collection.
#'
#' @param creator_uid character(1). User ID who is creating this bundle.
#'
#' @param description character(1). A long description of the collection,
#'  formatted in Markdown.
#'
#' @param destination character(1). User-owned destination storage bucket.
#'
#' @param details list. Supplementary JSON metadata for the collection.
#'  (ADD DESCRIPTION OF STRUCTURE)
#'
#' @param directurls logical(1). Include direct-access URLs in the response.
#'  This is mutually exclusive with the \code{presignedurls} parameter.  DEFAULT
#'  is \code{NULL}.
#'
#' @param es_query list. Elasticsearch query. (ADD DESCRIPTION OF STRUCTURE)
#'
#' @param x An HCABrowser object that is the subject of the request.
#'
#' @param json character(1) of a json query to be executed.
#'
#' @param output_format character(1). Specifies the output format. Either
#'  "summary" or "raw". The default format, "summary", is a list of UUIDs for
#'  bundles that match the query. Set this parameter to "raw" to get the
#'  verbatim JSON metadata for bundles that match the query.
#'
#' @param per_page numeric(1). Max number of results to return per page.
#'
#' @param presignedurls logical(1). Include presigned URLs in the response. This
#'   is mutually exclusive with the directurls parameter.
#'
#' @param replica character(1). A replica to fetch form. Can either be
#'  set to "aws", "gcp", or "azure".  DEFAULT is "aws".
#'
#' @param search_after character(1). **Search-After-Context**. An internal state
#'  pointer parameter for use with pagination. The API client should not need to
#'  set this parameter directly; it should instead directly fetch the URL given
#'  in the "Link" header.
#'
#' @param source_url character(1). Cloud URL for source data.
#'
#' @param token \code{Token}. Token to manage retries. End users constructing
#'   queries should not set this parameter. Use \code{get_token()} to generate.
#'
#' @param uuid character(1). A RFC4122-compliant ID for the bundle.
#'
#' @param version character(1). Timestamp of bundle creation in RFC3339.
#'
#' @return an HCABrowser object
#'
#' @examples
#' hca <- HCABrowser()
#' #addmore
#' 
#'
#' @name hca-api-methods
#' @author Daniel Van Twisk
NULL

.listBundles <- function(x, replica = c('aws', 'gcp'), prefix, token,
                         per_page = 100, search_after)
{
    
}

#' List all avaiable bundles
#'
#' @rdname hca-api-methods
#' @export
setMethod('listBundles', 'HCABrowser', .listBundles)

.getBundleCheckout <- function(x, replcia = c('aws', 'gcp'), checkout_job_id)
{
    replica <- match.arg(replica)
    x$dss.api.bundles.checkout.get(replica = replica, checkout_job_id = checkout_job_id)
}

#' Check the status of a bundle checkout request
#'
#' @rdname hca-api-methods
#' @export
setMethod('getBundleCheckout', 'HCABrowser', .getBundleCheckout)

.getBundle <- function(x, uuid, version, replica = c('aws', 'gcp'),
                       directurls, presignedurls, token, per_page = 500,
                       start_at)
{
    replica <- match.arg(replica)
    x$dss.api.bundles.checkout.post(uuid = uuid, version = version,
                                     replica = replica)
}

#' Retrieve an bundle given a UUID and and optionally a version
#'
#' @rdname hca-api-methods
#' @export
setMethod('getBundle', 'HCABrowser', .getBundle)

.checkoutBundle <- function(x, uuid, version, replica = c('aws', 'gcp'),
                            json_request_body)
{
    replica <- match.arg(replica)
    x$dss.api.bundles.checkout.post(uuid = uuid, version, replica = replica)
}

#' Check out a bundle to DSS-managed or user-managed cloud object storage destination
#'
#' @rdname hca-api-methods
#' @export
setMethod('checkoutBundle', 'HCABrowser', .checkoutBundle)

.getFile <- function(x, uuid, replica = c('aws', 'gcp'), version, token,
                     directurl, content_disposition)
{
    replica = match.arg(replica)
    x$Retrieve_a_file_given_a_UUID_and_optionally_a_version.(uuid = uuid, replica = replica)
}

#' Retrieve a file given a UUID and optionally a version
#'
#' @rdname hca-api-methods
#' @export
setMethod('getFile', 'HCABrowser', .getFile)

.headFile <- function(x, uuid, replica = c('aws', 'gcp'), version)
{
    replica = match.arg(replica)
    x$`Retrieve_a_file's_metadata_given_an_UUID_and_optionally_a_version.`(uuid = uuid, replica = replica)
}

#' Retrieve a file's metadata given an UUID and optionally a version
#'
#' @rdname hca-api-methods
#' @export
setMethod('headFile', 'HCABrowser', .headFile)

.searchBundles <- function(x, json_request_body,
                           output_format = c('summary', 'raw'),
                           replica = c('aws', 'gcp'), per_page = 100,
                           search_after)
{
    replica = match.arg(replica)
    output_format = match.arg(output_format)
    x$Find_bundles_by_searching_their_metadata_with_an_Elasticsearch_query(json_request_body = json_request_body, output_format = output_format, per_page = per_page)
}

#' Find bundles by searching their metadata with an Elasticsearch query 
#'
#' @rdname hca-api-methods
#' @export
setMethod('searchBundles', 'HCABrowser', .searchBundles)

