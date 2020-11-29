## https://dss.integration.data.humancellatlas.org/

#' @importFrom BiocFileCache BiocFileCache bfcnew bfcrpath bfccache bfcquery
#' @importFrom methods is
.retrieve_BiocFileCache_dbpath <- function(url)
{
    if (is.null(dbpath))
        dbpath <- BiocFileCache()
    if (is(dbpath, "BiocFileCache")) {
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

#' HCA API methods
#'
#' @aliases getBundleCheckout getBundle checkoutBundle getFile
#'     headFile
#'
#' @description
#'
#' Methods to access the Human Cell Atlas's Data Coordination Platform (HCA DCP)
#' by means of the platform's REST API.
#'
#' @usage
#'
#' getBundleCheckout(x, ...)
#' getBundle(x, ...)
#' checkoutBundle(x, ...)
#' getFile(x, ...)
#' headFile(x, ...)
#'
#' @param checkout_job_id character(1). A RFC4122-complliant ID for the checkout
#'  job request.
#'
#' @param directurls logical(1). Include direct-access URLs in the response.
#'  This is mutually exclusive with the \code{presignedurls} parameter.  DEFAULT
#'  is \code{NULL}.
#'
#' @param x An HCABrowser object that is the subject of the request.
#'
#' @param json_request_body character(1) of a json query to be executed.
#'
#' @param per_page numeric(1). Max number of results to return per page.
#'
#' @param presignedurls logical(1). Include presigned URLs in the response. This
#'   is mutually exclusive with the directurls parameter.
#'
#' @param replica character(1). A replica to fetch form. Can either be
#'  set to "aws", "gcp", or "azure".  DEFAULT is "aws".
#'
#' @param token \code{Token}. Token to manage retries. End users constructing
#'   queries should not set this parameter. Use \code{get_token()} to generate.
#'
#' @param uuid character(1). A RFC4122-compliant ID for the bundle.
#'
#' @param version character(1). Timestamp of bundle creation in RFC3339.
#'
#' @param ... Additional arguments.
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

.getBundleCheckout <- function(x, replica = c('aws', 'gcp'), checkout_job_id)
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
                       directurls, presignedurls, token, per_page = 500)
{
    replica <- match.arg(replica)
    if (missing(version))
        x$dss.api.bundles.checkout.post(uuid = uuid, replica = replica)
    else
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
    if (missing(version))
        x$dss.api.bundles.checkout.post(uuid = uuid, replica = replica)
    else
        x$dss.api.bundles.checkout.post(uuid = uuid, version = version, replica = replica)
}

#' Check out a bundle to DSS-managed or user-managed cloud object storage destination
#'
#' @rdname hca-api-methods
#' @export
setMethod('checkoutBundle', 'HCABrowser', .checkoutBundle)

.getFile <- function(x, uuid, replica = c('aws', 'gcp'), version, token)
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

