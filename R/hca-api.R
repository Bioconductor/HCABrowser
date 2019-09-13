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

#' @importFrom httr content
#' @export
parseToSearchResults <-
    function(res)
{
    res <- httr::content(res)
    res <- SearchResult(es_query=res$es_query, results=res$results,
           total_hits=res$total_hits)
    res
}

#' HCA API methods
#'
#' @aliases getBundlesCheckout deleteBundle getBundle putBundle
#'      postBundlesCheckout putCollection deleteCollection getCollection
#'      patchCollection getFile headFile putFile postSearch getSubscriptions
#'      putSubscription deleteSubscription getSubscription
#'
#' @description
#'
#' Methods to access the Human Cell Atlas's Data Coordination Platform (HCA DCP)
#' by means of the platform's REST API.
#'
#' @usage
#'
#' getBundlesCheckout(hca, ...)
#' deleteBundle(hca, ...)
#' getBundle(hca, ...)
#' putBundle(hca, ...)
#' postBundlesCheckout(hca, ...)
#' putCollection(hca, ...)
#' deleteCollection(hca, ...)
#' getCollection(hca, ...)
#' patchCollection(hca, ...)
#' getFile(hca, ...)
#' headFile(hca, ...)
#' putFile(hca, ...)
#' postSearch(hca, ...)
#' getSubscriptions(hca, ...)
#' putSubscription(hca, ...)
#' deleteSubscription(hca, ...)
#' getSubscription(hca, ...)
#' 
#' @param add_contents list. List of items to remove from the collection. Items
#'  must match exactly to be removed. Items not found in the collection are
#'  ignored. (ADD DESCRIPTION OF LIST OBJECT)
#'
#' @param attachments list. The set of bundle metadata items to be included in
#'  the payload of a notification request to a subscriptionendpoint. Each
#'  property in this object represents an attachment to the notification
#'  payload. Each attachment will be a child property of the "attachments"
#'  property of the payload. The name of such a child property can be chosen
#'  freely provided it does not start with an underscore. For example, if the
#'  subscription is ``` { "attachments": { "taxon": { "type": "jmespath",
#'  "expression": "files.biomaterial_j
#'  son.biomaterials[].content.biomaterial_core.ncbi_taxon_id[]" } } } ``` the
#'  corresponding notification payload will contain the following entry ```
#'  "attachments": { "taxon": [9606, 9606] } ``` If a general error occurs
#'  during the processing of attachments, the notification will be sent with
#'  `attachments` containing only the reserved `_errors` attachment containing a
#'  string describing the error. If an error occurs during the processing of a
#'  specific attachment, the notification will be sent with all
#'  successfully processed attachments and additionally
#'  the `_errors` attachment containing an object with one
#'  property for each failed attachment. For example, ```
#'  "attachments": { "taxon": [9606, 9606] "_errors" {
#'  "biomaterial": "Some error occurred" } } ``` The value
#'  of the `attachments` property must be less than or
#'  equal to 128 KiB in size when serialized to JSON and
#'  encoded as UTF-8. If it is not, the notification will
#'  be sent with "attachments": { "_errors": "Attachments
#'  too large (131073 bytes)" }
#'
#' @param callback_url character(1).
#'  The subscriber's URL. An HTTP request is made to the
#'  specified URL for every attempt to deliver a
#'  notification to the subscriber. If the HTTP response
#'  code is 2XX, the delivery attempt is considered
#'  successful and no more attemtpts will be made.
#'  Otherwise, more attempts will be made with an
#'  exponentially increasing delay between attempts, until
#'  an attempt is successful or the a maximum number of
#'  attempts is reached.
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
#' @param email character(1). An email address to send status updates to.
#'
#' @param encoding character(1). The MIME type describing the encoding of the
#'  request body.  Either "application/json" or "multipart/form-data".
#'
#' @param es_query list. Elasticsearch query. (ADD DESCRIPTION OF STRUCTURE)
#'
#' @param files list. (ADD DESCRIPTION ON STRUCTURE OF THIS ARGUMENT)
#'
#' @param form_fields list. A collection of static form fields to be supplied in
#'  the request body, alongside the actual notification payload.
#'
#' @param hca An HCABrowser object that is the subject of the request.
#'
#' @param hmac_key_id character(1). An optional key ID to use with
#'  "hmac_secret_key".
#'
#' @param hmac_secret_key character(1). The key for signing requests to the
#'  subscriber's URL. The signature will be constructed according to
#'  https://tools.ietf.org/html/draft-cavage-http-signatures and transmitted in
#'  the HTTP `Authorization` header.
#'
#' @param json character(1) of a json query to be executed.
#'
#' @param method The HTTP request method to use when delivering a notification
#'  to the subscriber.
#'
#' @param name character(1). A short name identifying the collection.
#'
#' @param output_format character(1). Specifies the output format. Either
#'  "summary" or "raw". The default format, "summary", is a list of UUIDs for
#'  bundles that match the query. Set this parameter to "raw" to get the
#'  verbatim JSON metadata for bundles that match the query.
#'
#' @param payload_form_field character(1). The name of the form field that will
#'  hold the notification payload when the request is made. If the default name
#'  of the payload field collides with that of a field in `form_fields`, this
#'  porperty can be used to rename the payload and avoid the collision. This
#'  property is ignored unless `encoding` is `multipart/form-data`.
#'
#' @param per_page numeric(1). Max number of results to return per page.
#'
#' @param presignedurls logical(1). Include presigned URLs in the response. This
#'   is mutually exclusive with the directurls parameter.
#'
#' @param reason character(1). User-friendly reason for the bundle or timestamp-
#'   specific bundle deletion.
#'
#' @param remove_contents list. List of items to remove from the collection.
#'  Items must match exactly to be removed. Items not found in the collection
#'  are ignored.
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
#' @param ... Other arguments
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

