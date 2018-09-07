## https://dss.integration.data.humancellatlas.org/

.apis <- c(
    getBundlesCheckout = "/bundles/%s/checkout",
    deleteBundle = "/bundles/%s",
    getBundle = "/bundles/%s",
    putBundle = "/bundles/%s",
    postBundlesCheckout = "/bundles/%s/checkout",
    putCollection = "/collections",
    deleteCollection = "/collections/%s",
    getCollection = "/collections/%s",
    patchCollection = "/collections/%s",
    getFile = "/files/%s",
    headFile = "/files/%s",
    putFile = "/files/%s",
    postSearch = "/search",
    getSubscriptions = "/subscriptions",
    putSubscriptions = "/subscriptions",
    deleteSubscriptions = "/subscriptions/%s",
    getSubscription = "/subscriptions/%s"
)

.build_url <- function(url, tag, uuid=NULL, args=NULL){
    if(!is.null(uuid)) tag <- sprintf(tag, uuid)
    url <- paste0(url, tag)
    if(!is.null(args)) {
        args[vapply(args, is.null, logical(1))] <- NULL
        args <- paste0(names(args), rep("=", length(args)), as.character(args))
        args <- paste0(args, collapse="&")
        url <- paste0(url, "?", args)
    }
    url
}

#' @importFrom httr add_headers
.build_header <- function(include_token)
{
    header <- list(
        `Content-Type` = "application/json",
        `Accept` = "application/json"
    )
    if (include_token) {
        token <- get_token()
        header['access_token'] <- token$credentials[['access_token']]
        header['token_type'] <- token_type = token$credentials[['token_type']]
        header['expires_in'] <- expires_in = token$credentials[['expires_in']]
    }
    do.call(add_headers, header)
}

.retrieve_BiocFileCache_dbpath <- function(url)
{
    if (is.null(dbpath))
        dbpath <- BiocFileCache()
    if (is(dbpath, "BiocFileCache")) {
        nrec <- NROW(bfcquery(dbpath, url, "rname", exact = TRUE))
        if (nrec == 0L)
            dbpath <- bfcnew(dbpath, url)
        else if (nrec == 1L)
            dbpath <- bfrcpath(dbpath, url)
        else
            stop(
                "\n  'bfc' contains duplicate Organism.dplyr record names",
                    "\n      url: ", url,
                    "\n      bfccache(): ", bfccache(dbpath),
                    "\n      rname: ", txdb_name
            )
    }
}

.save_as_BiocFileCache <- function(dbpath, url)
{
    fname <- BiocFileCache::bfcrpath(rnames = url)
    readr::read_tsv(fname)
}

#' @importFrom httr content stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_tsv
.return_response <-
    function(response, expected_response=c('json', 'file'))
{
    expected_response <- match.arg(expected_response)
    stop_for_status(response)
    response <- content(response, as = "text")
    if (expected_response == 'json')
        fromJSON(response, flatten=FALSE)
    else if (expected_response == 'file')
        readr::read_tsv(text=response, sep="\t")
}

#' @importFrom httr DELETE
.hca_delete <-
    function(url, body)
{
    header <- .build_header(include_token=TRUE)
    response <- httr::DELETE(url, header, body=body, encode="json")
    .return_response(response)
} 

#' @importFrom httr GET
.hca_get <-
    function(url, include_token, expected_response=c("json", "file"))
{
    header <- .build_header(include_token)
    expected_response <- match.arg(expected_response)
    if(include_token) response <- httr::GET(url, header)
    else response <- httr::GET(url)   
    .return_response(response, expected_response)
}

#' @importFrom httr HEAD
.hca_head <-
    function(url)
{
    header <- .build_header(include_token=FALSE)
    response <- httr::HEAD(url, header)
    .return_response(response)
}

#' @importFrom httr PATCH
.hca_patch <-
    function(url, body)
{
    header <- .build_header(include_token=TRUE)
    response <- httr::PATCH(url, header, body=body, encode="json")
    .return_response(response)
}

#' @importFrom httr POST headers
#' @importFrom stringr str_remove
.hca_post <-
    function(url, body)
{
    header <- .build_header(include_token=FALSE)
    response <- httr::POST(url, header, body=body, encode="json")
    res <- list(.return_response(response))
    while(!is.null(link <- httr::headers(response)[['link']])) {
        link <- str_remove(link, "<")
        link <- str_remove(link, ">.*")
        response <- .hca_post_next(link, header, body, encode)
        res <- c(res, list(.return_response(response)))
    }
    res
}

.hca_post_next <-
    function(link, header, body, encode)
{
    httr::POST(link, header, body=body, encode="json")
}

#' @importFrom httr PUT
.hca_put <-
    function(url, body, include_token)
{
    header <- .build_header(include_token)
    response <- httr::PUT(hca@url, header, body, encode="json")
    .return_response(response)
}

.getBundlesCheckout <-
    function(hca, checkout_job_id, replica=c('aws', 'gcp', 'azure'))
{
    replica <- match.arg(replica)
    args <- list=(replica=replica)
    url <- .build_url(hca@url, .apis['getBundlesCheckout'], checkout_job_id, args)
    .hca_get(url, include_token=FALSE)
}

.deleteBundle <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        reason = NULL)
{
    replica <- match.arg(replica)
    args <- list(replica=replica, version=version)
    url <- .build_url(hca@url, .apis['deleteBundle'], uuid, args)
    .hca_delete(url)
}

.getBundle <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        directurls=NULL, presignedurls=FALSE, token=NULL)
{
    replica <- match.arg(replica)
    args <- list(replica=replica, version=version, directurls=directurls,
                  presignedurls=presignedurls, token=token)
    url <- .build_url(hca@url, .apis['getBundle'], uuid, args)
    .hca_get(url, include_token=FALSE)
}

.putBundle <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        creator_uid, files)
{
    replica <- match.arg(replica)
    args <- list(replica=replica, version=version)
    body <- list(creator_uid=creator_uid, files=files)
    url <- .build_url(hca@url, .apis['putBundle'], uuid, args)
    .hca_put(url, body, include_token=FALSE)
}

.postBundlesCheckout <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), destination=NULL,
        email=NULL)
{
    replica <- match.arg(replica)
    args <- list(replica=replica, version=version)
    body <- list(destination=destination, email=email)
    url <- .build_url(hca@url, .apis['postBundlesCheckout'], uuid, args)
    .hca_post(url, body)
}

.putCollection <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), version, contents,
        description, details, name)
{
    replica <- match.arg(replica)
    body <- list(contents=contents, description=description, details=details,
                 name=name)
    url <- .build_url(hca@url, .apis['putCollection'], uuid, NULL)
    .hca_put(url, body, include_token=TRUE)
}

.deleteCollection <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'))
{
    replica <- match.arg(replica)
    args <- list=(replica=replica)
    url <- .build_url(hca@url, .apis['deleteCollection'], uuid, args)
    .hca_delete(url)
}

.getCollection <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), version=NULL)
{
    replica <- match.arg(replica)
    args <- list(replica=replica, version=version)
    url <- .build_url(hca@url, .apis['getCollection'], uuid, args)
    .hca_get(url, include_token=TRUE)
}

.patchCollection <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), version, add_contents,
        description, details, name, remove_contents)
{
    replica <- match.arg(replica)
    args <- list(replica=replica, version=version)
    body <- list(add_contents=add_contents, description=description,
                 details=details, name=name, remove_contents=remove_contents)
    url <- .build_url(hca@url, .apis['patchCollection'], uuid, args)
    .hca_patch(url, body)
}

.getFile <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), token=NULL, version=NULL)
{
    replica <- match.arg(replica)
    args <- list(replica=replica, version=version, token=token)
    url <- .build_url(hca@url, .apis['getFile'], uuid, args)
    .hca_get(url, include_token=FALSE, expected_response="file")
}

.headFile <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'), version=NULL)
{
    replica <- match.arg(replica)
    args <- list(replica=replica, version=version)
    url <- .build_url(hca@url, .apis['getFile'], uuid, args)
    .hca_head(url)
}

.putFile <-
    function(hca, uuid, creator_uid, source_url, version=NULL)
{
    replica <- match.arg(replica)
    args <- list(version=version)
    body <- list(creator_uid=creator_uid, source_url=source_url)
    url <- .build_url(hca@url, .apis['putFile'], uuid, args)
    .hca_put(url, body, include_token=FALSE)
}

.postSearch <-
    function(hca, replica=c('aws', 'gcp', 'azure'),
        output_format=c('summary', 'raw'), es_query, per_page=100,
        search_after=NULL)
{
    replica <- match.arg(replica)
    output_format <- match.arg(output_format)
    args <- list(replica=replica, output_format=output_format,
                 per_page=per_page, search_after=search_after)
    body <- list(es_query=es_query)
    url <- .build_url(hca@url, .apis['postSearch'], NULL, args)
    .hca_post(url, body)
}

.getSubscriptions <-
    function(hca, replica=c('aws', 'gcp', 'azure'))
{
    replica <- match.arg(replica)
    args <- list(replica=replica)
    url <- .build_url(hca@url, .apis['getSubscriptions'], NULL, args)
    .hca_get(url, include_token=TRUE)
}

.putSubscription <-
    function(hca, replica=c('aws', 'gcp', 'azure'), attachments, callback_url,
        encoding, es_query, form_fields, hmac_key_id, hmac_secret_key,
        method, payload_form_field)
{
    replica <- match.arg(replica)
    args <- list(replica=replica)
    body <- list(attachments=attachments, callback_url=callback_url,
                 encoding=encoding, es_query=es_query, form_fields=form_fields,
                 hmac_key_id=hmac_key_id, hmac_secret_key=hmac_secret_key,
                 method=method, payload_form_field=payload_form_field)
    url <- .build_url(hca@url, .apis['putSubscription'], NULL, args)
    .hca_put(url, body, include_token=TRUE)
}

.deleteSubscription <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'))
{
    replica <- match.arg(replica)
    args <- list(replica=replica)
    url <- .build_url(hca@url, .apis['deleteSubscription'], uuid, args)
    .hca_delete(url)
}

.getSubscription <-
    function(hca, uuid, replica=c('aws', 'gcp', 'azure'))
{
    replica <- match.arg(replica)
    args <- list(replica=replica)
    url <- .build_url(hca@url, .apis['getSubscription'], uuid, args)
    .hca_get(url, include_token=TRUE)
}

#' @name HCA API methods
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
#' getBundlesCheckout(checkout_job_id, ...)
#' deleteBundle(uuid, ...)
#' getBundle(uuid, ...)
#' putBundle(uuid, ...)
#' postBundle(uuid, ...)
#' postBundlesCheckout(uuid, ...)
#' putCollection(uuid, ...)
#' deleteCollection(uuid, ...)
#' getCollection(uuid, ...)
#' patchCollection(uuid, ...)
#' getFile(uuid, ...)
#' headFile(uuid, ...)
#' putFile(uuid, ...)
#' postSearch(replica, ...)
#' getSubscriptions(replica, ...)
#' putSubscription(replica, ...)
#' deleteSubscription(replica, ...)
#' getSubscription(replica, ...)
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
#' @param hmac-key-id character(1). An optional key ID to use with
#'  "hmac_secret_key".
#'
#' @param hmac_secret_key character(1). The key for signing requests to the
#'  subscriber's URL. The signature will be constructed according to
#'  https://tools.ietf.org/html/draft-cavage-http-signatures and transmitted in
#'  the HTTP `Authorization` header.
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
#' @param remove-contents list. List of items to remove from the collection.
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
#' 
#'
#' Check the status of a checkout request 
#'
#' @description Check the status of a checkout request 
#'
#' @author Daniel Van Twisk
#'
#' @export
setMethod("getBundlesCheckout", "HumanCellAtlas", .getBundlesCheckout)

#' Delete a bundle or a specific bundle version
#'
#' @export
setMethod("deleteBundle", "HumanCellAtlas", .deleteBundle)

#' Retrieve a bundle given a UUID and optionally a version
#'
#' @export
setMethod("getBundle", "HumanCellAtlas", .getBundle)

#' Create a bundle
#'
#' @export
setMethod("putBundle", "HumanCellAtlas", .putBundle)

#' Check out a bundle to DSS-namaged or user-managed cloud object storage
#' destination
#'
#' @export
setMethod("postBundlesCheckout", "HumanCellAtlas", .postBundlesCheckout)

#' Create a collection
#'
#' @export
setMethod("putCollection", "HumanCellAtlas", .putCollection)

#' Delete a collection
#'
#' @export
setMethod("deleteCollection", "HumanCellAtlas", .deleteCollection)

#' Retrieve a collection given a UUID
#'
#' @export
setMethod("getCollection", "HumanCellAtlas", .getCollection)

#' Update a collection
#'
#' @export
setMethod("patchCollection", "HumanCellAtlas", .patchCollection)

#' Retrieve a file given a UUID and optionally a version
#'
#' @export
setMethod("getFile", "HumanCellAtlas", .getFile)

#' Retrieve a file's metadata given an UUID and optionally a version
#'
#' @export
setMethod("headFile", "HumanCellAtlas", .headFile)

#' Create a new version of a file
#'
#' @export
setMethod("putFile", "HumanCellAtlas", .putFile)

#' Find bundles by searching their metadata with an Elasticsearch query
#'
#' @export
setMethod("postSearch", "HumanCellAtlas", .postSearch)

#' Retrieve a user's event Subscription
#'
#' @export
setMethod("getSubscriptions", "HumanCellAtlas", .getSubscriptions)

#' Creates an event subscription
#'
#' @export
setMethod("putSubscription", "HumanCellAtlas", .putSubscription)

#' Delete an event subscription
#'
#' @export
setMethod("deleteSubscription", "HumanCellAtlas", .deleteSubscription)

#' Retrieve an event subscription given a UUID
#'
#' @export
setMethod("getSubscription", "HumanCellAtlas", .getSubscription)

