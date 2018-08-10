## https://dss.data.humancellatlas.org/

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
        args[vapply(a, is.null, logical(1))] <- NULL
        args <- paste0(names(args), rep("=", length(args)), as.character(args))
        args <- paste0(args, collapse="&")
        url <- paste0(url, "?", args)
    }
    url
}

#' @importFrom httr content stop_for_status
#' @importFrom jsonlite fromJSON
.return_reponse <-
    function(reponse)
{
    stop_for_status(response)
    response <- content(response, as = "text")
    fromJSON(response, flatten=TRUE)
}

#' @importFrom httr DELETE
.hca_delete <-
    function(url, body)
{
    reponse <- httr:DELETE(url, body, encode="json")
    .return_reponse(response)
} 

#' @importFrom httr GET
.hca_get <-
    function(url)
{
    response <- httr::GET(url)
    .return_reponse(response)
}

#' @importFrom httr HEAD
.hca_head <-
    function(url)
{
    response <- httr::HEAD(url)
    .return_reponse(response)
}

#' @importFrom httr POST
.hca_post <-
    function(url, body)
{
    response <- httr::POST(url, body, encode="json")
    .return_reponse(response)
}

#' @importFrom httr PUT
.hca_put <-
    function(url, body)
{
    reponse <- httr::PUT(url, body, encode="json")
    .return_reponse(response)
}

.getBundlesCheckout <-
    function(checkout_job_id, replica=c('aws', 'gcp', 'azure'),
        url='https://dss.data.humancellatlas.org/')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica)
    url <- .build_url(url, .apis['getBundlesCheckout'], checkout_job_id, args)
    .hca_get(url)
}

.deleteBundle <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        reason = NULL, url='https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica, version=version)
    url <- .build_url(url, .apis['deleteBundle'], uuid, args)
    .hca_delete(url)
}

.getBundle <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        directurls=NULL, presignedurls=FALSE, token=NULL,
        url='https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica, version=version, directurls=directurls,
                  presignedurls=presignedurls, token=token)
    url <- .build_url(url, .apis['getBundle'], uuid, args)
    .hca_get(url)
}

.putBundle <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        creator_uid, files, url='https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica, version=version)
    body <- list(creator_uid=creator_uid, files=files)
    url <- .build_url(url, .apis['putBundle'], uuid, args)
    .hca_put(url, body)
#    headers <- add_headers(
#        accept = "application/json",
#        `Content-Type` = "application/json",
#        uuid = uuid,
#        replica = replica
#    )
}

.postBundlesCheckout <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), destination=NULL,
        email=NULL, url='https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica, version=version)
    body <- list(destination=destination, email=email)
    url <- .build_url(url, .apis['postBundlesCheckout'], uuid, args)
    .hca_post(url, body)
}

.putCollection <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version, contents,
        description, details, name,
        url='https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    body <- list(contents=contents, description=description, details=details,
                 name=name)
    url <- .build_url(url, .apis['putCollection'], uuid, NULL)
    .hca_put(url, body)
}

.deleteCollection <-
    function(uuid, replica=c('aws', 'gcp', 'azure'),
        url='https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica)
    url <- .build_url(url, .apis['deleteCollection'], uuid, args)
    .hca_delete(url)
}

.getCollection <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        url='https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica, version=version)
    url <- .build_url(url, .apis['getCollection'], uuid, args)
    .hca_get(url)
}

.patchCollection <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version, add_contents,
        description, details, name, remove-contents,
        url = 'https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica, version=version)
    body <- list(add_contents=add_contents, description=description,
                 details=details, name=name, remove_contents=remove_contents)
    url <- .build_url(url, .apis['patchCollection'], uuid, args)
    .hca_patch(url, body)
}

.getFile <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), token=NULL, version=NULL,
        url = 'https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica, version=version, token=token)
    url <- .build_url(url, .apis['getFile'], uuid, args)
    .hca_get(url)
}

.headFile <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        url = 'https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica, version=version)
    url <- .build_url(url, .apis['getFile'], uuid, args)
    .hca_head(url)
}

.putFile <-
    function(uuid, creator_uid, source_url, version=NULL,
        url = 'https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(version=version)
    body <- list(creator_uid=creator_uid, source_url=source_url)
    url <- .build_url(url, .apis['putFile'], uuid, args)
    .hca_put(url, body)
}

.getSubscriptions <-
    function(replica=c('aws', 'gcp', 'azure'),
        url = 'https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica)
    url <- .build_url(url, .apis['getSubscriptions'] args)
    .hca_get(url)
}

.putSubscription <-
    function(replica=c('aws', 'gcp', 'azure'), attachments, callback_url,
        encoding, es_query, form_fields, hmac_key_id, hmac_secret_key,
        method, payload_form_field,
        url = 'https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica)
    body <- list(attachments=attachments, callback_url=callback_url,
                 encoding=encoding, es_query=es_query, form_fields=form_fields,
                 hmac_key_id=hmac_key_id, hmac_secret_key=hmac_secret_key,
                 method=method, payload_form_field=payload_form_field)
    url <- .build_url(url, .apis['putSubscription'], NULL, args)
    .hca_put(url, body)
}

.deleteSubscription <-
    function(uuid, replica=c('aws', 'gcp', 'azure'),
        url = 'https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica)
    url <- .build_url(url, .apis['deleteSubscription'], uuid, args)
    .hca_delete(url)
}

.getSubscription <-
    function(uuid, replica=c('aws', 'gcp', 'azure'),
        url = 'https://dss.data.humancellatlas.org/v1')
{
    replica <- match.arg(replica)
    args <- list=(replica=replica)
    url <- .build_url(url, .apis['getSubscription'], uuid, args)
    .hca_get(url)
}

#' Check the status of a checkout request 
#'
#' @description Check the status of a checkout request 
#' @export
setMethod("getBundlesCheckout", "character", .getBundlesCheckout)

#' Delete a bundle or a specific bundle version
#'
#' @export
setMethod("deleteBundle", "character", .deleteBundle)

#' Retrieve a bundle given a UUID and optionally a version
#'
#' @export
setMethod("getBundle", "character", .getBundle)

#' Create a bundle
#'
#' @export
setMethod("putBundle", "character", .putBundle)

#' Check out a bundle to DSS-namaged or user-managed cloud object storage
#' destination
#'
#' @export
setMethod("postBundlesCheckout", "character", .postBundleCheckout)

#' Create a collection
#'
#' @export
setMethod("putCollection", "character", .putCollection)

#' Delete a collection
#'
#' @export
setMethod("deleteCollection", "character", .deleteCollection)

#' Retrieve a collection given a UUID
#'
#' @export
setMethod("getCollection", "character", .getCollection)

#' Update a collection
#'
#' @export
setMethod("patchCollection", "character", .patchCollection)

#' Retrieve a file given a UUID and optionally a version
#'
#' @export
setMethod("getFile", "character", .getFile)

#' Retrieve a file's metadata given an UUID and optionally a version
#'
#' @export
setMethod("headFile", "character", .headFile)

#' Create a new version of a file
#'
#' @export
setMethod("putFile", "character", .putFile)

#' Find bundles by searching their metadata with an Elasticsearch query
#'
#' @export
setMethod("postSearch", "character", .postSearch)

#' Retrieve a user's event Subscription
#'
#' @export
setMethod("getSubscriptions", "character", .getSubscriptions)

#' Creates an event subscription
#'
#' @export
setMethod("putSubscription", "character", .putSubscription)

#' Delete an event subscription
#'
#' @export
setMethod("deleteSubscription", "character", .deleteSubscription)

#' Retrieve an event subscription given a UUID
#'
#' @export
setMethod("getSubscription", "character", .getSubscription)

