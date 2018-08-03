## https://dss.data.humancellatlas.org/

.apis <- c(
    ## get / post / head
    bundles_checkout = "/bundles/checkout/%s",
    bundles = "/bundles/%s",
    "..."
)

.hcaget <-
    function(url)
{
    headers = add_headers(
        accept = "application/json",
        `Content-Type` = "application/json"
    )
    response <- httr::POST(url, headers, body = body)
    stop_for_status(response)
    response
}

bundles_checkout <-
    function(uuid)
{
    url <- sprintf(.apis$bundles_checkout, uuid)
    response <- .hcaget(url)
    ## ...
}

#' @importFrom httr GET POST add_headers content stop_for_status
#' @importFrom jsonlite fromJSON
.getBundlesCheckout <-
    function(uuid, replica=c('aws', 'gcp', 'azure'),
        url = 'https://dss.data.humancellatlas.org/')
{

}

.getBundle <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        directurls=TRUE, presignedurls=TRUE, token=NULL,
        url = 'https://dss.data.humancellatlas.org/')
{
    replica <- match.arg(replica)
    headers <- add_headers(
        accept = "application/json",
        `Content-Type` = "application/json",
        uuid = uuid,
        replica = replica
    )
    url <- paste0(url, 'v1/bundles/', uuid, '?replica=', replica)
    response <- httr::GET(url, headers, body=body)
    stop_for_status(response)
    response <- content(response, as = "text")
    fromJSON(response, flatten=TRUE)
}

.putBundle <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), request=NULL, version=NULL,
         url = 'https://dss.data.humancellatlas.org/')
{

}

.putCollections <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), request=NULL, version=NULL,
        url = 'https://dss.data.humancellatlas.org/')
{

}

.getCollections <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        url = 'https://dss.data.humancellatlas.org/')
{

}

.getFiles <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), token=NULL, version=NULL,
        url = 'https://dss.data.humancellatlas.org/')
{

}

.putFiles <-
    function(uuid, request=NULL, version=NULL,
         url = 'https://dss.data.humancellatlas.org/')
{

}

.getSubscriptions <-
    function(replica=c('aws', 'gcp', 'azure'),
        url = 'https://dss.data.humancellatlas.org/')
{

}

.putSubscriptions <-
    function(replica=c('aws', 'gcp', 'azure'), request=NULL,
         url = 'https://dss.data.humancellatlas.org/')
{

}

.getSubscription <-
    function(uuid, replica=c('aws', 'gcp', 'azure'),
        url = 'https://dss.data.humancellatlas.org/')
{

}

#' 
setMethod("getBundlesCheckout", "character", .getBundlesCheckout)
setMethod("getBundle", "character", .getBundle)
setMethod("putBundle", "character", .putBundle)
setMethod("putCollections", "character", .putCollections)
setMethod("getCollections", "character", .getCollections)
setMethod("getFiles", "character", .getFiles)
setMethod("putFiles", "character", .putFiles)
setMethod("getSubscriptions", "character", .getSubscriptions)
setMethod("putSubscriptions", "character", .putSubscriptions)
setMethod("getSubscriptions", "character", .getSubscriptions)

