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
}   b

bundles_checkout <-
    function(uuid)
{
    url <- sprintf(.apis$bundles_checkout, uuid)
    response <- .hcaget(url)
    ## ...
}

.getBundleCheckout <-
    function(replica=c('aws', 'gcp', 'azure'), checkout_job_id)
{

}

.getBundle <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL,
        directurls=TRUE, presignedurls=TRUE, token=NULL)
{

}

.putBundle <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), request=NULL, version=NULL)
{

}

.putCollections <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), request=NULL, version=NULL)
{

}

.getCollections <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), version=NULL)
{

}

.getFiles <-
    function(uuid, replica=c('aws', 'gcp', 'azure'), token=NULL, version=NULL)
{

}

.putFiles <-
    function(uuid, request=NULL, version=NULL)
{

}

.getSubscriptions <-
    function(replica=c('aws', 'gcp', 'azure'))
{

}

.putSubscriptions <-
    function(replica=c('aws', 'gcp', 'azure'), request=NULL)
{

}

.getSubscription <-
    function(uuid, replica=c('aws', 'gcp', 'azure'))
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

