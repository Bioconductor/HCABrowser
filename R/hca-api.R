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
