library(HCABrowser)

hca <- HCABrowser()

result <- hca$Find_bundles_by_searching_their_metadata_with_an_Elasticsearch_query(json_request_body=getEsQuery(hca), replica='aws')
result <- parseToSearchResults(result)

bundles <- lapply(results(result), function(results) {
    uuid <- stringr::str_split(results[["bundle_fqid"]], '\\.')[[1]][[1]]

    tryCatch({
    checkout_id <- httr::content(hca$dss.api.bundles.checkout.post(uuid=uuid, replica='aws'))$checkout_job_id

    bundle_checkout_status <- httr::content(hca$dss.api.bundles.checkout.get(checkout_job_id=checkout_id, replica='aws'))$status

    bundle <- hca$Retrieve_a_bundle_given_a_UUID_and_optionally_a_version.(uuid=uuid, replica='aws')
    httr::content(bundle)
    }, error = function(e) {
        message('Missing checkout request')
    }, finally = {
        NULL
    })
})

bundles
