################################################################################
context("HCA-API")
################################################################################

hca <- HumanCellAtlas()

test_that("getBundlesCheckout", {
    
})

test_that("deleteBundle", {

})

test_that("getBundle", {

})

test_that("putBundle", {

})

test_that("postBundlesCheckout", {

})

test_that("putCollection", {

})

test_that("deleteCollection", {

})

test_that("getCollection", {

})

test_that("patchCollection", {

})

test_that("getFile", {

})

test_that("headFile", {

})

test_that("putFile", {

})

test_that("postSearch", {
    es_query <- list(
        query = list(
            bool = list(
                must = list(
                #list(match = list(files.biomaterial_json.biomaterials.content.organ.text = "brain")),
                    list(match = list(files.biomaterial_json.biomaterials.content.genus_species = "Homo_sapiens"))
                    #list(match = list(files.file_json.files.content.file_core.file_format = "results")),
                    #list(range = list(manifest.version = list(gte = "2018-07-12T100000.000000Z")))
                )
            )
        )
    )
    res <- postSearch(hca, 'aws', 'raw', es_query=es_query)
})

test_that("getSubscriptions", {

})

test_that("putSubscription", {

})

test_that("deleteSubscription", {

})

test_that("getSubscription", {

})
