## https://dss.integration.data.humancellatlas.org/

#' HCA API methods
#'
#' @rdname hca-api
#' @md
#'
#' @name HCA-API
#'
#' @author Daniel Van Twisk
#'
#' @description Methods to access the Human Cell Atlas's Data
#'     Coordination Platform (HCA DCP) using the platform's REST API.
#'
#' @examples
#'
#' hca <-
#'     HCABrowser() %>%
#'     filter('files.specimen_from_organism_json.organ.text' == "brain")
#' hca
#'
#' result <-
#'     hca %>%
#'     searchBundles(per_page = 10L, output_format = "raw")
#' result
#'
#' tbl <-
#'    results(result)[[1]]$metadata$manifest$files %>%
#'    bind_rows() %>%
#'    mutate(`content-type` = noquote(`content-type`))
#' tbl
#'
NULL

.REPLICAS <- c("aws", "gcp") # "azure" ?

.hca_api_doit <-
    function(FUN, uuid, version, replica, call)
{
    replica <- match.arg(replica, .REPLICAS)
    stopifnot(
        .is_scalar_character(uuid),
        is.null(version) || .is_scalar_character(version)
    )

    if (is.null(version))
        response <- FUN(uuid = uuid, replica = replica)
    else
        response <- FUN(uuid = uuid, version = version, replica = replica)
    .stop_for_status(response, call)
}

#' @rdname hca-api
#' @md
#'
#' @description `checkoutBundle()` initiates the 'checkout' process
#'     from the HCA DCP DSS.
#'
#' @param x An HCABrowser object that is the subject of the request.
#'
#' @param uuid character(1). A RFC4122-compliant ID for the bundle.
#'
#' @param version character(1). Timestamp of bundle creation in RFC3339.
#'
#' @param replica character(1). A replica to fetch form. Can be one of
#'     "aws", "gcp", or "azure".  Default is "aws".
#'
#' @return `checkoutBundle()` returns a character(1) identifier to be
#'     used as the `checkout_job_id=` to determine status of the
#'     checkout using `getBundleCheckout()`.
#'
#' @examples
#'
#' re <- "^([^\\.]+)\\.(.*)$" # uuid / version as before / after the first '.'
#'
#' uuid <-
#'     hca %>%
#'     searchBundles(per_page = 10L, output_format = "summary") %>%
#'     as_tibble() %>%
#'     mutate(
#'        uuid = sub(re, "\\1", bundle_fqid),
#'        version = sub(re, "\\2", bundle_fqid)
#'    ) %>%
#'    pull(uuid)
#' uuid
#'
#' checkout_job_id <- checkoutBundle(hca, uuid[1])
#' checkout_job_id
#'
#' @export
checkoutBundle <-
    function(x, uuid, version = NULL, replica = "aws")
{
    stopifnot(is(x, "HCABrowser"))

    FUN <- x$dss.api.bundles.checkout.post
    response <- .hca_api_doit(FUN, uuid, version, replica, "checkoutBundle()")
    as.list(response)$checkout_job_id
}

#' @rdname deprecated
#' @title Deprecated functions in the HCABrowser package
#' @name deprecated
#' @md
#'
#' @description `getBundle()` is deprecated, use `checkoutBundle()`
#'
#' @keywords internal
#'
#' @export
getBundle <-
    function(x, uuid, version = NULL, replica = "aws")
{
    .Deprecated(
        msg = "'getBundle()' is deprecated, use 'checkoutBundle()' instead"
    )
    checkoutBundle(x, uuid, version, replica)
}

#' @rdname hca-api
#' @md
#'
#' @description `getBundleCheckout()` queries the status of a bundle
#'     checkout request
#'
#' @param checkout_job_id character(1). A RFC4122-complliant ID for
#'     the checkout job request.
#'
#' @return `getBundleCheckout()` returns a list. One component of the
#'     list is `status=`. If the value is `SUCCEEDED`, then the list
#'     contains a second element `location=` containing a URL to the
#'     location of the checkout, e.g., an s3 bucket.
#'
#' @examples
#'
#' getBundleCheckout(hca, checkout_job_id)
#'
#' @export
getBundleCheckout <-
    function(x, checkout_job_id, replica = "aws")
{
    replica <- match.arg(replica, .REPLICAS)
    stopifnot(
        is(x, "HCABrowser"),
        .is_scalar_character(checkout_job_id)
    )

    FUN <- x$dss.api.bundles.checkout.get
    response <- FUN(replica = replica, checkout_job_id = checkout_job_id)
    as.list(response)
}

#' Retrieve a file given a UUID and optionally a version
#'
#' @rdname hca-api
#' @md
#'
#' @description `getFile()` retrieves a file from its UUID.
#'
#' @param destination character(1) path to downloaded file. The path
#'     cannot exist.
#'
#' @return `getFile()` returns the path to the downloaded file.
#'
#' @importFrom httr set_config reset_config
#'
#' @examples
#'
#' fastq <-
#'    tbl %>%
#'    filter(endsWith(name, "fastq.gz")) %>%
#'    select(name, uuid)
#' fastq
#'
#' uuid <- pull(fastq) %>% tail(1)
#' uuid
#'
#' \dontrun{
#' destination <- getFile(hca, uuid)
#' readLines(destination, 4L)
#' }
#'
#' @export
getFile <-
    function(x, uuid, version = NULL, replica = "aws", destination = tempfile())
{
    stopifnot(
        is(x, "HCABrowser"),
        .is_scalar_character(destination),
        !file.exists(destination)
    )
    directory <- dirname(destination)
    if (!dir.exists(directory))
        dir.create(directory, recursive = TRUE)

    ## hard-coded to allow for write_disk()
    FUN <- local({
        api <- AnVIL:::.api(x)
        host <- api$host
        basePath <- api$basePath
        function(uuid, version, replica)  {
            url <- paste0(
                "https://", host, basePath, "/files/", uuid,
                "?replica=", replica,
                if (!is.null(version)) paste0("&version=", version)
            )
            httr::GET(url, httr::write_disk(destination), httr::progress())
        }
    })

    response <- .hca_api_doit(FUN, uuid, version, replica, "getFile()")

    destination
}

#' Retrieve a file's metadata given an UUID and optionally a version
#'
#' @rdname hca-api
#'
#' @description `headFile()` retrieves metadata about a file from its
#'     UUID.
#'
#' @return `headFile()` returns a tibble of technical metadata,
#'     including file size and content type (e.g.,
#'     `application/gzip`), about a file.
#'
#' @examples
#'
#' headFile(hca, uuid)
#'
#' @importFrom dplyr bind_cols %>%
#'
#' @export
headFile <-
    function(x, uuid, version = NULL, replica = "aws")
{
    stopifnot(is(x, "HCABrowser"))

    FUN <- x$`Retrieve_a_file's_metadata_given_an_UUID_and_optionally_a_version.`
    response <- .hca_api_doit(FUN, uuid, version, replica, "headFile()")
    hdrs <- headers(response)
    xdss <- hdrs[startsWith(names(hdrs), "x-dss-")]
    names(xdss) <- sub("x-dss-", "", names(xdss))
    bind_cols(tibble(uuid), as_tibble(xdss)) %>%
        select("uuid", "version", "size", "content-type", dplyr::everything())
}
