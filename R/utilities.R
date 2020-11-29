.is_scalar <- function(x, na.ok = FALSE)
    length(x) == 1L && (na.ok || !is.na(x))

.is_scalar_integer <- function(x, na.ok = FALSE)
    is.integer(x) && .is_scalar(x, na.ok)

.is_scalar_logical <- function(x, na.ok = FALSE)
    is.logical(x) && .is_scalar(x, na.ok)

#' @importFrom httr status_code
.stop_for_status <-
    function(x, call)
{
    code <- status_code(x)
    if (code >= 400L) {
        response <- as.list(x)
        reason <- if (is.null(x$code)) response$message else response$code
        stop(
            "'", call, "' failed\n",
            "status code: ", code, "\n",
            "reason: ", reason, "\n",
            "detail:\n", response$title,
            call. = FALSE
        )
    }
}
