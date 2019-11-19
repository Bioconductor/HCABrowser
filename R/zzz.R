
apiClient <- NULL

.onLoad <- function(libname, pkgname)
{
    apiClient <<- DefaultApi$new()
}

