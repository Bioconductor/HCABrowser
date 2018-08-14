#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @importFrom googleAuthR gar_auth gar_auto_auth gar_set_client
#' @export

get_token <- function() {
#  if(Sys.getenv('HCA_TOKEN') != '')  return(Sys.getenv('GDC_TOKEN'))
#  token_file <- "~/.hca_token"
#  if(file.exists(token_file))
#    return(suppressWarnings(readLines(token_file,n=1)))

#  oauth_endpoints("google")

#  myapp <- oauth_app("google",
#    key = "16795585089.apps.googleusercontent.com",
#    secret = "fj4D0Dj1o9KCx1478IiiLMnM")

#  google_token <- oauth2.0_token(oauth_endpoints("google"), myapp,
#    scope = "https://www.googleapis.com/auth/userinfo.email",
#    cache = token_file)

  required_scopes <- c("https://www.googleapis.com/auth/userinfo.email")
  
  gar_set_client(system.file("client.json", package = "HCABrowser"),
                 scopes = required_scopes)
  
  gar_auto_auth(required_scopes,
                new_user = FALSE,
                no_auto = FALSE,
                environment_var = "BQ_AUTH_FILE")
}
