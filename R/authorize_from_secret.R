authorize_from_secret <- function(access_token) {

  client_id <- getOption("slides.client.id")
  client_secret <- getOption("slides.client.secret")

  credentials = list(
    access_token = access_token,
    expires_in = 3599L,
    # refresh_token = refresh_token,
    scope = "https://www.googleapis.com/auth/presentations https://www.googleapis.com/auth/drive.readonly",
    token_type = "Bearer")

  app <- httr::oauth_app(appname = "googleslides", key = client_id,
                         secret = client_secret)
  endpoint <- httr::oauth_endpoints("google")

  token <- httr::oauth2.0_token(endpoint = endpoint, app = app,
                                scope = c("https://www.googleapis.com/auth/presentations",
                                          "https://www.googleapis.com/auth/drive.readonly"),
                                credentials = credentials)

  out <- rgoogleslides::authorize(token = token)

  return(out)
}
