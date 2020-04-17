### Gmail authentication -------------------------------------------------------

# Configure your app
gm_auth_configure(path = "credentials.json")

# Authenticate with the new cache, store tokens in .secret
gm_auth(cache = ".secret")


### HelpScout authentication ---------------------------------------------------

hs_app <- httr::oauth_app(
  "helpscout",
  key = Sys.getenv("HELPSCOUT_OAUTH_KEY"),
  secret = Sys.getenv("HELPSCOUT_OAUTH_SECRET")
)

hs_token <- httr::oauth2.0_token(
  httr::oauth_endpoint(
    authorize = "https://secure.helpscout.net/authentication/authorizeClientApplication",
    access = "https://api.helpscout.net/v2/oauth2/token"),
  app = hs_app)

htoken <- httr::config(token = hs_token)
