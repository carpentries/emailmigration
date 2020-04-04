## # Configure your app
## gm_auth_configure(path = "credentials.json")

## # Authenticate with the new cache, store tokens in .secret
## gm_auth(cache = ".secret")


## ## helpscout authentication
## hs_app <- oauth_app(
##   "helpscout",
##   key = "lu47u2YnCiA1Gy5xHY0YDwrSwZJefY8P",
##   secret = "LPjbYIKQiYelUxhGlMJ4L0WkpUPvuEap"
## )

## hs_token <- oauth2.0_token(oauth_endpoint(authorize = "https://secure.helpscout.net/authentication/authorizeClientApplication",  access = "https://api.helpscout.net/v2/oauth2/token"), app = hs_app)

## htoken <- config(token = hs_token)

test_message <- list(
  subject = "test message 3",
  customer = list(
    email = "francois@test.test"
  ),
  mailboxId = 213884,
  type = "email",
  imported = TRUE,
  status = "closed",
  createdAt = "2019-12-31T23:58:00Z",
  threads = list(
    list(
      type = "customer",
      customer = list(
        email = "francois@test.test"
      ),
      text = "this is a test message creat\ned from \n an API \n call. Fatma, \n \nWonderful! "
    ),
    list(
      type = "customer",
      customer = list(
        email = "francois@test.test"
      ),
      text = "I have another question in the same thread."
    )
  ),
  tags = list("email-import")
)

hs_parse <- function(res) {
  txt <- httr::content(res, "text", encoding = "UTF-8")
  if (identical(httr::headers(res)[["content-type"]], "application/hal+json")) {
    return(
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    )
  }
  if (identical(txt, "")) {
    stop("No output to parse, check your query", call. = FALSE)
  }
  txt
}

hs_check_res <- function(res) {
  if (! httr::status_code(res) < 400) {
    msg <- hs_parse(res)
    stop(
      "HTTP failure: ",
      httr::status_code(res),
      "\n  ",
      msg,
      call. = FALSE)
  }
  hs_parse(res)
}

hs_mailboxes_raw <- function(hstoken) {

  mb <- httr::GET(
    "https://api.helpscout.net",
    path = "/v2/mailboxes",
    htoken
  )
  hs_check_res(mb)

}

hs_mailboxes <- function(hstoken) {

  mb_raw <- hs_mailboxes_raw(hstoken)

  mbs <- mb_raw[["_embedded"]][["mailboxes"]] %>%
    purrr::map_df(
      ~ list(
        id = .[["id"]],
        name = .[["name"]],
        slug = .[["slug"]],
        email = .[["email"]],
        link = .[["_links"]][["self"]][["href"]]
      )
    )

  mbs
}

hs_mailbox_id <- function(mailbox_name = NULL, mailbox_email = NULL,
                          ignore_case = TRUE, hstoken) {
  if (!is.null(mailbox_name) && !is.null(mailbox_email)) {
    stop("only one of mailbox_name or mailbox_email can be specified")
  }

  mbs <- hs_mailboxes(hstoken)

  if (ignore_case) {
    mbs$name <- tolower(mbs$name)
    mbs$email <- tolower(mbs$email)
    mailbox_name <- tolower(mailbox_name)
    mailbox_email <- tolower(mailbox_email)
  }

  if (!is.null(mailbox_name) && rlang::is_string(mailbox_name)) {
    res <- mbs$id[mbs$name == mailbox_name]
  } else if (!is.null(mailbox_email) && rlang::is_string(mailbox_email)) {
    res <- mbs$id[mbs$email == mailbox_email]
  } else {
    stop("mispecified call.")
  }

  if (identical(length(res), 0L)) {
    warning("No match found.")
  }

  res

}

## hs_mailbox_id(mailbox_email = "workshops@carpentries.org", hstoken = htoken)

hs_create_thread <- function(thread, hstoken) {
  body <- jsonlite::toJSON(thread, auto_unbox = TRUE)

  res <- httr::POST(
    "https://api.helpscout.net",
    path = "/v2/conversations",
    body = body,
    htoken,
    content_type("application/json; charset=UTF-8")
  )

  res
}
