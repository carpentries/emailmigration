### Gmail threads conversion ---------------------------------------------------

fetch_hook_gmail_threads <- function(key, namespace) {
  convert_gmail_thread(key)
}

store_gmail_threads <- function(path = "cache/threads") {
  storr::storr_external(
    storr::driver_rds(path),
    fetch_hook_gmail_threads
  )
}

get_gmail_thread <- function(thread_id, namespace) {
  invisible(store_gmail_threads()$get(thread_id, namespace))
}


### Gmail labels ---------------------------------------------------------------

fetch_hook_gmail_labels <- function(key, namespace) {
  all_labels <- gmailr::gm_labels()
  all_label_names <- purrr::map_chr(all_labels$labels, ~ .$name)

  m <- match(key, all_label_names)

  if (is.na(m)) {
    message("Creating label: ", key)
    new_label <- gmailr::gm_create_label(key)
    new_label$id
  } else {
    all_labels$labels[[m]]$id
  }
}

store_gmail_labels <- function(path = "cache/gmail-labels") {
  storr::storr_external(
    storr::driver_rds(path),
    fetch_hook_gmail_labels
  )
}

get_gmail_label <- function(label) {
  store_gmail_labels()$get(label)
}


### HelpSout queries responses -------------------------------------------------

fetch_hook_hs_response <- function(key, namespace) {
  res <- get_gmail_thread(key, namespace)
  rsp <- hs_create_thread(res$get(), htoken)

  if (identical(httr::content(rsp, "text", encoding = "utf-8"), "")) {
    lbl <- paste0("success-", namespace)
    my_gm_modify_thread(
      id = key, add_labels = get_gmail_label(lbl)
    )
  } else {
    message("failure for thread: ", key)
    message(httr::content(rsp, "text", encoding = "utf-8"))
    lbl <- paste0("failure-", namespace)
    my_gm_modify_thread(
      id = key, add_labels = get_gmail_label(lbl)
    )
  }

  rsp
}

store_hs_responses <- function(path = "cache/hs_responses") {
  storr::storr_external(
    storr::driver_rds(path),
    fetch_hook_hs_response
  )
}

get_hs_response <- function(thread_id, namespace = "v2020-04-10.1") {
  invisible(store_hs_responses()$get(thread_id, namespace))
}
