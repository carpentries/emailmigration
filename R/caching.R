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


fetch_hook_hs_response <- function(key, namespace) {
  res <- get_gmail_thread(key, namespace)
  hs_create_thread(res$get(), htoken)
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
