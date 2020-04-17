## Code to Perform email migration from Gmail to HelpScout

This repository contains the code that was used to transfer the Regional
Coordinators's emails from Gmail to HelpScout around April 10th, 2020.


## Code used

```r
res <- convert_gmail_thread("16ce4923ce2c6e35")
res_hs <- hs_create_thread(res$get(), htoken)


## first 100 threads
threads <- gm_threads()
threads_id <- map_chr(threads[[1]]$threads, "id")

cvrtd_threads <- map(
  threads_id,
  convert_gmail_thread
)

res_hs_uploads <- map(
  cvrtd_threads,
  function(x) {
    hs_create_thread(x$get(), htoken)
  }
)

## Actual migration
get_all_threads <- function() {
  
  first_it <- gm_threads()
  next_token <- first_it[[1]]$nextPageToken
  
  res <- append(list(), first_it)
  
  while (length(next_token) > 0) {
    tmp <- gm_threads(page_token = next_token)
    res <- append(
      res, tmp
    )
    next_token <- tmp[[1]]$nextPageToken
    message("next token: ", next_token)
  }
  
  res
}

threads <- get_all_threads()

threads_ids <- map(
  threads,
  ~ map_chr(.$threads, ~ .$id)
) %>%
  unlist()

hs_res <- purrr::walk(
  threads_ids,
  ~ get_hs_response(., namespace = "v2020-04-10.1")
)


idx <- store_hs_responses()$list(namespace = "v2020-04-10.1")

is_error <- purrr::map_lgl(
  idx,
  ~ httr::status_code(store_hs_responses()$get(., namespace = "v2020-04-10.1")) >=  400
)

## How many calls failed?
sum(is_error)

## Which thread_ids failed?
idx[is_error]
```
