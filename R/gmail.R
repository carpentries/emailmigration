
gmail_messages_to_tibble <- function(gm_msgs) {
  gm_msgs[[1]]$messages %>%
    purrr::map_df(
      ~ list(
        id = .[["id"]],
        thread_id = .[["threadId"]]
      )
    )
}

extract_gm_body <- function(gm_msg) {

  body <- gm_body(gm_msg)

  if (length(body) > 0 &&  body != "") {
    return(body)
  }

  if (gm_msg$payload$parts[[1]]$mimeType == "multipart/alternative") {
    body <- gmailr:::base64url_decode_to_char(
      gm_msg$payload$parts[[1]]$parts[[1]]$body$data
    )
    if (!is.null(body) && body != "") {
      return(body)
    }
    stop("don't know this type of email body")
  }

}

format_gm_body <- function(body) {
  commonmark::markdown_html(body)
}

convert_base64url <- function(data) {
  gsub("_", "/", gsub("-", "+", data))
}

extract_gmail_message <- function(gm_msg) {
  x <- gm_msg
  attached_files <- lapply(x$payload$parts, function(part) {
    if (!is.null(part$filename) && part$filename != "") {
      list(
        file_name = part$filename,
        mime_type = part$mimeType,
        data = convert_base64url(
          gm_attachment(part$body$attachmentId, x$id)$data
        )
      )
    }
  })
  attached_files <- purrr::discard(attached_files, is.null)
  list(
    to =  gm_to(x),
    from = gm_from(x),
    date = gm_date(x),
    subject = gm_subject(x),
    id = gm_id(x),
    body = format_gm_body(extract_gm_body(x)),
    attached_files = attached_files
  )
}

gmail_messages_from_thread <- function(gm_thread_id) {

  thrd <- gmailr::gm_thread(id = gm_thread_id)
  map(
    thrd$messages,
    extract_gmail_message
  )

}

convert_date <- function(gm_datetime) {
  paste0(
    anytime::iso8601(anytime::anytime(gm_datetime, tz = "UTC")),
    "Z"
  )
}

extract_email <- function(email_string) {
  if (!grepl("<", email_string) && !grepl(">", email_string)) {
    return(email_string)
  }

  email_pattern <- gregexpr("<(.*?)>", email_string)
  email_match <- regmatches(email_string, email_pattern)
  emails <- gsub("<|>", "", unlist(email_match))

  emails
}

initialize_hs_thread <- function(subject, customer_email, created_at) {
  HS_conversation$new(
    conversation_subject = subject,
    conversation_customer = customer_email,
    created_at = created_at
  )
}

add_message_to_hs_thread <- function(thread, customer_email, text,
                                     attachments, ...) {
  thread$add_message(
    customer_email, text, attachments = attachments, ...
  )
}

convert_gmail_thread <- function(gmail_thread_id) {

  gm_msg <- gmail_messages_from_thread(gmail_thread_id)

  thread_subject <- gm_msg[[1]]$subject
  thread_customer_email <- extract_email(gm_msg[[1]]$from)
  thread_creation <- convert_date(gm_msg[[1]]$date)

  thread <- initialize_hs_thread(
    thread_subject,
    thread_customer_email,
    thread_creation
  )

  purrr::walk(
    gm_msg,
    function(msg) {
      add_message_to_hs_thread(
        thread,
        customer_email = extract_email(msg$from),
        text = msg$body,
        created_at = convert_date(msg$date),
        attachments = msg$attached_files
      )
    }
  )

  thread

}

##

if (FALSE) {
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

  ## TODO
  ##
  ## - [ ] rewrite gm_body to extract text or html version of multipart message
  ## - [ ] figure out how to deal with other people included in the conversation
  ##       (use cc?)
  ## - [ ] create a store cache for each thread, and store success/failures

}
