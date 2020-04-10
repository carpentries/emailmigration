
gmail_messages_to_tibble <- function(gm_msgs) {
  gm_msgs[[1]]$messages %>%
    purrr::map_df(
      ~ list(
        id = .[["id"]],
        thread_id = .[["threadId"]]
      )
    )
}

format_gm_body <- function(msg) {
  if (identical(attr(msg, "mime_type"), "text/plain"))
    return(commonmark::markdown_html(msg))

  msg
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
    body = format_gm_body(my_gm_body(x)),
    attached_files = attached_files
  )
}

gmail_messages_from_thread <- function(gm_thread_id) {

  thrd <- gmailr::gm_thread(id = gm_thread_id)
  purrr::map(
    thrd$messages,
    extract_gmail_message
  )

}

my_gm_body <- function(x, type = "text/html", collapse = FALSE, ...) {

  extract_message_body <- function(xx) {
    ## no multipart
    if (identical(xx$mimeType, "text/html")) {
      if (exists("attachmentId", xx$body)) {
        msg <- list(
          data = convert_base64url(
            gm_attachment(xx$body$attachmentId, x$id)$data
          ),
          mime_type = xx$mimeType
        )
      } else {
        msg <- list(
          data = xx$body$data,
          mime_type = xx$mimeType
        )
      }
    } else if (identical(xx$mimeType, "text/plain")) {
      msg <- list(
        data = xx$body$data,
        mime_type = xx$mimeType
      )
    } else if (grepl("^multipart", xx$mimeType)) {
      ## has multipart
      msg <- lapply(xx$parts, extract_message_body)
    } else {
      msg <- NULL
    }
    return(msg)
  }

  msg <- extract_message_body(x$payload)
  msg <- unlist(msg)
  data_pos <- names(msg) %in% "data"
  mime_type_pos <- names(msg) %in% "mime_type"

  msg <- purrr::map2(
    msg[data_pos],
    msg[mime_type_pos],
    ~ list(
      data = .x,
      mime_type = .y
    )
  )

  mime_types <- vapply(msg, function(.x) .x$mime_type, character(1))

  m <- match(type, mime_types)

  if (is.na(m)) {
    m <- match("text/plain", mime_types)

    if (is.na(m)) {
      browser()
    }
  }

  msg <- msg[[m]]

  res <- gmailr:::base64url_decode_to_char(msg$data)

  if (collapse) {
    res <- paste0(res, collapse = "\n")
  }

  attr(res, "mime_type") <- msg$mime_type
  res

}



convert_date <- function(gm_datetime) {
  paste0(
    anytime::iso8601(anytime::anytime(gm_datetime, tz = "UTC")),
    "Z"
  )
}

extract_email <- function(email_string) {

  if (is.null(email_string)) {
    return(NULL)
  }

  res <- email_string %>%
    strsplit(", ") %>%
    unlist() %>%
    purrr::map_chr(function(es) {
      if (!grepl("<", es) && !grepl(">", es)) {
        return(es)
      }

      email_pattern <- gregexpr("<(.*?)>", es)
      email_match <- regmatches(es, email_pattern)
      emails <- gsub("<|>", "", unlist(email_match))

      emails
    })

  res[grepl("@", res)]
}

initialize_hs_thread <- function(subject, customer_email, created_at) {
  HS_conversation$new(
    conversation_subject = subject,
    conversation_customer = customer_email,
    created_at = created_at
  )
}

add_message_to_hs_thread <- function(thread, customer_email, text,
                                     attachments, cc, ...) {
  thread$add_message(
    customer_email, text, attachments = attachments, cc = cc, ...
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
        attachments = msg$attached_files,
        cc = extract_email(msg$to)
      )
    }
  )

  thread

}

### Gmail labels ---------------------------------------------------------------

my_gm_modify_thread <- function(id, add_labels = character(0),
                                remove_labels = character(0),
                                user_id = "me") {
  body <- list(
    addLabelIds = list(add_labels),
    removeLabelIds = list(remove_labels)
  )
  req <- httr::POST(
    gmailr:::gmail_path(
      gmailr:::rename(user_id), "threads", id, "modify"
    ),
    body = body, encode = "json", gmailr::gm_token())
  httr::stop_for_status(req)
  invisible(httr::content(req, "parsed"))
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

  ## Real deal
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

}
