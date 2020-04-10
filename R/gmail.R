
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

  ## no multipart
  if (identical(x$payload$mimeType, "text/html")) {
    msg <- list(
      data = x$payload$body$data,
       mime_type = x$payload$mimeType
     )
  } else if (identical(x$payload$mimeType, "text/plain")) {
    msg <- list(
      data = x$payload$body$data,
      mime_type = x$payload$mimeType
    )
  } else {
    ## has multipart
    if (!grepl("^multipart", x$payload$mimeType)) {
      message("not really multipart!")
      browser()
    }

    ## we can now look into the mime type of each part
    mime_types <- sapply(x$payload$parts, function(p) p$mimeType) #, character(1))
    if (length(mime_types) == 0) {
      message("no mime types")
      browser()
    }
    message(paste(mime_types, collapse = "; "))

    txt_content <- grep("^multipart|^text", mime_types)

    if (any(grepl("^multipart", mime_types))) {
      txt_content <- grep("^multipart", mime_types)
    } else if (any(grepl("^text", mime_types))) {
      txt_content <- match(type, mime_types)
    } else {
      message("don't know what to do")
      browser()
      NULL
    }

    if (length(txt_content) > 1 || length(txt_content) < 1) {
      message("issue:multiple matches")
      browser()
      NULL
    }

    msg <- lapply(x$payload$parts[txt_content], function(p) {
      if (identical(p$mimeType, "multipart/alternative") ||
            identical(p$mimeType, "multipart/related")) {
        types <- vapply(p$parts, function(pp) {
          pp$mimeType
        }, character(1))
        pos <- match(type, types)
        if (is.na(pos)) {
          warning("mime type ", sQuote(type), " not found in this message.")
          browser()
          return(NA_character_)
        }
        mt <- types[pos]
        return(
          list(
            data = p$parts[[pos]]$body$data,
            mime_type = mt
          )
        )
      } else if (identical(p$mimeType, "text/plain") ||
                   identical(p$mimeType, "text/html")) {
        return(
          list(
            data = p$body$data,
            mime_type = p$mimeType
          )
        )
      } else {
        browser()
        NULL
      }
    })

    if (any(sapply(msg, function(m) is.null(m$data)))) browser()
    if (length(msg) > 1) browser()
    msg <- msg[[1]]
  }

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

  email_string %>%
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

}
