## HelpScout    | Gmail
## thread       | message
## conversation | thread

HS_attachment <- R6::R6Class("HS_attachment",
  private = list(
    file_name = NA_character_,
    mime_type = NA_character_,
    data = NA_character_
  ),
  public = list(
    initialize = function(file_name, mime_type, data) {
      private$file_name <- file_name
      private$mime_type <- mime_type
      private$data <- data
    },
    get = function() {
      if (any(is.na(c(private$file_name, private$mime_type, private$data)))) {
        stop("object not yet initialized")
      }
      list(
        fileName = private$file_name,
        mimeType = private$mime_type,
        data = private$data
      )
    }
  ))

HS_thread <- R6::R6Class("HS_thread",
  private = list(
    text = NA_character_,
    customer = NULL,
    imported = TRUE,
    cc = NA_character_,
    bcc = NA_character_,
    created_at = NA_character_,
    attachments = NULL
  ),
  public = list(
    initialize = function(customer_email, text, imported = TRUE, cc = NULL,
                          bcc = NULL, created_at = NULL, attachments = NULL) {
      private$customer <- customer_email
      private$text <- text
      private$imported <- imported
      private$cc <- cc
      private$bcc <- bcc
      private$created_at <- created_at
      if (!is.null(attachments) && is.list(attachments)) {
        private$attachments <- purrr::map(
          attachments,
          function(a) {
            atcht <- HS_attachment$new(a$file_name, a$mime_type, a$data)
            atcht$get()
          }
        )
      }
      invisible(self)
    },
    get = function() {
      if (is.null(private$customer)) {
        stop("object hasn't been initialized properly", call. = FALSE)
      }
      res <- list(
        type = "customer",
        customer = list(
          email = private$customer
        ),
        imported = private$imported,
        cc = I(private$cc),
        bcc = private$bcc,
        text = private$text,
        createdAt = private$created_at,
        attachments = private$attachments
      )
      purrr::discard(res, ~ is.null(.) || length(.) == 0L)
    }
  )
)

HS_conversation <- R6::R6Class("HS_conversation",
  private = list(
    conversation_subject = NA_character_,
    conversation_customer = NA_character_,
    hs_mailbox_id = NA_integer_,
    imported = TRUE,
    type = "email",
    status = "closed",
    created_at = NA_character_,
    threads = list(),
    tags = function() {
      default <- "email-import"
      if (identical(length(private$threads), 0L)) {
        return(list(default))
      }

      all_emails <- purrr::map(
        private$threads,
        ~ unlist(c(.$customer$email, .$cc))
      ) %>%
        unlist() %>%
        unique()

      if ("admin-afr@carpentries.org" %in% all_emails) {
        default <- c("admin-afr", default)
      }

      if ("admin-au@carpentries.org" %in% all_emails) {
        default <- c("admin-au", default)
      }

      if ("admin-ca@carpentries.org" %in% all_emails) {
        default <- c("admin-ca", default)
      }

      if ("admin-nordic@carpentries.org" %in% all_emails) {
        default <- c("admin-nordic", default)
      }

      if ("admin-nz@carpentries.org" %in% all_emails) {
        default <- c("admin-nz", default)
      }

      if ("admin-uk@carpentries.org" %in% all_emails) {
        default <- c("admin-uk", default)
      }

      if (identical(length(default), 1L)) {
        default <- c("admin-unknown", default)
      }

      as.list(default)

    }
  ),
  public = list(
    initialize = function(conversation_subject, conversation_customer,
                          hs_mailbox_id = 213884, created_at) {
      private$conversation_subject <- conversation_subject
      private$conversation_customer <- conversation_customer
      private$hs_mailbox_id <- hs_mailbox_id
      private$created_at <- created_at
    },
    add_message = function(customer_email, text, attachments, cc, ...) {
      msg <- HS_thread$new(customer_email, text, attachments = attachments,
        cc = cc, ...)
      private$threads <- append(
        private$threads,
        list(msg$get())
      )
      invisible(self)
    },
    get = function() {
      if (identical(length(private$threads), 0L)) {
        stop("no messages in this thread")
      }
      list(
        subject = private$conversation_subject,
        customer = list(email = private$conversation_customer),
        mailboxId = private$hs_mailbox_id,
        imported = private$imported,
        type = private$type,
        status = private$status,
        threads = private$threads,
        tags = private$tags()
      )
    })
)
