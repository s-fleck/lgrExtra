  # AppenderDigest --------------------------------------------------------

#' Abstract class for digests
#'
#' Digests is an abstract class for report-like output that contain several
#' log messages and a title. A praictical example would be an E-mail containg
#' the last 10 log messages before an error was encountered.
#'
#' @template abstract_class
#'
#' @export
#' @seealso [LayoutFormat], [LayoutGlue]
#' @name AppenderDigest
#' @family Digest Appenders
AppenderDigest <-  R6::R6Class(
  "AppenderDigest",
  inherit = lgr::AppenderMemory,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(...){
      stop(CannotInitializeAbstractClassError())
    },


    set_subject_layout = function(layout){
      assert(inherits(layout, "Layout"))
      private$.subject_layout <- layout
      invisible(self)
    },


    format = function(
      color = FALSE,
      ...
    ){
      res <- super$format(color = color, ...)

      if (!color)
        style_error <- identity

      if (class(self)[[1]] == "AppenderDigest"){
        paste(res[[1]], style_error("[abstract class]"))
      } else {
        res
      }
    }
  ),


  active = list(
    #' @field subject_layout A [Layout] used to format the last [LogEvent]
    #'   in this Appenders buffer when it is flushed. The result will be used as
    #'   the subject of the tigest (for example, the E-mail subject).
    subject_layout = function() get(".subject_layout", private)
  ),


  private = list(
    .subject_layout = NULL
  )
)




# AppenderPushbullet --------------------------------------------------------


#' Send push-notifications via RPushbullet
#'
#' Send push notifications via [pushbullet](https://www.pushbullet.com/). This
#' Appender keeps an in-memory buffer like [AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message that is sent to
#' [RPushbullet::pbPost()]. The default behaviour is to push the last 7 log
#' events in case a `fatal` event is encountered.
#'
#'
#' @export
#' @family Appenders
#' @family Digest Appenders
#' @seealso [LayoutFormat], [LayoutGlue]
#' @name AppenderPushbullet
#' @export
AppenderPushbullet <- R6::R6Class(
  "AppenderPushbullet",
  inherit = AppenderDigest,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(
      threshold = NA_integer_,
      flush_threshold = "fatal",
      layout = LayoutFormat$new(fmt = "%K  %t> %m %f", timestamp_fmt = "%H:%M:%S"),
      subject_layout = LayoutFormat$new(fmt = "[LGR] %L: %m"),
      buffer_size = 6,
      recipients = NULL,
      email = NULL,
      channel = NULL,
      devices = NULL,
      apikey = NULL,
      filters = NULL
    ){
      assert_namespace("RPushbullet")

      private$initialize_buffer(buffer_size)
      self$set_flush_on_rotate(FALSE)
      self$set_flush_on_exit(FALSE)
      self$set_filters(filters)

      self$set_layout(layout)
      self$set_threshold(threshold)
      self$set_flush_threshold(flush_threshold)
      self$set_apikey(apikey)
      self$set_buffer_size(buffer_size)
      self$set_subject_layout(subject_layout)
      self$set_should_flush(function(event){
        is.na(.obj()[["flush_threshold"]]) || all(event[["level"]] <= .obj()[["flush_threshold"]])
      })

      self$set_recipients(recipients)
      self$set_email(email)
      self$set_channel(channel)
      self$set_devices(devices)
    },

    flush = function(
    ){
      assign("insert_pos", 0L, envir = private)

      body <- paste(
        unlist(lapply(self$buffer_events, self$layout$format_event)),
        collapse = "\n"
      )
      le    <- self$buffer_events[[length(self$buffer_events)]]
      title <- self$subject_layout$format_event(le)

      cl <- list(
        type = "note",
        title = title,
        body  = body
      )

      if (!is.null(self$apikey)){
        cl$apikey <- self$apikey
        cl$recipients <- self$recipients
        cl$email <- self$email
        cl$channel <- self$channel
        cl$devices <- self$devices
      }

      do.call(RPushbullet::pbPost, compact(cl))
      private$.buffer_events <- list()

      invisible(self)
    },

    set_apikey = function(x){
      assert(is.null(x) || is_scalar_character(x))
      private$.apikey <- x
      invisible(self)
    },

    set_recipients = function(x){
      private$.recipients <- x
      invisible(self)
    },

    set_email = function(x){
      private$.email <- x
      invisible(self)
    },

    set_channel = function(x){
      private$.channel <- x
      invisible(self)
    },

    set_devices = function(x){
      private$.devices <- x
      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(

    #' @field apikey see [RPushbullet::pbPost()]
    apikey = function() private$.apikey,

    #' @field recipients see [RPushbullet::pbPost()]
    recipients = function() get(".recipients", private),

    #' @field email see [RPushbullet::pbPost()]
    email = function() get(".email", private),

    #' @field channel see [RPushbullet::pbPost()]
    channel = function() get(".channel", private),

    #' @field devices see [RPushbullet::pbPost()]
    devices = function() get(".devices", private),


    destination = function(){
      if (!is.null(self$recipients))
        return(self$recipients)
      else if (!is.null(self$email)){
        return(self$email)
      } else if (!is.null(self$channel)){
        self$channel
      }
    }
  ),


  private = list(
    .apikey = NULL,
    .recipients = NULL,
    .email = NULL,
    .channel = NULL,
    .devices = NULL
  )
)




# AppenderMail ------------------------------------------------------------

#' Abstract class for email appenders
#'
#' @template abstract_class
#'
#' @inheritSection AppenderDigest Fields
#' @inheritSection AppenderDigest Methods
#'
#'@section Fields:
#' \describe{
#'   \item{`to`, `from`, `cc`, `bcc`}{
#'     `character` vectors.
#'   }
#'   \item{`html`, `set_html()`}{`TRUE` or `FALSE`. Send a html email message?
#'     This does currently only formats the log contents as monospace verbatim
#'     text.
#'   }
#' }
#' @family Digest Appenders
#' @name AppenderMail
#' @keywords internal
#'
AppenderMail <- R6::R6Class(
  "AppenderMail",
  inherit = AppenderDigest,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(

    initialize = function(...){
      stop(CannotInitializeAbstractClassError())
    },

    set_to = function(x){
      private$.to <- x
      invisible(self)
    },


    set_from = function(x){
      private$.from <- x
      invisible(self)
    },


    set_cc = function(x){
      private$.cc <- x
      invisible(self)
    },


    set_bcc = function(x){
      private$.bcc <- x
      invisible(self)
    },


    set_html = function(x){
      assert(is_scalar_bool(x))
      private$.html <- x
      invisible(self)
    },


    format = function(
      color = FALSE,
      ...
    ){
      res <- super$format(color = color, ...)

      if (!color)
        style_error <- identity

      if (class(self)[[1]] == "AppenderMail"){
        paste(res[[1]], style_error("[abstract class]"))
      } else {
        res
      }
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(

    #' @field to `character` vector. The email addresses of the recepient
    to = function() get(".to", envir = private),

    #' @field to `character` vector. The email address of the sender
    from = function() get(".from", envir = private),

    #' @field to `character` vector. The email addresses of the cc-recepients (carbon copy)
    cc = function() get(".cc", envir = private),

    #' @field to `character` vector. The email addresses of bcc-recepients (blind carbon copy)
    bcc = function() get(".bcc", envir = private),

    #' @field html `logical` scalar.  Send a html email message?
    #'  This does currently only format the log contents as monospace verbatim
    #'  text.
    html = function() get(".html", envir = private),
    destination = function() self$to
  ),

  private = list(
    .to = NULL,
    .from = NULL,
    .cc = NULL,
    .bcc = NULL,
    .html = NULL
  )
)




# AppenderSendmail --------------------------------------------------------

#' Send emails via sendmailR
#'
#' Send mails via [sendmailR::sendmail()], which requires that you have access
#' to an SMTP server that does not require authentication. This
#' Appender keeps an in-memory buffer like [AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message. The default behaviour
#' is to push the last 30 log events in case a `fatal` event is encountered.
#'
#' @note The default Layout's `fmt` indents each log entry with 3 blanks. This
#'   is a workaround so that Microsoft Outlook does not mess up the line breaks.
#'
#' @examples
#' \dontrun{
#' lgr::AppenderSendmail$new(
#'   to = "user@ecorp.com",
#'   control = list(smtpServer = "mail.ecorp.com"),
#'   from = "lgr_user@yourmail.com"
#' )
#' }
#'
#' @export
#' @seealso [LayoutFormat], [LayoutGlue]
#' @family Appenders
#' @family Digest Appenders
#' @name AppenderSendmail
#' @export
AppenderSendmail <- R6::R6Class(
  "AppenderSendmail",
  inherit = AppenderMail,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(
      to,
      control,
      threshold = NA_integer_,
      flush_threshold = "fatal",
      layout = LayoutFormat$new(fmt = "   %L [%t] %m %f", timestamp_fmt = "%H:%M:%S"),
      subject_layout = LayoutFormat$new(fmt = "[LGR] %L: %m"),
      buffer_size = 29,
      from = get_user(),
      cc = NULL,
      bcc = NULL,
      html = FALSE,
      headers = NULL,
      filters = NULL
    ){
      assert_namespace("sendmailR")
      assert(is_scalar_bool(html))

      private$initialize_buffer(buffer_size)
      self$set_flush_on_rotate(FALSE)
      self$set_flush_on_exit(FALSE)
      self$set_filters(filters)

      self$set_to(to)
      self$set_control(control)
      self$set_from(from)
      self$set_cc(cc)
      self$set_bcc(bcc)
      self$set_headers(headers)
      self$set_subject_layout(subject_layout)
      self$set_html(html)

      self$set_layout(layout)
      self$set_threshold(threshold)
      self$set_flush_threshold(flush_threshold)
    },

    flush = function(
    ){
      assign("insert_pos", 0L, envir = private)

      # body
      body <- paste(
        unlist(lapply(self$buffer_events, self$layout$format_event)),
        collapse = "\r\n"
      )
      if (self$html) {
        body <- paste0("<pre>\n", body, "</pre>\n")
        body <- sendmailR::mime_part(body)
        body[["headers"]][["Content-Type"]] <- "text/html; charset=UTF-8;"
      } else {
        body <- sendmailR::mime_part(body)
        body[["headers"]][["Content-Type"]] <- "text/plain; charset=UTF-8;"
      }


      # title
      le    <- self$buffer_events[[length(self$buffer_events)]]
      title <- self$subject_layout$format_event(le)
      title <- try(
        iconv(title, from = "UTF-8", to = "ASCII//TRANSLIT"),
        silent = TRUE
      )


      args <- list(
        from = self$from,
        to   = self$to,
        subject = title,
        msg = body,
        cc = self$cc,
        bcc = self$bcc,
        headers = self$headers,
        control = self$control
      )

      # sendmailR expects missing() instead of NULL for default values
      args <- compact(args)

      do.call(sendmailR::sendmail, args)
      private$.buffer_events <- list()

      invisible(self)
    },

    set_control = function(x){
      private$.control <- x
    },

    set_headers = function(x){
      private$.headers <- x
    }
  ),


  # +- active ---------------------------------------------------------------
  active = list(

    #' @field control see [sendmailR::sendmail()]
    control = function() get(".control", envir = private),

    #' @field headers see [sendmailR::sendmail()]
    headers = function() get(".headers", envir = private)
  ),

  private = list(
    .control = NULL,
    .headers = NULL
  )
)




# AppenderGmail --------------------------------------------------------


#' Send emails via gmailr
#'
#' Send mails via [gmailr::send_message()]. This
#' Appender keeps an in-memory buffer like [AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message. The default behaviour
#' is to push the last 30 log events in case a `fatal` event is encountered.
#'
#' @export
#' @seealso [LayoutFormat], [LayoutGlue]
#' @family Appenders
#' @name AppenderGmail
#' @export
AppenderGmail <- R6::R6Class(
  "AppenderGmail",
  inherit = AppenderMail,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    initialize = function(
      to,
      threshold = NA_integer_,
      flush_threshold = "fatal",
      layout = LayoutFormat$new(fmt = "%L [%t] %m %f", timestamp_fmt = "%H:%M:%S"),
      subject_layout = LayoutFormat$new(fmt = "[LGR] %L: %m"),
      buffer_size = 30,
      from = get_user(),
      cc = NULL,
      bcc = NULL,
      html = FALSE,
      filters = NULL
    ){
      assert_namespace("gmailr")

      private$initialize_buffer(buffer_size)
      self$set_flush_on_rotate(FALSE)
      self$set_flush_on_exit(FALSE)
      self$set_filters(filters)

      self$set_to(to)
      self$set_from(from)
      self$set_cc(cc)
      self$set_bcc(bcc)
      self$set_html(html)
      self$set_subject_layout(subject_layout)

      self$set_layout(layout)
      self$set_threshold(threshold)
      self$set_flush_threshold(flush_threshold)
      self$set_buffer_size(buffer_size)

      private$.flush_on_exit   <- FALSE
      private$.flush_on_rotate <- FALSE
    },

    flush = function(
    ){
      assign("insert_pos", 0L, envir = private)

      body <- paste(
        unlist(lapply(self$buffer_events, self$layout$format_event)),
        collapse = "\n"
      )
      le    <- self$buffer_events[[length(self$buffer_events)]]
      title <- self$subject_layout$format_event(le)
      title <- try(
        iconv(title, from = "UTF-8", to = "ASCII//TRANSLIT"),
        silent = TRUE
      )

      mail <- gmailr::mime()
      mail <- gmailr::to(mail, self$to)
      mail <- gmailr::from(mail, self$from)
      mail <- gmailr::cc(mail, self$cc)
      mail <- gmailr::bcc(mail, self$bcc)
      mail <- gmailr::subject(mail, title)

      if (self$html){
        mail <- gmailr::html_body(mail, paste0("<pre>\n", body, "</pre>\n"))
      } else {
        mail <- gmailr::text_body(mail, body)
      }

      gmailr::send_message(mail)
      private$.buffer_events <- list()

      invisible(self)
    }
  ),


  # +- active ---------------------------------------------------------------

  private = list(
    .to = NULL,
    .from = NULL,
    .cc = NULL,
    .bcc = NULL,
    .html = NULL
  )
)
