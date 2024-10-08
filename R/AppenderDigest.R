  # AppenderDigest --------------------------------------------------------

#' Abstract class for digests (multi-log message notifications)
#'
#' @description
#' Digests is an abstract class for report-like output that contain several
#' log messages and a title; e.g. an E-mail containing the last 10 log messages
#' before an error was encountered or a push notification.
#'
#' @template abstract_class
#'
#' @export
#' @seealso [lgr::LayoutFormat], [lgr::LayoutGlue]
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
    }
  ),

  active = list(
    #' @field subject_layout A [lgr::Layout] used to format the last [lgr::LogEvent]
    #'   in this Appenders buffer when it is flushed. The result will be used as
    #'   the subject of the digest (for example, the E-mail subject).
    subject_layout = function() get(".subject_layout", private)
  ),

  private = list(
    .subject_layout = NULL
  )
)




# AppenderPushbullet --------------------------------------------------------


#' Send push-notifications via RPushbullet
#'
#' @description
#' Send push notifications via [Pushbullet](https://www.pushbullet.com/). This
#' Appender keeps an in-memory buffer like [lgr::AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message that is sent to
#' [RPushbullet::pbPost()]. The default behavior is to push the last 7 log
#' events in case a `fatal` event is encountered.
#'
#' @template appender
#' @family Digest Appenders
#' @seealso [lgr::LayoutFormat], [lgr::LayoutGlue]
#' @export
#' @examples
#' if (requireNamespace("RPushbullet") && !is.null(getOption("rpushbullet.key")) ){
#'   app <- AppenderPushbullet$new()
#'
#'   lg <- lgr::get_logger("test/dbi")$
#'     add_appender(app, "pb")$
#'     set_propagate(FALSE)
#'
#'   lg$fatal("info")
#'   lg$fatal("test")
#'
#'  invisible(lg$config(NULL))
#' }
AppenderPushbullet <- R6::R6Class(
  "AppenderPushbullet",
  inherit = AppenderDigest,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
  #' @param recipients,email,channel,devices,apikey see [RPushbullet::pbPost]
  #' @param threshold,flush_threshold,layout,buffer_size see [lgr::AppenderBuffer]
  #' @param subject_layout A [lgr::LayoutFormat] object.
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
      apikey = getOption("rpushbullet.key"),
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
      self$set_should_flush(NULL)

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

#' Abstract class for email Appenders
#'
#' @template abstract_class
#'
#' @family Digest Appenders
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

    #' @field to `character` vector. The email addresses of the recipient
    to = function() get(".to", envir = private),

    #' @field from `character` vector. The email address of the sender
    from = function() get(".from", envir = private),

    #' @field cc `character` vector. The email addresses of the cc-recipients (carbon copy)
    cc = function() get(".cc", envir = private),

    #' @field bcc `character` vector. The email addresses of bcc-recipients (blind carbon copy)
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
#' @description
#' Send mails via [sendmailR::sendmail()], which requires that you have access
#' to an SMTP server that does not require authentication. This
#' Appender keeps an in-memory buffer like [lgr::AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message. The default behavior
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
#' @template appender
#' @seealso [lgr::LayoutFormat], [lgr::LayoutGlue]
#' @family Digest Appenders
#' @export
#' @examples
#' if (requireNamespace("sendmailR")){
#' # requires that you have access to an SMTP server
#'
#'   lg <- lgr::get_logger("lgrExtra/test/mail")$
#'     set_propagate(FALSE)$
#'     add_appender(AppenderSendmail$new(
#'       from = "ceo@ecorp.com",
#'       to = "some.guy@ecorp.com",
#'     control = list(smtpServer = "mail.somesmptserver.com")
#'   ))
#'   # cleanup
#'   invisible(lg$config(NULL))
#' }
AppenderSendmail <- R6::R6Class(
  "AppenderSendmail",
  inherit = AppenderMail,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(
    #' @description
    #' see [AppenderMail] for details
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
      self$set_should_flush(NULL)
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


#' Send emails via the Gmail REST API
#'
#' @description
#' Send mails via [gmailr::gm_send_message()]. This
#' Appender keeps an in-memory buffer like [lgr::AppenderBuffer]. If the buffer is
#' flushed, usually because an event of specified magnitude is encountered, all
#' buffered events are concatenated to a single message. The default behavior
#' is to push the last 30 log events in case a `fatal` event is encountered.
#'
#' **NOTE:** This Appender requires that you set up google API authorization,
#' please refer to the [documentation of gmailr](https://github.com/r-lib/gmailr)
#' for details.
#'
#' @template appender
#' @seealso [lgr::LayoutFormat], [lgr::LayoutGlue]
#' @export
AppenderGmail <- R6::R6Class(
  "AppenderGmail",
  inherit = AppenderMail,
  cloneable = FALSE,

  # +- public --------------------------------------------------------------
  public = list(

    #' @description see [AppenderMail] for details
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
      self$set_should_flush(NULL)

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

      mail <- gmailr::gm_mime()
      mail <- gmailr::gm_to(mail, self$to)
      mail <- gmailr::gm_from(mail, self$from)
      mail <- gmailr::gm_cc(mail, self$cc)
      mail <- gmailr::gm_bcc(mail, self$bcc)
      mail <- gmailr::gm_subject(mail, title)

      if (self$html){
        mail <- gmailr::gm_html_body(mail, paste0("<pre>\n", body, "</pre>\n"))
      } else {
        mail <- gmailr::gm_text_body(mail, body)
      }

      gmailr::gm_send_message(mail)
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
