make_call <- function(...) {
  as.call(list(...))
}

ensure_all_named <- function(object, msg) {

  if (missing(msg))
    msg <- "all objects must be named"

  if (is.null(names(object)))
    stop(msg, call. = FALSE)

  if (!all(nzchar(names(object))))
    stop(msg, call. = FALSE)

  TRUE
}
