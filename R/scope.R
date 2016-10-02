#' Scope to Directory
#'
#' Sets the working directory, and resets it at the end
#' of the active scope.
#'
#' @param directory The working directory to use.
#' @family scope-related functions
#' @export
scope_dir <- function(directory) {
  owd <- setwd(directory)
  defer_parent(setwd(owd))
}

#' Scope with Environment Variables
#'
#' Set environment variables for the current scope, and resets it
#' at the end of the active scope.
#'
#' @param ... Named arguments, mapping environment variable
#'   names to values.
#' @family scope-related functions
#' @export
scope_env_vars <- function(...) {

  dots <- list(...)
  ensure_all_named(dots, "all arguments must be named")

  old <- as.list(Sys.getenv(names(dots), unset = NA, names = TRUE))

  na <- is.na(old)
  to_restore <- old[!na]
  to_unset <- old[na]

  Sys.setenv(...)

  defer_parent({
    Sys.unsetenv(names(to_unset))
    do.call(Sys.setenv, to_restore)
  })

}

#' Scope with R Options
#'
#' Set \R options (through \code{options}) and restore at the
#' end of the active scope.
#'
#' @param ... Named arguments, mapping option names to values.
#' @family scope-related functions
#' @export
scope_options <- function(...) {

  dots <- list(...)
  ensure_all_named(dots, "all arguments must be named")

  opts <- as.list(do.call(base::options, as.list(names(dots))))

  options(...)
  defer_parent(do.call(base::options, opts))

}

#' Scope with Locale
#'
#' Set aspects of the current locale and restore the original locale at the
#' end of the active scope. See \code{\link{Sys.getlocale}()} and
#' \code{\link{Sys.setlocale}()} for more information.
#'
#' @param category The locale aspect, as a character string.
#' @param locale The locale name.
#'
#' @family scope-related functions
#' @export
scope_locale <- function(category = "LC_COLLATE", locale) {
  original <- Sys.getlocale(category = category)
  Sys.setlocale(category = category, locale = locale)
  defer_parent(Sys.setlocale(category = category, locale = original))
}
