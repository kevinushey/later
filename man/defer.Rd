% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defer.R
\name{defer}
\alias{defer}
\alias{defer_parent}
\title{Defer Evaluation of an Expression}
\usage{
defer(expr, envir = parent.frame(), priority = c("first", "last"))

defer_parent(expr, priority = c("first", "last"))
}
\arguments{
\item{expr}{An expression to be evaluated.}

\item{envir}{Attach exit handlers to this environment.
Typically, this should be either the current environment or
a parent frame (accessed through \code{\link{parent.frame}()}).}

\item{priority}{Specify whether this handler should be executed
\code{"first"} or \code{"last"}, relative to any other registered
handlers on this environment.}
}
\description{
Similar to \code{\link{on.exit}()}, but allows one to attach
an expression to be evaluated when exitting any frame currently
on the stack. This provides a nice mechanism for scoping side
effects for the duration of a function's execution.
}
\details{
\code{defer} works by attaching handlers to the requested environment (as an
attribute called \code{"handlers"}), and registering an exit handler that
executes the registered handler when the function associated with the
requested environment finishes execution.
}
\examples{
# define a 'scope' function that creates a file, and
# removes it when the parent function has finished executing
scope_file <- function(path) {
  file.create(path)
  defer_parent(unlink(path))
}

# create tempfile path
path <- tempfile()

# use 'scope_file' in a function
local({
  scope_file(path)
  stopifnot(file.exists(path))
})

# file is deleted as we leave 'local' scope
stopifnot(!file.exists(path))

# investigate how 'defer' modifies the
# executing function's environment
local({
  scope_file(path)
  print(attributes(environment()))
})
}
\seealso{
Other scope-related functions: \code{\link{scope_dir}},
  \code{\link{scope_env_vars}}, \code{\link{scope_locale}},
  \code{\link{scope_options}}, \code{\link{scope_path}}
}

