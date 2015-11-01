# Later -- Scoped Side Effects for R Functions

[![Travis-CI Build Status](https://travis-ci.org/kevinushey/later.svg?branch=master)](https://travis-ci.org/kevinushey/later)

This package:

- generalizes `on.exit()` to a function `defer()`, which can attach exit handlers
  to any currently executing function,

- uses `defer()` to provide a number of 'scoped' functions: for example,
  `scope_dir()` sets the current working directory, and then unsets it
  after the active function has finished execution.

For example:

```r
list_files <- function(dir) {
  scope_dir(dir)
  list.files()
}
```

This function will enter the directory `dir`, list any files, and then
automatically return to the previously set working directory at the end of the
function's execution.
