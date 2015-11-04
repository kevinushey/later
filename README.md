# Later -- Scoped Side Effects for R

[![Travis-CI Build Status](https://travis-ci.org/kevinushey/later.svg?branch=master)](https://travis-ci.org/kevinushey/later)

Generalizes `on.exit()` to a function `defer()`, which can attach exit handlers
to any currently executing function, and uses that to scope side effects.

## Example

```r
list_files <- function(dir) {
  scope_dir(dir)
  list.files()
}
```

This function will enter the directory `dir`, list any files, and then
automatically return to the previously set working directory at the end of the
function's execution.

## Inspiration & Credit

This package is heavily inspired by Jim Hester's
[withr](https://github.com/jimhester/withr) package.

The main 'trick' that enabled the generalization of `on.exit()` was provided in
a mailing list post
here, by Peter Meilstrup: https://stat.ethz.ch/pipermail/r-devel/2013-November/067874.html
