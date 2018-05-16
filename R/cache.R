#' Cache the value of a void function call
#'
#' `cache()` is a convenience helper that transforms a function into a function
#' that caches the value of its _void call_, i.e., `cache()`
#' [memoizes](https://en.wikipedia.org/wiki/Memoization) void functions. Use
#' `cache()` when you want to treat a computed value as a _constant_ with
#' \dQuote{structure,} namely the computation that produced it.
#'
#' @param f Function that can be called without arguments. (NB: `cache()` itself
#'   does not check whether `f()` is indeed a valid call.)
#'
#' @return Function without formal arguments that returns the (cached) value of
#'   the void call `f()`.
#'
#' @seealso \code{\link[=compose]{\%>>>\%}}
#'
#' @examples
#' # Function with constant return value
#' val <- {message("Computing from scratch"); mtcars} %>>>%
#'   split(.$cyl) %>>>%
#'   lapply(function(data) lm(mpg ~ wt, data)) %>>>%
#'   lapply(summary) %>>>%
#'   sapply(`[[`, "r.squared")
#'
#' # Caching `val` ensures that its value is computed only once.
#' # On subsequent calls, the computed value is simply fetched:
#' val_cached <- cache(val)
#' val_cached()
#' val_cached()
#' val()
#' val()
#'
#' # As values, `val()` and `val_cached()` are identical.
#' # But `val_cached()` also has structure, namely the function `val_cached`:
#' val_cached
#'
#' # For instance, you can inspect the intermediate summary:
#' head(val_cached, -1)()
#'
#' \dontrun{
#' # You can think of `%>>>%` combined with `cache()` as a lazy, structured
#' # alternative to the magrittr `%>%`.
#'
#' require(magrittr)
#'
#' val2 <- mtcars %>%
#'   split(.$cyl) %>%
#'   lapply(function(data) lm(mpg ~ wt, data)) %>%
#'   lapply(summary) %>%
#'   sapply(`[[`, "r.squared")
#'
#' # `val2` and `val()` are identical values. But in contrast to `val_cached()`,
#' # the “structure” of `val2` is only implicit in source code.
#' stopifnot(identical(val2, val_cached()))}
#'
#' @export
cache <- local({
  cached <- function() {
    if (was_called)
      return(value)
    was_called <<- TRUE
    value <<- f()
    value
  }
  cache <- list(value = NULL, was_called = FALSE)

  function(f) {
    environment(cached) <- envir(f) %encloses% c(f = f, cache)
    attributes(cached) <- attributes(f)
    class(cached) <- "CachedVoidFunction" %subclass% class(f)
    cached
  }
})

#' @export
print.CachedVoidFunction <- function(x, ...) {
  cat("<Cached Void Function>\n")
  NextMethod()
  invisible(x)
}
