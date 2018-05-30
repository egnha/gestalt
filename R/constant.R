#' Values as Functions
#'
#' @description
#' A **constant** is a fixed value that incorporates its very computation. This
#' is none other than a _function_ that computes a fixed value when called
#' without arguments. `constant()` declares such a function as a bona fide
#' constant by transforming it to a function that caches the value of its void
#' call (i.e., `constant()`
#' [memoizes](https://en.wikipedia.org/wiki/Memoization) void functions).
#'
#' Combine ``\link[=compose]{`%>>>%`}`` with `constant()` for a _lazy_,
#' _structured_ alternative to the
#' [\pkg{magrittr}](https://cran.r-project.org/package=magrittr) `` `%>%` ``
#' (see ‘Examples’).
#'
#' @param f Function, or symbol or name (string) thereof, that can be called
#'   without arguments. (NB: `constant()` itself does not check whether `f()` is
#'   indeed a valid call.)
#'
#' @return `constant()` yields a function without formal arguments that returns
#'   the (cached, visibility-preserving) value of the void call `f()`.
#'
#' @seealso ``\link[=compose]{`%>>>%`}``
#'
#' @examples
#' # Function with a constant return value
#' val <- {message("Computing from scratch"); mtcars} %>>>%
#'   split(.$cyl) %>>>%
#'   lapply(function(data) lm(mpg ~ wt, data)) %>>>%
#'   lapply(summary) %>>>%
#'   sapply(`[[`, "r.squared")
#'
#' # With every invocation, `val()` is computed anew:
#' val()
#' val()
#'
#' # Declaring `val` as a constant ensures that its value is computed only once.
#' # On subsequent calls, the computed value is simply fetched:
#' const <- constant(val)
#' const()
#' const()
#'
#' # As values, `val()` and `const()` are identical. But `const()`, moreover,
#' # has structure, namely the function `const`:
#' const
#'
#' # For instance, you can inspect the intermediate summaries:
#' head(const, -1)()
#'
#' # Which can itself be a constant:
#' summ <- constant(head(const, -1))
#' summ()
#' summ()
#'
#' \dontrun{
#' # Think of `%>>>%` combined with `constant()` as a lazy, structured
#' # alternative to the magrittr `%>%`.
#' library(magrittr)
#'
#' val2 <- mtcars %>%
#'   split(.$cyl) %>%
#'   lapply(function(data) lm(mpg ~ wt, data)) %>%
#'   lapply(summary) %>%
#'   sapply(`[[`, "r.squared")
#'
#' # `val2` and `const()` are identical values. But whereas `val2` is computed
#' # immediately and carries no structure, `const` embodies the process that
#' # produces its value, and allows you to defer its realization to the
#' # invocation `const()`.
#' stopifnot(identical(val2, const()))}
#'
#' @export
constant <- local({
  const <- function() {
    if (is.null(`__const__`)) {
      res <- withVisible(`__value__`())
      val <- .subset2(res, "value")
      if (.subset2(res, "visible"))
        `__const__` <<- function() val
      else
        `__const__` <<- function() invisible(val)
    }
    `__const__`()
  }

  function(f) {
    f <- match.fun(f)
    if (inherits(f, "ConstantFunction"))
      return(f)
    environment(const) <- envir(f) %encloses% list(
      `__value__` = f,
      `__const__` = NULL
    )
    attributes(const) <- attributes(f)
    class(const) <- "ConstantFunction" %subclass% class(f)
    const
  }
})

#' @rdname constant
#'
#' @return `variable()` is the inverse transformation of `constant()`: it
#'   recovers the underlying (uncached) function of a constant function.
#'
#' @examples
#' # Use `variable()` to recover the original (“variable”) function
#' val_var <- variable(const)
#' stopifnot(identical(val_var, val))
#' val_var()
#' val_var()
#'
#' @export
variable <- local({
  get_variable <- getter("__value__")

  function(f) {
    f <- match.fun(f)
    get_variable(f) %||% f
  }
})

#' @export
print.ConstantFunction <- function(x, ...) {
  cat("Constant Function:\n")
  NextMethod()
  invisible(x)
}
