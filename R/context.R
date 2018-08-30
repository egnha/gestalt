#' Run an Action in an Ordered Context
#'
#' @description
#' Programming in R typically involves:
#'
#'   1. Making a context—assigning values to names.
#'
#'   2. Performing an action—evaluating an expression relative to a context.
#'
#' `let()` and `run()` enable you to treat these procedures as reusable,
#' _composable_ components.
#'
#'   - `let()` makes a **context**: it _lazily_ binds a sequence of ordered
#'     named expressions to a child of a given environment (by default, the
#'     current one).
#'
#'     For instance, in an environment `env` where `z` is in scope,
#'     ```
#'       let(env, x = 1, y = x + 2, z = x * y * z)
#'     ```
#'     is equivalent to calling
#'     ```
#'       local({
#'         x <- 1
#'         y <- x + 2
#'         z <- x * y * z
#'         environment()
#'       })
#'     ```
#'     except `let()` binds the named expressions lazily (as
#'     [promises][delayedAssign()]) and comprehends tidyverse
#'     [quasiquotation][rlang::quasiquotation].
#'
#'   - `run()` performs an **action**: it evaluates an expression relative to an
#'     environment (by default, the current one) and, optionally, a sequence of
#'     _lazily evaluated_ ordered named expressions.
#'
#'     For instance, in an environment `env` where `x` is in scope,
#'     ```
#'       run(env, x + y + z, y = x + 2, z = x * y * z)
#'     ```
#'     is equivalent to calling
#'     ```
#'       local({
#'         y <- x + 2
#'         z <- x * y * z
#'         x + y + z
#'       })
#'     ```
#'     except `run()`, like `let()`, binds `y` and `z` lazily and comprehends
#'     quasiquotation.
#'
#' @param _data Context of named values, namely an environment, list or data
#'   frame; if a list or data frame, it is interpreted as an environment (like
#'   the `envir` argument of [eval()]).
#' @param `_expr` Expression to evaluate (“run”). Quasiquotation is supported.
#' @param ... Named expressions. An expression looks up values to the left of
#'   it, and takes precedence over those in `` `_data` ``.
#'   [Quasiquotation][rlang::quasiquotation] of names and expressions is
#'   supported (see ‘Examples’).
#'
#' @return `run()` returns the evaluation of `` `_expr` `` in the combined
#'   environment of `` `_data` `` and `...`.
#'
#'   `let()` returns an environment where the bindings in `...` are in scope, as
#'   [promises][delayedAssign()], as if they were assigned from left to right in
#'   a child of the environment defined by `` `_data` ``.
#'
#' @section Composing Contexts:
#'   **Contexts**, as made by `let()`, have an advantage over ordinary local
#'   assignments because contexts are both lazy and composable. Like
#'   assignments, the order of named expressions in a context is significant.
#'
#'   For example, you can string together contexts to make larger ones:
#'   ```
#'     foo <-
#'       let(a = ., b = a + 2) %>>>%
#'       let(c = a + b) %>>>%
#'       run(a + b + c)
#'
#'     foo(1)
#'     #> [1] 8
#'   ```
#'   Earlier bindings can be overriden by later ones:
#'   ```
#'     bar <-
#'       foo[1:2] %>>>%        # Collect the contexts of 'foo()'
#'       let(c = c - 1) %>>>%  # Override 'c'
#'       run(a + b + c)
#'
#'     bar(1)
#'     #> [1] 7
#'   ```
#'   Bindings are [promises][delayedAssign()]—they are only evaluated on demand:
#'   ```
#'     run(let(x = a_big_expense(), y = "avoid a big expense"), y)
#'     #> [1] "avoid a big expense"
#'   ```
#'
#' @section Remark:
#'   “Contexts” as described here should not be confused with “contexts” in
#'   [R’s internal mechanism](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Contexts).
#'
#' @seealso [with()] is like `run()`, but more limited because it doesn't
#'   support quasiquotation or provide a means to override local bindings.
#'
#' @examples
#' # Miles-per-gallon of big cars
#' mtcars$mpg[mtcars$cyl == 8 & mtcars$disp > 350]
#' run(mtcars, mpg[cyl == 8 & disp > 350])
#' run(mtcars, mpg[big_cars], big_cars = cyl == 8 & disp > 350)
#'
#' # 'let()' makes a reusable local context for big cars
#' cars <- let(mtcars, big = cyl == 8 & disp > 350)
#'
#' eval(quote(mpg[big]), cars)  # Quoting restricts name lookup to 'cars'
#' run(cars, mpg[big])          # The same, but shorter and more transparent
#'
#' run(cars, wt[big])
#' mtcars$wt[mtcars$cyl == 8 & mtcars$disp > 350]
#'
#' # Precedence of names is from right to left (“bottom-up”):
#' a <- 1000
#' run(`_expr` = a + b, a = 1, b = a + 2)    # 4: all references are local
#' run(list(a = 1), a + b, b = a + 2)        # 4: 'b' references local 'a'
#' run(let(a = 1, b = a + 2), a + b)         # 4: 'b' references local 'a'
#' run(let(a = 1, b = a + 2), a + b, a = 0)  # 3: latter 'a' takes precedence
#' run(list(a = 1, b = a + 2), a + b)        # 1003: 'b' references global 'a'
#'
#' # Bound expressions are lazily evaluated: no error unless 'x' is referenced
#' run(`_expr` = "S’all good, man.", x = stop("!"))
#' run(let(x = stop("!")), "S’all good, man.")
#' let(x = stop("!"))    # Environment binding 'x'
#' \donttest{let(x = stop("!"))$x  # Error: !}
#'
#' # Quasiquotation is supported
#' a <- 1
#' run(let(a = 2), a + !!a)               #> [1] 3
#' run(let(a = 1 + !!a, b = a), c(a, b))  #> [1] 2 2
#'
#' @name context
NULL

#' @rdname context
#' @export
let <- local({
  assign_setter("env_top", "expr_promises")

  function(`_data` = parent.frame(), ...) {
    if (!is.environment(`_data`))
      `_data` <- evalq(environment(), `_data`, parent.frame())
    exprs <- exprs(...)
    cxt <- as_ordered_promises(`_data`, exprs)
    env_top(cxt) <- env_top(`_data`) %||% `_data`
    expr_promises(cxt) <- c(expr_promises(`_data`), exprs)
    class(cxt) <- c("Context", "environment")
    cxt
  }
})

assign_getter("env_top", "expr_promises")

as_ordered_promises <- function(env, exprs) {
  all(nzchar(names(exprs))) %because% "Expressions must be named"
  for (i in seq_along(exprs))
    env <- bind_as_promise(exprs[i], env)
  env
}

bind_as_promise <- function(expr, parent) {
  env <- new.env(parent = parent)
  do.call(delayedAssign, list(names(expr), expr[[1L]], parent, env))
  env
}

#' @export
names.Context <- function(x) {
  names(expr_promises(x))
}

#' @importFrom utils capture.output
#' @export
print.Context <- function(x, ...) {
  cat("<Ordered Context>\n")
  cat("\n* Topmost environment:\n")
  top <- capture.output(print(env_top(x)))
  cat("\ \ ", top, "\n", sep = "")
  cat("\n* Named expressions (resolved from the bottom up):")
  exprs <- expr_promises(x)
  nms <- names(exprs)
  for (i in seq_along(exprs)) {
    expr <- paste(capture.output(print(exprs[[i]])), collapse = "\n")
    cat("\n\ \ ", nms[[i]], ": ", expr, sep = "")
  }
  invisible(x)
}

#' @rdname context
#' @export
run <- function(`_data` = parent.frame(), `_expr`, ...) {
  eval(enexpr(`_expr`), let(`_data` = `_data`, ...))
}
