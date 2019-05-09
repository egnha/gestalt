fn_constructor <- function(get_exprs, make_fn) {
  force(get_exprs)
  force(make_fn)

  function(..., ..env = parent.frame()) {
    is.environment(..env) %because% "'..env' must be an environment"
    fun <- fn_parts(get_exprs(...))
    make_fn(fun$args, fun$body, ..env)
  }
}

fn_parts <- function(xs) {
  n <- length(xs)
  validate(xs, n)
  args <- get_args(xs[-n])
  remains <- behead(xs[n])
  list(args = c(args, remains$head), body = remains$body)
}

validate <- function(xs, n = length(xs)) {
  (n > 0L) %because%
    "Function must be declared"
  is_fml <- vapply(xs, is_formula, TRUE)
  all(!is_fml[-n]) %because%
    "Only the body (as last argument) should be a formula"
  is_fml[n] %because%
    "Final argument must be a formula (specifying the body)"
  invisible(xs)
}

get_args <- function(xs) {
  if (is_empty(xs))
    return(NULL)
  no_name <- !nzchar(names(xs))
  names(xs)[no_name] <- vapply(xs[no_name], expr_name, "")
  xs[no_name] <- blank
  xs
}

behead <- function(x) {
  nm <- names(x)
  fml <- x[[1L]]
  if (is_onesided(fml)) {
    (nm == "") %because% "Default value of final argument expected"
    head <- NULL
  } else {
    head <- nonempty_head(fml, nm)
  }
  list(head = head, body = f_rhs(fml))
}
nonempty_head <- function(fml, nm) {
  expr <- f_lhs(fml)
  if (nzchar(nm))
    return(list(expr) %named% nm)
  blank %named% expr_name(expr)
}

make_function <- function(args, body, env) {
  if (is_closure(body)) {
    body <- call("function", formals(body), base::body(body))
  } else {
    is_expression(body) %because% "Body must be an expression or closure"
  }
  new_fn(args, body, env)
}

blank <- list(quote(expr = ))

#' Raw quotation of expressions
#'
#' `literal_tidy()` is an extension of [rlang::exprs()] that comprehends literal
#' unquoting operators: `QUQ()`, `QUQS()` are substituted as `` `!!`() ``,
#' `` `!!!`() ``, resp.
#'
#' @noRd
literal_tidy <- function(...) {
  lapply(quos(...), quo_get_expr_)
}

#' Function Declarations with Quasiquotation
#'
#' @description
#' `fn()` enables you to create (anonymous) functions, of arbitrary call
#' signature. Use it in place of the usual [function()] invocation whenever you
#' want to:
#'
#' - **Be concise**: The function declarations
#'   ```
#'     fn(x, y = 1 ~ x + y)
#'
#'     function(x, y = 1) x + y
#'   ```
#'   are equivalent.
#'
#' - **Enforce immutability**: By enabling Tidyverse
#'   [quasiquotation][rlang::quasiquotation], `fn()` allows you to \dQuote{burn
#'   in} values at the point of function creation. This guards against changes
#'   in a function's enclosing environment. (See \sQuote{Use Unquoting to Make
#'   Robust Functions}.)
#'
#' @param ... Function declaration, which supports
#'   [quasiquotation][rlang::quasiquotation].
#' @param ..env Environment in which to create the function (i.e., the
#'   function's [enclosing environment][base::environment]).
#'
#' @return A function whose enclosing environment is `..env`.
#'
#' @section Function Declarations: A **function declaration** is an expression
#'   that specifies a function's arguments and body, as a comma-separated
#'   expression of the form
#'   ```
#'     arg1, arg2, ..., argN ~ body
#'   ```
#'   or
#'   ```
#'     arg1, arg2, ..., argN, ~body
#'   ```
#'   (Note in the second form that the body is a one-sided formula. This
#'   distinction is relevant for argument [splicing][rlang::quasiquotation], see
#'   \sQuote{Quasiquotation}.)
#'
#'   * To the left of `~`, you write a conventional function-argument
#'     declaration, just as in `function(<arguments>)`: each of `arg1`, `arg2`,
#'     \dots, `argN` is either a bare argument (e.g., `x` or `...`) or an
#'     argument with default value (e.g., `x = 1`).
#'
#'   * To the right of `~`, you write the function body, i.e., an expression of
#'     the arguments.
#'
#' @section Quasiquotation:
#'   All parts of a function declaration support Tidyverse
#'   [quasiquotation][rlang::quasiquotation]:
#'
#'   * To unquote values (of arguments or parts of the body), use `!!`:
#'     ```
#'       z <- 0
#'       fn(x, y = !!z ~ x + y)
#'       fn(x ~ x > !!z)
#'     ```
#'
#'   * To unquote argument names (with default value), use `:=` (definition
#'     operator):
#'     ```
#'       arg <- "y"
#'       fn(x, !!arg := 0 ~ x + !!as.name(arg))
#'     ```
#'
#'   * To splice in a (formal) list of arguments, use `!!!`:
#'     ```
#'       # NB: Body is a one-sided formula
#'       fn(!!!alist(x, y = 0), ~ x + y)
#'     ```
#'     Splicing allows you to treat a complete function declaration as a unit:
#'     ```
#'       soma <- alist(x, y = 0, ~ x + y)
#'       fn(!!!soma)
#'     ```
#'
#'   * To write literal unquoting operators, use `QUQ()`, `QUQS()`, which read
#'   as \dQuote{quoted unquoting,} \dQuote{quoted unquote-splicing,} resp. (cf.
#'   `fn_()`):
#'     ```
#'       library(dplyr)
#'
#'       my_summarise <- fn(df, ... ~ {
#'         group_by <- quos(...)
#'         df %>%
#'           group_by(QUQS(group_by)) %>%
#'           summarise(a = mean(a))
#'       })
#'     ```
#'     (Source: [Programming with
#'     dplyr](http://dplyr.tidyverse.org/articles/programming.html))
#'
#' @section Use Unquoting to Make Robust Functions:
#'   Functions in R are generally
#'   [impure](https://en.wikipedia.org/wiki/Pure_function), i.e., the return
#'   value of a function will _not_ in general be determined by the value of its
#'   inputs alone. This is because, by design, a function may depend on objects
#'   in its
#'   [lexical scope](http://adv-r.hadley.nz/functions.html#lexical-scoping), and
#'   these objects may mutate between function calls. Normally this isn't a
#'   hazard.
#'
#'   However, if you are working interactively and sourcing files into the
#'   global environment, or using a notebook interface like
#'   [Jupyter](https://jupyter.org) or
#'   [R Notebook](http://rmarkdown.rstudio.com/r_notebooks.html), it can be
#'   tricky to ensure that you haven't unwittingly mutated an object that an
#'   earlier function depends upon.
#'
#'   You can use unquoting to guard against such mutations.
#'
#'   \subsection{Example}{
#'   Consider the following function:
#'   ```
#'     a <- 1
#'     foo <- function(x) x + a
#'   ```
#'   What is the value of `foo(1)`? It is not necessarily `2`, because the value
#'   of `a` may have changed between the _creation_ of `foo()` and the _calling_
#'   of `foo(1)`:
#'   ```
#'     foo(1)  #> [1] 2
#'
#'     a <- 0
#'
#'     foo(1)  #> [1] 1
#'   ```
#'   In other words, `foo()` is impure because the value of `foo(x)` depends not
#'   only on the value of `x` but also on the _externally mutable_ value of `a`.
#'
#'   With `fn()`, you can unquote `a` to \dQuote{burn in} its value at the point
#'   of creation:
#'   ```
#'     a <- 1
#'     foo <- fn(x ~ x + !!a)
#'   ```
#'   Now `foo()` is a pure function, unaffected by changes to `a` in the lexical
#'   scope:
#'   ```
#'     foo(1)  #> [1] 2
#'
#'     a <- 0
#'
#'     foo(1)  #> [1] 2
#'   ```
#'   }
#'
#' @examples
#' fn(x ~ x + 1)
#' fn(x, y ~ x + y)
#' fn(x, y = 2 ~ x + y)
#' fn(x, y = 1, ... ~ log(x + y, ...))
#'
#' # to specify '...' in the middle, write '... = '
#' fn(x, ... = , y ~ log(x + y, ...))
#'
#' # use one-sided formula for constant functions or commands
#' fn(~ NA)
#' fn(~ message("!"))
#'
#' # unquoting is supported (using `!!` from rlang)
#' zero <- 0
#' fn(x = !!zero ~ x > !!zero)
#'
#' # formals and function bodies can also be spliced in
#' f <- function(x, y) x + y
#' g <- function(y, x, ...) x - y
#' frankenstein <- fn(!!!formals(f), ~ !!body(g))
#' stopifnot(identical(frankenstein, function(x, y) x - y))
#' \donttest{
#' # mixing unquoting and literal unquoting is possible
#' library(dplyr)
#'
#' summariser <- quote(mean)
#'
#' my_summarise <- fn(df, ... ~ {
#'   group_by <- quos(...)
#'   df %>%
#'     group_by(QUQS(group_by)) %>%
#'     summarise(a = `!!`(summariser)(a))
#' })
#' my_summarise}
#'
#' @export
fn <- fn_constructor(literal_tidy, make_function)

literal <- function(...) {
  exprs <- as.list(substitute(...()))
  exprs %named% names_chr(exprs)
}

#' @description
#'  `fn_()` is a variant of `fn()` that does _not_ comprehend quasiquotation. It
#'  is useful when you want unquoting (`` `!!` ``) or splicing (`` `!!!` ``)
#'  operators in the function body to be literally interpreted, rather than
#'  immediately invoked. (See \sQuote{Quasiquotation} for a complementary way to
#'  literally interpret unquoting and splicing operators in `fn()`.)
#'
#' @examples
#' # Use fn_() with fn() as a concise way to force ("pin down") bindings
#' # For example, the 'x' is immutable in the function produced by call_upon():
#' call_upon <- fn_(x ~ fn(f ~ f(!!x)))
#' sapply(list(sin, cos), call_upon(0))  # [1] 0 1
#'
#' # Return-value checking, as a functional transformation
#' enforce <- fn_(condition ~
#'   fn(x ~ {
#'     stopifnot(!!substitute(condition))
#'     x
#'   })
#' )
#' no_nan <- enforce(!is.nan(x))
#' \donttest{
#' log_strict <- fn(x ~ no_nan(log(x)))
#' log_strict(2)   # [1] 0.6931472
#' log_strict(-1)  # Error: !is.nan(x) is not TRUE}
#'
#' @rdname fn
#' @export
fn_ <- fn_constructor(literal, make_function)
