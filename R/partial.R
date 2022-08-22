#' Fix a Number of Arguments to a Function
#'
#' @description
#' `partial()` enables
#' [partial application](https://en.wikipedia.org/wiki/Partial_application):
#' given a function, it fixes the value of selected arguments to produce a
#' function of the remaining arguments.
#'
#' `departial()` undoes the application of `partial()` by returning the original
#' function.
#'
#' @param ..f Function.
#' @param ... Argument values of `..f` to fix, specified by name or position.
#'   Captured as [quosures][rlang::quotation].
#'   [Unquoting][rlang::quasiquotation] and [splicing][rlang::quasiquotation]
#'   are supported (see \sQuote{Examples}). Argument values may match the `...`
#'   argument of `..f` (if present), but only when specified by name.
#'
#' @return `partial()` returns a function whose [formals][base::formals()] are a
#'   truncation of the formals of `..f` (as a closure) by the fixed arguments.
#'   NB the original default values do not appear in the formals of a
#'   partialized function, but are nonetheless applied when the function is
#'   called.
#'
#'   The function `partial(..f)` is identical to `..f`.
#'
#'   In conformance with R's calling convention, fixed argument values are lazy
#'   [promises][base::delayedAssign()]. Moreover, when forced, they are [tidily
#'   evaluated][rlang::eval_tidy()]. Lazy evaluation of fixed arguments can be
#'   overridden via unquoting, see \sQuote{Examples}.
#'
#'   When `..f` is a partially applied function, `departial(..f)` is the
#'   (closure of) the underlying function. For ordinary (non-partially applied)
#'   functions, `departial(..f)` is identical to `..f`.
#'
#' @details
#'   Even while `partial()` truncates formals, it remains compatible with
#'   functions that use [`missing()`][base::missing()] to test whether a
#'   specified argument was supplied in a call. For example,
#'   `draw3 <- partial(sample, size = 3)` works as a function that randomly
#'   draws three elements, even though `sample()` invokes `missing(size)` and
#'   `draw3()` has the form `function(x, replace, prob) {...}`.
#'
#'   Because partially applied functions call the original function in an ad hoc
#'   environment, impure functions that depend on the calling context as a
#'   _value_, rather than as a lexical scope, may not be amenable to
#'   `partial()`. For example, `partial(ls, all.names = TRUE)()` is not
#'   equivalent to `ls(all.names = TRUE)`, because `ls()` inspects the calling
#'   environment to produce its value, whereas `partial(ls, all.names = TRUE)()`
#'   calls `ls(all.names = TRUE)` from an (ephemeral) evaluation environment.
#'
#' @examples
#' # Arguments can be fixed by name
#' draw3 <- partial(sample, size = 3)
#' draw3(letters)
#'
#' # Arguments can be fixed by position
#' draw3 <- partial(sample, , 3)
#' draw3(letters)
#'
#' # Use departial() to recover the original function
#' stopifnot(identical(departial(draw3), sample))
#'
#' # Lazily evaluate argument values by default
#' # The value of 'n' is evaluated whenever rnd() is called.
#' rnd <- partial(runif, n = rpois(1, 5))
#' replicate(4, rnd(), simplify = FALSE)   # variable length
#'
#' # Eagerly evaluate argument values with unquoting (`!!`)
#' # The value of 'n' is fixed when 'rnd_eager' is created.
#' rnd_eager <- partial(runif, n = !!rpois(1, 5))
#' len <- length(rnd_eager())
#' reps <- replicate(4, rnd_eager(), simplify = FALSE)   # constant length
#' stopifnot(all(vapply(reps, length, integer(1)) == len))
#'
#' # Mix evaluation schemes by combining lazy evaluation with unquoting (`!!`)
#' # Here 'n' is lazily evaluated, while 'max' is eagerly evaluated.
#' rnd_mixed <- partial(runif, n = rpois(1, 5), max = !!sample(10, 1))
#' replicate(4, rnd_mixed(), simplify = FALSE)
#'
#' # Arguments to fix can be spliced
#' args_eager <- list(n = rpois(1, 5), max = sample(10, 1))
#' rnd_eager2 <- partial(runif, !!!args_eager)
#' replicate(4, rnd_eager2(), simplify = FALSE)
#'
#' # Use rlang::exprs() to selectively evaluate arguments to fix
#' args_mixed <- rlang::exprs(n = rpois(1, 5), max = !!sample(10, 1))
#' rnd_mixed2 <- partial(runif, !!!args_mixed)
#' replicate(4, rnd_mixed2(), simplify = FALSE)
#'
#' # partial() truncates formals by the fixed arguments, omits default values
#' foo <- function(x, y = x, ..., z = "z") NULL
#' stopifnot(
#'   identical(formals(partial(foo)),
#'             formals(foo)),
#'   identical(formals(partial(foo, x = 1)),
#'             formals(function(y, ..., z) NULL)),
#'   identical(formals(partial(foo, x = 1, y = 2)),
#'             formals(function(..., z) NULL)),
#'   identical(formals(partial(foo, x = 1, y = 2, z = 3)),
#'             formals(function(...) NULL))
#' )
#'
#' # Nevertheless, partial() remembers default argument values when called
#' f <- function(x, y = x) c(x, y)
#' p <- partial(f, x = 1)
#' stopifnot(identical(p(), c(1, 1)))
#'
#' @export
partial <- function(..f, ...) {
  UseMethod("partial")
}

assign_getter("expr_partial", "names_fixed_args")
assign_setter("expr_partial")

#' @export
partial.PartialFunction <- function(..f, ...) {
  if (missing(...))
    return(..f)
  p <- partial_(departial.PartialFunction(..f), !!!fixed_args(..f), ...)
  expr_partial(p) <- expr_partial(..f)
  class(p) <- class(..f)
  p
}

fixed_args <- function(f) {
  env_get_list(env_partial(f), names_fixed_args(f))
}

env_partial <- function(f) {
  # '__partial__' won't necessarily be bound to the environment of 'f' itself
  environment(get("__partial__", environment(f)))
}

#' @export
partial.CompositeFunction <- function(..f, ...) {
  if (missing(...))
    return(..f)
  fst <- pipeline_head2(..f)
  ..f[[fst$idx]] <- partial(fst$fn, ...)
  ..f
}

pipeline_head2 <- local({
  index_head <- function(x) {
    depth <- 0L
    while (is.list(x)) {
      depth <- depth + 1L
      x <- x[[1L]]
    }
    rep(1L, depth)
  }

  function(f) {
    fs <- as.list.CompositeFunction(f)
    idx <- index_head(fs)
    list(idx = idx, fn = fs[[idx]])
  }
})

#' @export
partial.function <- local({
  expr_fn <- function(..f, clo) {
    if (is.name(expr <- substitute(..f, parent.frame())))
      return(expr)
    call("(", call("function", formals(clo), quote(...)))
  }

  function(..f, ...) {
    if (missing(...))
      return(..f)
    clo <- closure(..f)
    p <- partial_(clo, ...)
    expr_partial(p) <- expr_partial(..f) %||% expr_fn(..f, clo)
    class(p) <- "PartialFunction" %subclass% class(..f)
    p
  }
})

partial_ <- local({
  BODY <- quote(eval(`[[<-`(match.call(), 1L, `__partial__`), parent.frame()))
  DOTS <- as.pairlist(alist(... = ))

  body_partial <- function(nms) {
    fix <- lapply(nms, function(n) call("__eval__", as.name(n))) %named% nms
    as.call(c(quote(`__fn__`), fix, quote(...)))
  }

  match_args <- function(f, ...) {
    qs <- quos(...)
    ordered <- as.call(c(quote(c), seq_along(qs) %named% names(qs)))
    matched <- eval(match.call(f, ordered), baseenv())
    qs <- qs %named% names_chr(matched)[order(matched)]
    qs[vapply(qs, non_void_expr, TRUE)]
  }

  non_void_expr <- function(q) {
    !identical(quo_get_expr(q), quote(expr = ))
  }

  validate <- function(args, nms = names(args)) {
    all(nzchar(nms)) %because%
      "only named arguments can be fixed"
    (anyDuplicated(nms) == 0L) %because%
      "can't set the value of a named '...' argument more than once"
    args
  }

  truncate_formals <- function(f, drop, fmls = formals(f)) {
    fmls <- as.list(fmls[names(fmls) %notin% drop])
    fmls[] <- list(quote(expr = ))
    as.pairlist(fmls)
  }

  assign_setter("names_fixed_args")

  function(clo, ...) {
    fix <- validate(match_args(clo, ...))
    p <- new_fn(truncate_formals(clo, drop = names(fix)), BODY, baseenv(),
                `__partial__` = new_fn(DOTS, body_partial(names(fix))))
    environment(environment(p)$`__partial__`) <- emptyenv() %encloses%
      c(`__fn__` = clo, `__eval__` = eval_tidy, fix)
    names_fixed_args(p) <- names(fix)
    p
  }
})

#' @export
partial.default <- function(..f, ...) {
  not_fn_coercible(..f)
}

#' @rdname partial
#' @export
departial <- function(..f) {
  UseMethod("departial")
}

#' @export
departial.PartialFunction <- function(..f) {
  get("__fn__", env_partial(..f))
}

#' @export
departial.CompositeFunction <- function(..f) {
  fst <- pipeline_head2(..f)
  ..f[[fst$idx]] <- departial(fst$fn)
  ..f
}

#' @export
departial.function <- function(..f) {
  ..f
}

#' @export
departial.default <- function(..f) {
  not_fn_coercible(..f)
}

#' @export
print.PartialFunction <- function(x, ...) {
  cat("<Partially Applied Function>\n")
  cat("\n* FUNCTION:\n")
  expr_print(signature(x))
  cat("\nCalling 'FUNCTION(...)' is equivalent to calling\n")
  fun <- expr_partial(x)
  expr_print(as.call(c(fun, fixed_args(x), quote(...))))
  if (is.name(fun)) {
    cat("\nThe function '", fun, "()' has the form\n", sep = "")
    expr_print(signature(departial(x)))
  }
  cat("\nRecover the called function with 'departial()'.")
  invisible(x)
}

signature <- function(f) {
  call("function", formals(f), quote(...))
}
