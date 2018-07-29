#' Run an Action in an Ordered Context
#'
#' @description
#' Using R typically involves:
#'
#'   1. Making a context—assigning a set of values.
#'
#'   2. Performing an action—evaluating an expression relative to a context.
#'
#' `let()` and `run()` enable you to treat these procedures as reusable,
#' _composable_ components.
#'
#'   - `let()` makes a **context**: it binds a sequence of _ordered_ named
#'     expressions to a given environment (by default, the current one).
#'
#'     For instance, in an environment `env` where `z` is in scope,
#'     ```
#'       let(x = 1, y = x + 2, z = x * y * z, `_data` = env)
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
#'     except `let()` binds the named expressions _lazily_, as
#'     [promises][delayedAssign()], and comprehends tidyverse
#'     [quasiquotation][rlang::quasiquotation].
#'
#'   - `run()` performs an **action**: it evaluates an expression relative to an
#'     environment (by default, the current one) and, optionally, a sequence of
#'     ordered named expressions.
#'
#'     For instance, in an environment `env` where `x` is in scope,
#'     ```
#'       run(x + y + z, y = x + 2, z = x * y * z, `_data` = env)
#'     ```
#'     is equivalent to calling
#'     ```
#'       local({
#'         y <- x + 2
#'         z <- x * y * z
#'         x + y + z
#'       })
#'     ```
#'     except `run()`, like `let()`, binds `y` and `z` _lazily_ and comprehends
#'     quasiquotation.
#'
#' @param _data Context of named values, namely an environment, list or data
#'   frame; if a list or data frame, it is interpreted as an environment (like
#'   the `envir` argument of [eval()]).
#' @param `_expr` Expression to evaluate (“run”). Quasiquotation is supported.
#' @param ... Named expressions. An expression depends on the preceding ones,
#'   and its name takes precedence over those in `` `_data` ``.
#'   [Quasiquotation][rlang::quasiquotation] of names and expressions is
#'   supported (see ‘Examples’).
#'
#' @return `run()` returns the evaluation of `` `_expr` `` in the combined
#'   environment of `` `_data` `` and `...`.
#'
#'   `let()` returns an environment where the bindings in `...` are in scope, as
#'   [promises][delayedAssign()], as if they were assigned from left to right in
#'   the environment defined by `` `_data` ``.
#'
#'
#'
#'
#' @name run
NULL

#' @rdname run
#' @export
let <- function(`_data` = parent.frame(), ...) {
  if (!is.environment(`_data`))
    `_data` <- evalq(environment(), `_data`, parent.frame())
  as_ordered_promises(`_data`, ...)
}

as_ordered_promises <- function(env, ...) {
  exprs <- exprs(...)
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

#' @rdname run
#' @export
run <- function(`_data` = parent.frame(), `_expr`, ...) {
  eval(enexpr(`_expr`), let(`_data`, ...))
}
