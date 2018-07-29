#' Run an Expression in an Ordered Environment
#'
#' @param `_data` Context of named values, namely an environment, list or data
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
