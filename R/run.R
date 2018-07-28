#' Run an Expression in an Ordered Environment
#'
#' @param ..data Environment, list or data frame; if a list or data frame, it is
#'   interpreted as an environment (like the `envir` argument of [eval()]).
#' @param ..expr Expression to evaluate (“run”).
#'   [Quasiquotation][rlang::quasiquotation] is supported.
#' @param ... Named expressions. An expression depends on the preceding ones,
#'   and its name takes precedence over those in `..data`. Quasiquotation is
#'   supported.
#'
#' @return `run()` returns the evaluation of `..expr` in the combined
#'   environment of `..data` and `...`.
#'
#' @export
run <- function(..data = parent.frame(), ..expr, ...) {
  eval(enexpr(..expr), wrt(..data, ...))
}

#' @rdname run
#'
#' @return `wrt()` returns an environment where the bindings in `...` are in
#'   scope, as [promises][delayedAssign()], as if they were assigned from left
#'   to right in the environment defined by `..data`.
#'
#' @export
wrt <- function(..data = parent.frame(), ...) {
  if (!is.environment(..data))
    ..data <- evalq(environment(), ..data, parent.frame())
  as_ordered_promises(..data, ...)
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
