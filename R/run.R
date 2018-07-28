#' Run an Expression in an Ordered Environment
#'
#' @param ..data Environment, list or data frame of bindings (cf. `envir`
#'   argument of [eval()]).
#' @param ..expr Expression to evaluate (“run”).
#'   [Quasiquotation][rlang::quasiquotation] is supported.
#' @param ... Named expressions. An expression may depend on the preceding ones.
#'   For `run()`, these override those in `..data`. Quasiquotation is supported.
#'
#' @return `run()` returns the evaluation of `..expr` in the combined
#'   environment of `..data` and `...`.
#'
#' @export
run <- function(..data = parent.frame(), ..expr, ...) {
  if (!is.environment(..data))
    ..data <- evalq(environment(), ..data, parent.frame())
  eval(enexpr(..expr), wrt(..data, ...))
}

#' @rdname run
#'
#' @param ..env Environment in which the expressions of `...` are resolved.
#'
#' @return `wrt()` returns an environment where the bindings in `...` are in
#'   scope, as [promises][delayedAssign()], as if they were assigned from left
#'   to right in the environment `..env`.
#'
#' @export
wrt <- function(..env = parent.frame(), ...) {
  as_ordered_promises(exprs(...), ..env)
}

as_ordered_promises <- function(data, env) {
  all(nzchar(names(data))) %because% "Expressions must be named"
  for (i in seq_along(data))
    env <- bind_as_promise(data[i], env)
  env
}

bind_as_promise <- function(expr, parent) {
  env <- new.env(parent = parent)
  do.call(delayedAssign, list(names(expr), expr[[1L]], parent, env))
  env
}
