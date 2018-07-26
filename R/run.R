#' Run an Expression in an Ordered Context
#'
#' @param ..data List or environment of bindings.
#' @param ..expr Expression to evaluate (“run”). Unquoting is supported.
#' @param ... Named expressions. An expression may depend on the preceding ones.
#'   Unquoting is supported. For `run()`, these override those in `..data`.
#'
#' @return `run()` evaluates `..expr` in the combined context defined by
#'   `..data` and `...`.
#'
#' @export
run <- function(..data = parent.frame(), ..expr, ...) {
  if (!is.environment(..data))
    ..data <- evalq(environment(), ..data, parent.frame())
  eval(enexpr(..expr), wrt(..., ..env = ..data))
}

#' @rdname run
#'
#' @param ..env Environment in which the expressions of `...` are resolved.
#'
#' @return `wrt()` returns an environment which binds the last expression as a
#'   [promise][base::delayedAssign()]. The parent of this environment binds the
#'   penultimate expression as a promise, and so forth, until `..env` is reached
#'   as the common ancestor.
#'
#' @export
wrt <- function(..., ..env = parent.frame()) {
  as_ordered_promises(exprs(...), ..env)
}

as_ordered_promises <- function(data, env) {
  for (i in seq_along(data))
    env <- bind_as_promise(data[i], env)
  env
}

bind_as_promise <- function(expr, parent) {
  env <- new.env(parent = parent)
  do.call(delayedAssign, list(names(expr), expr[[1L]], parent, env))
  env
}
