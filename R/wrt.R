#' Create a Function with Dependencies
#'
#' @param ... Function declaration whose body evaluates to a function.
#'   Quasiquotation is supported.
#' @param ..env Environment in which to create the function.
#'
#' @return Function.
#'
#' @examples
#' # A composition with functional dependencies ('b', 'n'):
#' foo <- wrt(b = 2, n ~ {
#'   sample %>>>% log(base = b) %>>>% rep(n)
#' })
#'
#' set.seed(1)
#' foo(1:10, size = 2, n = 3)
#' #> [1] 1.584963 2.000000 1.584963 2.000000 1.584963 2.000000
#'
#' set.seed(1)
#' rep(log(sample(1:10, size = 2), base = 2), 3)
#' #> [1] 1.584963 2.000000 1.584963 2.000000 1.584963 2.000000
#'
#' # Because 'wrt()' does the composition upfront, it is faster
#' # than the equivalent function defined using the magrittr pipe:
#'
#' library(magrittr)  # Provides the pipe %>%
#'
#' foo_pipe <- function(..., b = 2, n) {
#'   sample(...) %>% log(base = b) %>% rep(n)
#' }
#'
#' set.seed(1)
#' foo_pipe(1:10, size = 2, n = 3)
#' #> [1] 1.584963 2.000000 1.584963 2.000000 1.584963 2.000000
#'
#' @name wrt
NULL

make_dependent_function <- local({
  `%wrt%` <- function(call, nms) {
    as.call(c(group, clean_up(nms), lapply(nms, bind_actively), call))
  }
  group <- as.name("{")
  clean_up <- function(nms) {
    substitute(
      on.exit(rm(list = NMS, envir = `__env__`)),
      list(NMS = nms)
    )
  }
  bind_actively <- function(nm) {
    substitute(
      makeActiveBinding(NM, function() SYM, `__env__`),
      list(NM = nm, SYM = as.name(nm))
    )
  }

  dots <- list(... = quote(expr = ))
  call_fun <- quote(`__fun__`(...))

  function(args, body, parent) {
    env <- new.env(parent = parent)
    env$`__env__` <- env
    env$`__fun__` <- eval(body, env) %unless% "Body cannot be evaluated: %s"
    is.function(env$`__fun__`) %because% "Body must evaluate to a function"
    new_fn(c(dots, args), call_fun %wrt% names(args), env)
  }
})

#' @rdname wrt
#' @export
wrt <- fn_constructor(exprs, make_dependent_function)
