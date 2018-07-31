#' External Dependencies as Function Arguments
#'
#' `wrt()` allows you to reference external dependencies of a function (i.e.,
#' unbound names in its body) as function arguments, which are _locally scoped_.
#'
#' @param ... Function declaration whose body evaluates to a function.
#'   Quasiquotation is supported. The syntax is same as that of [fn()].
#' @param ..env Environment in which to create the function. Usually, you should
#'   not need to set this.
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
#' foo(2 ^ (1:10), size = 2, n = 3)
#' #> [1] 3 4 3 4 3 4
#'
#' set.seed(1)
#' rep(log(sample(2 ^ (1:10), size = 2), base = 2), 3)
#' #> [1] 3 4 3 4 3 4
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
#' foo_pipe(2 ^ (1:10), size = 2, n = 3)
#' #> [1] 3 4 3 4 3 4
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
      on.exit(rm(list = NMS, envir = `__lex__`)),
      list(NMS = nms)
    )
  }
  bind_actively <- function(nm) {
    substitute(
      makeActiveBinding(quote(SYM), function() SYM, `__lex__`),
      list(SYM = as.name(nm))
    )
  }

  dots <- list(... = quote(expr = ))
  call_fun <- quote(`__fun__`(...))

  function(args, body, parent) {
    env <- new.env(parent = parent)
    env$`__lex__` <- env
    env$`__fun__` <- eval(body, env) %unless% "Body cannot be evaluated: %s"
    is.function(env$`__fun__`) %because% "Body must be a function"
    new_fn(c(dots, args), call_fun %wrt% names(args), env)
  }
})

#' @rdname wrt
#' @export
wrt <- fn_constructor(exprs, make_dependent_function)
