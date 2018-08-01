#' External Dependencies as Function Arguments
#'
#' `posure()` allows you to reference external dependencies of a function (i.e.,
#' unbound names in its body) as function arguments, which are _locally scoped_.
#'
#' @param ... Function declaration whose body evaluates to a _composite
#'   function_, as declared using [compose()] or [`%>>>%`]. Quasiquotation is
#'   supported. The syntax is same as that of [fn()].
#' @param ..env Environment in which to create the function. Usually, you should
#'   not need to set this.
#'
#' @return Function.
#'
#' @examples
#' # A composition with functional dependencies ('b', 'n'):
#' foo <- posure(b = 2, n ~ {
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
#' # Because 'posure()' does the composition upfront, it is faster
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
#' # Moreover, posures are safer than functions made using the pipe,
#' # because '%>>>%' validates constituent functions:
#' \donttest{
#' posure(b = 2, n ~ log(Base = b) %>>>% rep(n))
#' # Error: unused argument (Base = b)
#'
#' posure(b = 2 ~ my_sample %>>>% log(base = b))
#' # Error: object 'my_sample' not found}
#'
#' @name posure
NULL

make_posure <- local({
  is_posure <- function(expr) {
    expr <- unpack(expr)
    is.call(expr) && is_op_compose(expr)
  }
  unpack <- function(expr) {
    if (!(is.call(expr) && is_lambda(expr)))
      return(expr)
    Recall(expr[[2L]])
  }
  group <- as.name("{")

  `%wrt%` <- function(call, nms) {
    as.call(c(group, clean_up(nms), lapply(nms, bind_promises), call))
  }
  clean_up <- function(nms) {
    substitute(
      on.exit(rm(list = NMS, envir = `__lex__`)),
      list(NMS = nms)
    )
  }
  bind_promises <- function(nm) {
    substitute(
      delayedAssign(NM, SYM, assign.env = `__lex__`),
      list(NM = nm, SYM = as.name(nm))
    )
  }

  dots <- list(... = quote(expr = ))
  call_fun <- quote(`__fun__`(...))

  assign_setter("expr_posure")

  function(args, body, parent) {
    is_posure(body) %because%
      "Posure body must be a composite function expressed using '%>>>%'"
    env <- new.env(parent = parent)
    env$`__lex__` <- env
    env$`__fun__` <- eval(body, env) %unless% "Body cannot be evaluated: %s"
    p <- new_fn(c(dots, args), call_fun %wrt% names(args), env)
    class(p) <- c("Posure", "function")
    expr_posure(p) <- unpack(body)
    p
  }
})

#' @rdname posure
#' @export
posure <- fn_constructor(exprs, make_posure)

#' @export
print.Posure <- function(x, ...) {
  cat("<Posure>\n\n")
  print(with_posure_body(x))
  invisible(x)
}

with_posure_body <- local({
  get_expr_posure <- assign_getter("expr_posure")

  function(x) {
    composite <- call("(", get_expr_posure(x))
    call_composite <- as.call(c(composite, quote(...)))
    body(x) <- call("{", call_composite)
    x
  }
})
