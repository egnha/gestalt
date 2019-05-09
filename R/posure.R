#' Variable Composite Functions
#'
#' @description
#' `posure()` enables you to create _efficient_ variable (i.e., parameterized)
#' [composite functions][compose()].
#'
#' For instance, say you have a composite function such as
#' ```
#'   function(..., b = 2, n) {
#'     (sample %>>>% log(base = b) %>>>% rep(n))(...)
#'   }
#'
#'   # Alternatively, expressed with the magrittr %>%:
#'   function(..., b = 2, n) {
#'     sample(...) %>% log(base = b) %>% rep(n)
#'   }
#' ```
#' which varies according to the values of `b` and `n`.  You can express this
#' more succinctly with `posure()`, by dropping the placeholder argument
#' (\sQuote{`...`}):
#' ```
#'   posure(b = 2, n ~ {
#'     sample %>>>% log(base = b) %>>>% rep(n)
#'   })
#' ```
#'
#' This creates a function with same [formals][formals()] and return values.
#'
#' But the `posure()` version is more efficient because it creates the composite
#' function just _once_, rather than anew with each function call. Morever, it
#' is robuster than the functionally equivalent construction with the
#' [\pkg{magrittr}](https://cran.r-project.org/package=magrittr) `` `%>%` ``
#' because `posure()` validates the constituent functions (see
#' \sQuote{Examples}).
#'
#' @details
#' `posure()` [curries](https://en.wikipedia.org/wiki/Currying) composite
#' functions. However, the main significance of `posure()` is its efficiency,
#' which is achieved via non-standard scoping semantics (transparent to the
#' caller). `posure()` creates the given composite function once. When the
#' resulting variable composite function is called, its dependencies are
#' dynamically bound to its localized _lexical_ scope, for fast lookup, then
#' removed when the function exits. Thus a **posure** is a (parameterized)
#' [closure][function()] that is _partially dynamically scoped_. (This
#' portmanteau is due to [Henry Stanley](https://github.com/henryaj).)
#'
#' @param ... Function declaration whose body must be a function composition
#'   expressed using [`%>>>%`]. [Quasiquotation][rlang::quasiquotation] is
#'   supported. The syntax is that of [fn()] (see \sQuote{Function
#'   Declarations}) except that declaring \sQuote{`...`} among `...` is
#'   ambiguous.
#' @param ..env Environment in which to create the function. (You should rarely
#'   need to set this.)
#'
#' @return Function with [formals][formals()]
#'   `function (..., <composite_function_dependencies>)`, where
#'   `<composite_function_dependencies>` stands for the formals captured by the
#'   dots of `posure()`. In particular, a call of the form
#'   ```
#'     posure(a, b = value ~ f(a, b) %>>>% g(a, b))
#'   ```
#'   produces a function with the same formals and return values as
#'   ```
#'     function(..., a, b = value) {
#'       (f(a, b) %>>>% g(a, b))(...)
#'     }
#'   ```
#'
#' @seealso [`%>>>%`], [fn()], [partial()].
#'
#' @examples
#' foo <- posure(b = 2, n ~ {
#'   sample %>>>% log(base = b) %>>>% rep(n)
#' })
#'
#' # A posure is a composite function with dependencies:
#' foo
#'
#' set.seed(1)
#' foo(2^(1:10), size = 2, n = 3)
#' #> [1] 3 4 3 4 3 4
#'
#' set.seed(1)
#' rep(log(sample(2^(1:10), size = 2), base = 2), 3)
#' #> [1] 3 4 3 4 3 4
#'
#' # However, a 'posure()' does the composition upfront, so it is faster
#' # than the equivalent function defined using the magrittr pipe:
#'
#' library(magrittr)  # Provides the pipe %>%
#'
#' foo_pipe <- function(..., b = 2, n) {
#'   sample(...) %>% log(base = b) %>% rep(n)
#' }
#'
#' set.seed(1)
#' foo_pipe(2^(1:10), size = 2, n = 3)
#' #> [1] 3 4 3 4 3 4
#'
#' # Moreover, posures are safer than functions defined using the pipe,
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

  `%wrt%` <- function(call, nms) {
    as.call(c(group, clean_up(nms), lapply(nms, bind_promises), call))
  }
  group <- as.name("{")
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
  assign_getter("expr_posure")

  function(x) {
    composite <- call("(", expr_posure(x))
    call_composite <- as.call(c(composite, quote(...)))
    body(x) <- call("{", call_composite)
    x
  }
})
