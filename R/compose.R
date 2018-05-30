#' Compose Functions
#'
#' @description
#' Compose functions in two ways:
#'
#' - Use `compose(f, g, ...)` to make the function that applies `f`, then `g`,
#'   etc. It has the [formals][base::formals()] of the first function applied,
#'   namely `f`. Thus
#'   ```
#'     compose(paste, toupper)
#'   ```
#'   is equivalent to the function
#'   ```
#'     function(..., sep = " ", collapse = NULL) {
#'       toupper(paste(..., sep = sep, collapse = collapse))
#'     }
#'   ```
#'
#' - Alternatively, use the infix notation `f %>>>% g %>>>% ...`, which
#'   comprehends the semantics of the
#'   [\pkg{magrittr}](https://cran.r-project.org/package=magrittr) `` `%>%` ``
#'   operator and, additionally, [quasiquotation][rlang::quasiquotation].
#'   Thus, assuming `sep` has the value `""`,
#'   \preformatted{%
#'   sample \%>>>\% paste(collapse = !!sep)}
#'   is equivalent to the function
#'   ```
#'     function(x, size, replace = FALSE, prob = NULL) {
#'       paste(sample(x, size, replace, prob), collapse = "")
#'     }
#'   ```
#'
#' Use [as.list()] to recover the list of composite functions.
#'
#' @param ... Functions or lists thereof to compose, in order of application.
#'   Lists of functions are automatically spliced in.
#'   [Unquoting][rlang::quasiquotation] of names, via `!!` on the left-hand side
#'   of `:=`, and [splicing][rlang::quasiquotation], via `!!!`, are supported.
#'
#' @return Function of class `CompositeFunction`, whose
#'   [formals][base::formals()] match those of the first function applied (as a
#'   closure).
#'
#' @section Semantics of `` `%>>>%` ``:
#'   `` `%>>>%` `` adopts the semantics of the
#'   [\pkg{magrittr}](https://cran.r-project.org/package=magrittr) `` `%>%` ``:
#'   \enumerate{
#'     \item Names are matched to functions.
#'     \item Function calls are implicitly “partialized” as a unary function of
#'       `.` (a point). The rule is that when the point matches an argument
#'       value, such as in
#'       \preformatted{%
#'   ... \%>>>\% f(x, .) \%>>>\% ...
#'   ... \%>>>\% f(x, y = .) \%>>>\% ...}
#'       the ‘`f(x, .)`’, resp. ‘`f(x, y = .)`’, is interpreted as the function
#'       `function(.) f(x, .)`, resp. `function(.) f(x, y = .)`. Otherwise, the
#'       call is implicitly “partialized,” e.g., in sequences such as
#'       \preformatted{%
#'   ... \%>>>\% f(x, y) \%>>>\% ...
#'   ... \%>>>\% f(x, y(.)) \%>>>\% ...}
#'       the ‘`f(x, y)`’, resp. ‘`f(x, y(.))`’, is interpreted as the function
#'       `function(.) f(., x, y)`, resp. `function(.) f(., x, y(.))`.
#'     \item Expressions in curly braces are interpreted as unary-function
#'       bodies (i.e., curly braces represent “lambda functions”). For example,
#'       in
#'       \preformatted{%
#'   ... \%>>>\% {f(.); g(.)} \%>>>\% ...}
#'       the ‘`{f(.); g(.)}`’ is interpreted as the function
#'       `function(.) {f(.); g(.)}`. Curly braces are useful when you want to
#'       circumvent the implicit-partialization rule for function calls.
#'   }
#'   \subsection{Exceptional Function Calls}{
#'     Exceptions to the rule of implicit partialization of function calls are
#'     made in a few cases of convenience:
#'     \itemize{
#'       \item Parenthesis ([`(`][base::Paren]) applies grouping. In particular,
#'         expressions within parentheses are literally interpreted.
#'       \item Colon ([`:`][base::Colon]) applies naming, according to the
#'         syntax ‘`<name>: <function>`’, where ‘`<function>`’ is interpreted
#'         according to the semantics of `` `%>>>%` ``. For example, in
#'       \preformatted{%
#'   ... \%>>>\% a_name: f \%>>>\% ...}
#'       the function `f` is named `"a_name"`.
#'       \item [fn()], [namespace operators][base::ns-dblcolon] (`` `::`  ``,
#'         `` `:::` ``) and [extractors][base::Extract] (`` `$` ``, `` `[[` ``,
#'         `` `[` ``) are literally interpreted. This allows for list extractors
#'         to be applied to composite functions appearing in a `` `%>>>%` ``
#'         call (see ‘Operate on a Composite Function’).
#'     }
#'   }
#'   \subsection{Quasiquotation}{
#'     Tidyverse [unquoting][rlang::quasiquotation] via `!!` is supported. Use
#'     it to:
#'     \itemize{
#'       \item Enforce immutability. For example, by unquoting `res` in
#'         \preformatted{%
#'   res <- "result"
#'   get_result <- identity \%>>>\% lapply(`[[`, !!res)}
#'         you ensure that the function `get_result()` always extracts the
#'         component named `"result"`, even if the binding `res` mutates or
#'         is removed.
#'       \item Interpret ‘`.`’ in the lexical scope. For example,
#'         \preformatted{%
#'   . <- "point"
#'   is_point <- {.[!is.na(.)]} \%>>>\% {. == !!.}}
#'         determines those non-`NA` components of a (character) vector that
#'         equal the string `"point"`.
#'       \item Programmatically assign names. For example, unquoting `nm` in
#'         \preformatted{%
#'   nm <- "a_name"
#'   ... \%>>>\% !!nm: f \%>>>\% ...}
#'         names the `f`-component of the resulting composite function
#'         `"a_name"`.
#'       \item Expend a computation upfront to spare a runtime expense. For
#'         example, presuming the value of the call `f()` is immutable and that
#'         `g` is a pure function, both
#'         \preformatted{%
#'   ... \%>>>\% g(f()) \%>>>\% ...
#'   ... \%>>>\% g(!!f()) \%>>>\% ...}
#'         would be functions yielding the same values. But the first would
#'         compute `f()` anew with each call, whereas the second would simply
#'         depend on a pre-computed value of `f()`.
#'     }
#'   }
#'
#' @section Operate on a Composite Function as If It Were a List:
#'   You can think of a composite function as embodying the (possibly nested)
#'   structure of its list of constituent functions. In fact, you can apply
#'   familiar index and assignment operations to a composite function, as if it
#'   were this list, getting a function in return. This enables you to leverage
#'   composite functions as _structured computations_.
#'
#'   \subsection{Indexing}{
#'     For instance, the ‘`sum`’ in the following composite function
#'     \preformatted{%
#'   f <- abs \%>>>\% out: (log \%>>>\% agg: sum)}
#'     can be [extracted][base::Extract] in the usual ways:
#'     \preformatted{%
#'   f[[2]][[2]], f[[c(2, 2)]],
#'   f$out$agg, f[["out"]][["agg"]], f[["out"]]$agg,
#'   f$out[[2]], f[[list("out", 2)]], ...}
#'     The last form of indexing with a mixed list is handy when you need to
#'     create an index programmatically.
#'     \cr\cr
#'     Additionally, you can excise sub-composite functions with
#'     [`[`][base::Extract], [head()], [tail()]. For example:
#'     \itemize{
#'       \item Both `f[1]` and `head(f, 1)` get the ‘`abs`’ as a composite
#'         function, namely `compose(abs)`
#'       \item `f[2:1]` reverses the order of the top-level functions to yield
#'         \preformatted{%
#'   out: (log \%>>>\% agg: sum) \%>>>\% abs}
#'       \item `f$out[c(FALSE, TRUE)]` gets the ‘`sum`’ as a (named) composite
#'         function
#'     }
#'   }
#'   \subsection{Subset Assignment}{
#'     Similarily, subset assignment works as it does for lists. For instance,
#'     you can replace the ‘`sum`’ with the identity function:
#'     \preformatted{%
#'   f[[2]][[2]] <- identity
#'   f$out$agg <- identity
#'   f[["out"]][["agg"]] <- identity
#'   f$out[[2]] <- identity
#'   f[[list("out", 2)]] <- identity}
#'     Multiple constituent functions can be reassigned using
#'     [`[<-`][base::Extract]. For example
#'     \preformatted{%
#'   f[2] <- list(log)
#'   f["out"] <- list(log)
#'   f[c(FALSE, TRUE)] <- list(log)}
#'     all replace the second constituent function with `log`, so that `f`
#'     becomes `abs %>>>% log`.
#'   }
#'   \subsection{Other Methods}{
#'     The generic methods [unlist()], [length()], [names()] also apply to
#'     composite functions. In conjunction with `compose()`, you can use
#'     `unlist()` to “flatten” compositions. For example
#'     \preformatted{%
#'   compose(unlist(f, use.names = FALSE))}
#'     gives a function that is identical to
#'     \preformatted{%
#'   abs \%>>>\% log \%>>>\% sum}
#'   }
#'
#' @section Composite Functions Are Unsimplified, yet Flattened When Called:
#'   `compose()` and `` `%>>>%` `` are **associative**, semantically and
#'   operationally. Thus, for instance,
#'   \preformatted{%
#'   compose(f, g, h), f \%>>>\% g \%>>>\% h
#'   compose(f, compose(g, h)), f \%>>>\% (g \%>>>\% h)
#'   compose(compose(f, g), h), (f \%>>>\% g) \%>>>\% h}
#'   are implemented as the _same function_—lists of functions are automatically
#'   “flattened out” when composed. In practical terms, this means the speed of
#'   a composite function made by `compose()` or `` `%>>>%` ``, regardless of
#'   its nested depth, is on par with a manually constructed _serial_
#'   composition.
#'
#'   Nonetheless, the original nested structure of constituent functions is
#'   faithfully recovered by [as.list()]. In particular, `as.list()` and
#'   `compose()` are **mutually invertible**: `as.list(compose(fs))` is the same
#'   as `fs`, when `fs` is a (nested) list of functions. (Though the names of
#'   `as.list()` are always strings, possibly empty.)
#'
#' @seealso [constant()]; combined with `` `%>>>%` ``, this provides a lazy,
#'   structured alternative to the
#'   [\pkg{magrittr}](https://cran.r-project.org/package=magrittr) `` `%>%` ``.
#'
#' @examples
#' # Functions are applied in the order in which they are listed
#' inv <- partial(`/`, 1)  # reciprocal
#' f0 <- compose(abs, log, inv)
#' stopifnot(all.equal(f0(-2), 1 / log(abs(-2))))
#'
#' # Alternatively, compose using the `%>>>%` operator
#' f1 <- abs %>>>% log %>>>% {1 / .}
#' stopifnot(all.equal(f1(-2), f0(-2)))
#'
#' \dontrun{
#' # Transform a function to a JSON function
#' library(jsonlite)
#'
#' # By composing higher-order functions:
#' jsonify <- {fromJSON %>>>% .} %>>>% {. %>>>% toJSON}
#'
#' # By directly composing with input/output transformers:
#' jsonify <- fn(f ~ fromJSON %>>>% f %>>>% toJSON)}
#'
#' # Formals of initial function are preserved
#' add <- function(a, b = 0) a + b
#' stopifnot(identical(formals(compose(add, inv)), formals(add)))
#'
#' # Compositions can be provided by lists, in several equivalent ways
#' f2 <- compose(list(abs, log, inv))
#' f3 <- compose(!!! list(abs, log, inv))
#' f4 <- compose(abs, list(log, inv))
#' f5 <- compose(abs, !!! list(log, inv))
#' stopifnot(
#'   all.equal(f2, f0), all.equal(f2(-2), f0(-2)),
#'   all.equal(f3, f0), all.equal(f3(-2), f0(-2)),
#'   all.equal(f4, f0), all.equal(f4(-2), f0(-2)),
#'   all.equal(f5, f0), all.equal(f5(-2), f0(-2))
#' )
#'
#' # compose() and as.list() are mutally invertible
#' f6 <- compose(abs, as.list(compose(log, inv)))
#' stopifnot(
#'   all.equal(f6, f0), all.equal(f6(-2), f0(-2))
#' )
#' fs <- list(abs, log, inv)
#' stopifnot(all.equal(check.attributes = FALSE,
#'   as.list(compose(fs)), fs,
#' ))
#'
#' # `%>>>%` supports names, magrittr `%>%` semantics, quasiquotation
#' sep <- ""
#' scramble <- shuffle: sample %>>>% paste(collapse = !!sep)
#' nonsense <- scramble(letters)
#' stopifnot(
#'   nchar(nonsense) == 26L,
#'   identical(letters, sort(strsplit(nonsense, sep)[[1]])),
#'   identical(scramble$shuffle, sample)
#' )
#'
#' @export
compose <- function(...) {
  fs <- fn_tree(...)
  if (is_empty(fs))
    return(NULL)
  cmp <- fuse(fs)
  class(cmp) <- c("CompositeFunction", "function")
  cmp
}

#' @param fst,snd Functions. These may be optionally named using `:`, e.g.,
#'   `f %>>>% nm: g` names the `g`-component.
#'   [Quasiquotation][rlang::quasiquotation] and the
#'   [\pkg{magrittr}](https://cran.r-project.org/package=magrittr) `` `%>%` ``
#'   semantics are supported (see ‘Semantics of `` `%>>>%` ``’ and ‘Examples’).
#'
#' @rdname compose
#' @export
`%>>>%` <- function(fst, snd) {
  compose(enquo(fst), enquo(snd))
}

fn_tree <- function(...) {
  fs <- lapply(list_tidy(...), fn_interp)
  fs <- drop_null(fs)
  unlist(fs, recursive = FALSE)
}

drop_null <- function(xs) {
  xs[vapply(xs, is.null, TRUE)] <- NULL
  names(xs) <- names_chr(xs)
  is_list <- vapply(xs, is.list, TRUE)
  xs[is_list] <- lapply(xs[is_list], drop_null)
  xs
}

fn_interp <- function(x) {
  UseMethod("fn_interp")
}

#' @export
fn_interp.default <- function(x) {
  not_fn_coercible(x)
}

#' @export
fn_interp.quosure <- function(x) {
  expr <- quo_get_expr_(x)
  if (is_literal(expr))
    return(fn_interp(eval_tidy(x)))
  if (is_group(expr))
    return(list(fn_interp(eval_tidy(x))))
  if (is_named(expr))
    return(lambda_named(expr, quo_get_env(x)))
  if (is_lambda(expr))
    return(lambda(expr, quo_get_env(x)))
  lambda_partial(expr, quo_get_env(x))
}

is_literal <- function(expr) {
  !is.call(expr)        ||
    is_op_compose(expr) ||
    is_subsetter(expr)  ||
    is_fn_lambda(expr)  ||
    is_op_namespace(expr)
}

is_op_compose <- check_head("%>>>%")

is_subsetter <- function(expr) {
  is_dollar(expr) || is_sqr_sqr(expr) || is_sqr(expr)
}
is_dollar  <- check_head("$")
is_sqr_sqr <- check_head("[[")
is_sqr     <- check_head("[")

is_fn_lambda <- check_head("fn")

is_op_namespace <- function(expr) {
  is_op_public(expr) || is_op_private(expr)
}
is_op_public  <- check_head("::")
is_op_private <- check_head(":::")

is_group <- check_head("(")

is_named <- check_head(":")

lambda_named <- function(expr, env) {
  expr[[1L]] <- quote(`:=`)
  rhs <- expr[[3L]]
  # quos() already nests, so subsequent grouping must be undone
  if (is.call(rhs) && is_group(rhs))
    expr[[3L]] <- rhs[[2L]]
  enquos <- as.call(c(quos, expr))
  fn_interp(eval(enquos, env))
}

is_lambda <- check_head("{")

lambda <- function(body, env) {
  new_fn(alist(. = ), body, env)
}

lambda_partial <- local({
  point <- as.name(".")
  is_void <- function(call) {
    length(call) == 1L
  }
  verify_conformance <- function(call, to) {
    match.call(args(to) %||% as_closure(to), call) %unless%
      sprintf("%s is an invalid call: %%s", expr_label(call))
    invisible(call)
  }

  function(call, env) {
    caller <- match.fun(eval_tidy(call[[1L]], env))
    if (is_void(call))
      return(caller)
    args <- as.list(call)[-1L]
    if (all(args != point))
      call <- as.call(c(call[[1L]], point, args))
    verify_conformance(call, to = caller)
    lambda(call, env)
  }
})

#' @export
fn_interp.quosures <- function(x) {
  lapply(x, fn_interp.quosure)
}

#' @export
fn_interp.list <- function(x) {
  lapply(x, fn_interp)
}

#' @export
fn_interp.CompositeFunction <- getter("__fn_tree__")

#' @export
fn_interp.function <- function(x) x

#' @export
fn_interp.NULL <- function(x) NULL

fuse <- function(fs) {
  pipeline <- unlist(fs, use.names = FALSE)
  mouth <- pipeline[[1L]]
  fmls <- fml_args(mouth)
  pipe <- reduce_calls(length(pipeline), fmls)
  env <- envir(mouth) %encloses% (pipeline %named% pipe$nms)
  makeActiveBinding("__fn_tree__", get_tree(fs, env), env)
  new_fn(fmls, pipe$expr, env)
}

reduce_calls <- function(n, fmls) {
  nms <- as_protected_name(seq_len(n))
  args <- lapply(names(fmls), as.name)
  expr <- as.call(c(as.name(nms[[1L]]), args))
  for (nm in nms[-1L])
    expr <- call(nm, expr)
  list(expr = expr, nms = nms)
}

get_tree <- function(fs, env) {
  force(env)
  nms <- fn_names(fs)

  function() {
    mut_nodes(nms, get, envir = env, mode = "function", inherits = FALSE)
  }
}

fn_names <- function(fs) {
  i <- 0L
  mut_nodes(fs, function(.) {
    i <<- i + 1L
    as_protected_name(i)
  })
}

as_protected_name <- function(i) sprintf("__%d__", i)

#' @export
`$.CompositeFunction` <- function(x, i) {
  fs <- as.list.CompositeFunction(x)
  fs <- .subset2(fs, i)
  if (is.function(fs))
    return(fs)
  compose(fs)
}

#' @export
`$<-.CompositeFunction` <- function(x, name, value) {
  fs <- as.list.CompositeFunction(x)
  fs[[name]] <- value
  compose(fs)
}

#' @export
`[[.CompositeFunction` <- function(x, i, ...) {
  fs <- as.list.CompositeFunction(x)
  fs <- pick(fs, i)
  if (is.function(fs))
    return(fs)
  compose(fs)
}

#' @export
`[[<-.CompositeFunction` <- function(x, i, value) {
  fs <- as.list.CompositeFunction(x)
  pick(fs, i) <- value
  compose(fs)
}

#' @export
`[.CompositeFunction` <- function(x, i) {
  if (missing(i))
    return(x)
  fs <- as.list.CompositeFunction(x)
  i <- standardize(i, length(fs))
  compose(.subset(fs, i))
}

#' @export
`[<-.CompositeFunction` <- function(x, i, value) {
  fs <- as.list.CompositeFunction(x)
  fs <- replace_strictly(fs, i, value)
  compose(fs)
}

replace_strictly <- function(x, i, value) {
  len_value <- length(value)
  if (missing(i)) {
    len <- length(x)
  } else {
    i <- standardize(i, length(x))
    len <- if (is.logical(i)) sum(i) else length(i)
  }
  (len_value == 1L || len_value == len) %because%
    sprintf("Replacement length (%d) must be 1 or %d", len_value, len)
  x[i] <- value
  x
}

standardize <- function(i, len) {
  if (is.numeric(i))
    i <- i[abs(i) <= len]
  # Don't recycle predicate vectors
  if (is.logical(i) && (l <- length(i)) != len)
    halt("Predicate length (%d) must be %d", l, len)
  i
}

#' @importFrom utils head
#' @export
head.CompositeFunction <- function(x, n = 1L, ...) {
  compose(head(as.list.CompositeFunction(x), n, ...))
}

#' @importFrom utils tail
#' @export
tail.CompositeFunction <- function(x, n = 1L, ...) {
  compose(tail(as.list.CompositeFunction(x), n, ...))
}

#' @export
names.CompositeFunction <- function(x) {
  names(as.list.CompositeFunction(x))
}

#' @export
`names<-.CompositeFunction` <- function(x, value) {
  fs <- as.list.CompositeFunction(x)
  # From rlang::names2()
  if (is.null(value)) {
    value <- rep("", length(fs))
  } else {
    value <- value %|% ""
  }
  names(fs) <- value
  compose(fs)
}

#' @export
length.CompositeFunction <- function(x) {
  length(as.list.CompositeFunction(x))
}

#' @export
as.list.CompositeFunction <- function(x, ...) {
  fn_interp.CompositeFunction(x)
}

#' @method unlist CompositeFunction
#' @export
unlist.CompositeFunction <- function(x, recursive = TRUE, use.names = TRUE) {
  unlist(as.list.CompositeFunction(x),
         recursive = recursive, use.names = use.names)
}

#' @export
print.CompositeFunction <- function(x, ...) {
  cat("<Function Composition>\n")
  cat("In order of application:\n")
  fs <- as.list.CompositeFunction(x)
  nms <- index_names(fs)
  pipeline <- unlist(fs)
  for (i in seq_along(pipeline)) {
    out <- trim_capture(pipeline[[i]])
    pad <- rep("\ \ ", length(out))
    cat("\n", nms[[i]], "\n", paste0(pad, out, "\n"), sep = "")
  }
  cat("\nRecover the list of functions with 'as.list()'.")
  invisible(x)
}

index_names <- function(x) {
  path_nms <- vapply(index_paths(x), paste, "", collapse = "]][[")
  sprintf("[[%s]]", path_nms)
}

index_paths <- function(x) {
  if (!is.list(x))
    return(NULL)
  idx_nms <- as_indices(names(x))
  paths <- lapply(seq_along(x), function(i) {
    subpaths <- index_paths(x[[i]]) %??% list(NULL)
    lapply(subpaths, function(path) c(idx_nms[[i]], path))
  })
  do.call(c, paths)
}

as_indices <- function(nms) {
  is_named <- nzchar(nms)
  nms[ is_named] <- sprintf('"%s"', nms[is_named])
  nms[!is_named] <- seq_along(nms)[!is_named]
  nms
}

#' @importFrom utils capture.output
trim_capture <- function(f) {
  out <- capture.output(print(f))
  if (inherits(f, "PartialFunction"))
    out <- out[-c(2L, length(out) - 1L, length(out))]
  out
}
