# Aliases
list_tidy <- list2
names_chr <- names2
`%named%` <- function(x, nm) `names<-`(x, nm)

`%??%` <- function(x, default) {
  if (length(x) == 0L) default else x
}

new_fn <- function(..args, ..body, ..env = NULL, ...) {
  if (!is.pairlist(..args))
    ..args <- as.pairlist(..args)
  if (missing(...))
    return(eval(call("function", ..args, ..body), ..env))
  eval(call("function", ..args, ..body), list_tidy(...), ..env)
}

fml_args <- function(f) {
  formals(args(f) %||% as_closure(f))
}

closure <- function(f) {
  if (typeof(f) == "closure")
    return(f)
  as_closure(f, parent.frame())
}

eponymous <- function(nms) {
  lapply(nms, as.name) %named% nms
}

has_dots <- function(x) {
  match("...", x, nomatch = 0L) > 0L
}

`%notin%` <- function(xs, set) {
  match(xs, set, nomatch = 0L) == 0L
}

`%are%` <- function(xs, set) {
  all(match(xs, set, nomatch = 0L) > 0L)
}

is_onesided <- function(fml) {
  length(fml) == 2L
}

`%because%` <- function(assertion, reason) {
  if (!assertion) stop(reason, call. = FALSE)
  invisible(TRUE)
}

`%unless%` <- function(expr, failure) {
  tryCatch(expr, error = function(e) halt(failure, e$message))
}

halt <- function(msg, ...) {
  stop(sprintf(msg, ...), call. = FALSE)
}

`%subclass%` <- function(class, superclass) {
  wh_class <- which(superclass == class)
  if (is_empty(wh_class))
    return(c(class, superclass))
  if (isTRUE(wh_class == 1L))
    return(superclass)
  c(class, superclass[-wh_class])
}

# nocov start (build-time only)
getter <- function(nm) {
  force(nm)
  function(x) {
    env <- environment(x) %||% emptyenv()
    if (exists(nm, envir = env))
      return(get(nm, envir = env))
    NULL
  }
}

assign_getter <- function(nm) {
  property <- mangle(nm)
  getter <- function(x) {
    attr(x, property, exact = TRUE)
  }
  assign(nm, getter, envir = parent.frame())
  invisible(getter)
}

assign_setter <- function(nm) {
  property <- mangle(nm)
  setter <- function(x, value) {
    attr(x, property) <- value
    invisible(x)
  }
  assign(paste0(nm, "<-"), setter, envir = parent.frame())
  invisible(setter)
}

mangle <- function(nm) {
  paste0(".__GESTALT_", toupper(nm), "__.")
}

check_head <- function(nm) {
  sym <- as.name(nm)
  function(x) identical(x[[1L]], sym)
}
# nocov end

`%encloses%` <- function(parent, bindings) {
  list2env(bindings, parent = parent)
}

envir <- function(f) {
  environment(f) %||% baseenv()
}

mut_nodes <- function(xs, f, ...) {
  rapply(xs, f, how = "replace", ...)
}

pick <- function(x, i) {
  if (is.atomic(i))
    return(.subset2(x, i))
  for (idx in i)
    x <- x[[idx]]
  x
}

`pick<-` <- local({
  lhs <- quote(x)

  function(x, i, value) {
    if (is.atomic(i)) {
      x[[i]] <- value
      return(x)
    }
    for (idx in i)
      lhs <- call("[[", lhs, idx)
    eval(call("<-", lhs, value))
    x
  }
})

not_fn_coercible <- function(x) {
  cls <- paste(deparse(class(x)), collapse = "")
  halt("Cannot interpret object of class %s as a function", cls)
}

#' Raw quotation of an expression
#'
#' `quo_get_expr_()` is an extension of [rlang::quo_get_expr()] that comprehends
#' literal unquoting operators: `QUQ()`, `QUQS()` are substituted as
#' `` `!!`() ``, and `` `!!!`() ``, resp.
#'
#' @noRd
quo_get_expr_ <- local({
  quq <- list(
    QUQ  = as.name("!!"),
    QUQS = as.name("!!!")
  )

  function(x) {
    do.call("substitute", list(quo_get_expr(x), quq))
  }
})
