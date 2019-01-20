# formals(closure(f)) and formals(args(f)) coincide, unlike rlang::as_closure()
closure <- function(f) {
  if (typeof(f) == "closure")
    return(f)
  nm <- primitive_name(f)
  if (!is.null(op <- primitive_ops[[nm]]))
    return(op)
  fmls <- formals(args(nm))
  new_fn(fmls, invocation(nm, fmls), .BaseNamespaceEnv)
}

# Cf. rlang::prim_name()
primitive_name <- function(prim) {
  nm <- format(prim)
  matches <- regmatches(nm, regexec('.Primitive\\("(.+)"\\)$', nm))
  matches[[1]][[2]]
}

invocation <- function(nm, fmls) {
  nms <- names(fmls)
  args <- lapply(nms, as.name) %named% character(length(nms))
  if (!is.na(pos <- match("...", nms)))
    names(args)[-seq_len(pos)] <- nms[-seq_len(pos)]
  as.call(c(as.name(nm), args))
}

primitive_ops <- alist(
  `:`    = function(a, b) a:b,
  `&&`   = function(x, y) x && y,
  `||`   = function(x, y) x || y,
  `(`    = function(x) x,
  `{`    = function(...) eval.parent(`[[<-`(match.call(), 1, `{`)),
  `[`    = function(x, ...) x[...],
  `[[`   = function(x, ...) x[[...]],
  `[[<-` = function(x, i, value) `[[<-`(x, i, value = value),
  `[<-`  = function(x, ..., value) `[<-`(x, ..., value = value),
  `$`    = function(x, name) eval(call("$", x, substitute(name))),
  `$<-`  = function(x, name, value) eval(call("$<-", x, substitute(name), value)),
  `@`    = function(object, name) eval(call("@", object, substitute(name))),
  `@<-`  = function(object, name, value) eval(call("@<-", object, substitute(name), value)),
  `<-`   = function(x, value) eval.parent(call("<-", substitute(x), value)),
  `<<-`  = function(x, value) eval.parent(call("<<-", substitute(x), value)),
  `=`    = function(x, value) eval.parent(call("=", substitute(x), value)),
  `~`    = function(y, model) {
    if (missing(model))
      return(eval.parent(call("~", substitute(y))))
    eval.parent(call("~", substitute(y), substitute(model)))
  }
)
primitive_ops <- lapply(primitive_ops, eval, envir = .BaseNamespaceEnv)

constructs <- c(
  "break",
  "for",
  "function",
  "if",
  "next",
  "repeat",
  "return",
  "while"
)
primitive_ops[constructs] <- lapply(constructs, function(word) {
  msg <- sprintf("`%s` cannot be expressed as a closure", word)
  function() stop(msg, call. = FALSE)
})

# Ensure that the list of primitive operators is complete
stopifnot({
  objs <- objects("package:base", all.names = TRUE)
  prim <- sapply(objs, function(x) is.primitive(get(x)))
  no_formals <- !objs[prim] %in% union(names(.ArgsEnv), names(.GenericArgsEnv))
  setequal(names(primitive_ops), objs[prim][no_formals])
})
