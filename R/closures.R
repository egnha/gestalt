# formals(closure(f)) and formals(args(f)) coincide, unlike rlang::as_closure()
closure <- function(f) {
  if (typeof(f) == "closure")
    return(f)
  nm <- primitive_name(f)
  if (!is.null(op <- primitive_ops[[nm]])) {
    if (inherits(op, "error")) stop(op)
    return(op)
  }
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
  `[`    = function(x, ...) x[...],
  `[[`   = function(x, ...) x[[...]],
  `[[<-` = function(x, i, value) `[[<-`(x, i, value = value),
  `[<-`  = function(x, ..., value) `[<-`(x, ..., value = value)
)
primitive_ops <- lapply(primitive_ops, eval, envir = .BaseNamespaceEnv)
invalid_ops <- c(
  "(", "{", "$", "$<-", "@", "@<-", "<-", "<<-", "=", "~",
  "if", "for", "while", "break", "next", "repeat",
  "function", "return"
)
primitive_ops[invalid_ops] <- lapply(invalid_ops, function(nm) {
  msg <- sprintf("Expressing `%s` as a closure is not supported", nm)
  structure(
    list(message = msg, call = NULL),
    class = c("error", "condition")
  )
})
primitive_ops <- list2env(primitive_ops)

stopifnot({
  objs <- objects("package:base", all.names = TRUE)
  prim <- sapply(objs, function(x) is.primitive(get(x)))
  no_formals <- !objs[prim] %in% union(names(.ArgsEnv), names(.GenericArgsEnv))

  # Ensure that the list of primitive operators is complete
  setequal(names(primitive_ops), objs[prim][no_formals])
})
