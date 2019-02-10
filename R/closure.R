#' Express a function as a closure
#'
#' Unlike [rlang::as_closure()], the formals of `closure()` agree with those
#' of [args()].
#'
#' @param f Function.
#'
#' @return If `f` is a closure, it is simply returned. Otherwise, `f` is a
#'   primitive function, and `closure()` wraps it in a closure whose environment
#'   is `.BaseNamespaceEnv` and whose formals are those of `args()`, unless this
#'   is `NULL`, in which case an error is raised.
#'
#' @details The environment `primitives` is local to `closure()` so that the
#'   package byte-compiles.
#'
#' @noRd
closure <- local({
  # Environment of primitive functions as closures
  primitives <- list(
    `:`    = function(a, b) a:b,
    `&&`   = function(x, y) x && y,
    `||`   = function(x, y) x || y,
    `[`    = function(x, ...) x[...],
    `[[`   = function(x, ...) x[[...]],
    `[[<-` = function(x, ..., value) `[[<-`(x, ..., value = value),
    `[<-`  = function(x, ..., value) `[<-`(x, ..., value = value)
  )
  primitives <- lapply(primitives, `environment<-`, value = .BaseNamespaceEnv)
  primitives <- list2env(primitives)

  # No good way to define these primitives as closures; raise error if you try.
  for (op in c(
    "(", "{", "$", "$<-", "@", "@<-", "<-", "<<-", "=", "~",
    "if", "for", "while", "break", "next", "repeat", "function", "return"
  )) {
    err <- sprintf("Expressing `%s` as a closure is not supported", op)
    primitives[[op]] <- structure(
      list(message = err, call = NULL),
      class = c("NonclosurePrimitive", "error", "condition")
    )
  }

  # Ensure that the remaining primitives are those with formals
  stopifnot({
    objs <- objects("package:base", all.names = TRUE)
    prim <- sapply(objs, function(x) is.primitive(get(x)))
    no_formals <- !objs[prim] %in% union(names(.ArgsEnv), names(.GenericArgsEnv))
    setequal(names(primitives), objs[prim][no_formals])
  })

  invocation <- function(nm, fmls) {
    nms <- names(fmls)
    args <- lapply(nms, as.name) %named% character(length(nms))
    if (!is.na(pos <- match("...", nms)))
      names(args)[-seq_len(pos)] <- nms[-seq_len(pos)]
    as.call(c(as.name(nm), args))
  }

  # Bind primitives with formals, as closures
  for (prim in union(names(.ArgsEnv), names(.GenericArgsEnv))) {
    fmls <- formals(args(prim))
    primitives[[prim]] <- new_fn(fmls, invocation(prim, fmls), .BaseNamespaceEnv)
  }

  # Cf. rlang::prim_name()
  primitive_name <- function(prim) {
    nm <- format(prim)
    sub("\"\\)$", "", sub("^.Primitive\\(\"", "", nm))
  }

  function(f) {
    if (typeof(f) == "closure")
      return(f)
    prim <- primitives[[primitive_name(f)]]
    if (inherits(prim, "NonclosurePrimitive"))
      stop(prim)
    prim
  }
})
