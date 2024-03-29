fn_kinds <- list(
  closure = identity,
  special = log,
  builtin = c,
  partial = partial(function(x, y) c(x, y), x = 0)
)

test_that("function returned unchanged when no argument values to fix", {
  for (f in fn_kinds)
    expect_identical(partial(f), f)
})

test_that("named argument values can be fixed", {
  f <- function(x, y) c(x, y)
  out <- c(0, 1)

  fix_x <- partial(f, x = 0)
  expect_identical(fix_x(1), fix_x(y = 1))
  expect_identical(fix_x(1), out)

  fix_y <- partial(f, y = 1)
  expect_identical(fix_y(0), fix_y(x = 0))
  expect_identical(fix_y(0), out)

  fix_xy <- partial(f, x = 0, y = 1)
  expect_identical(fix_xy(), out)
  expect_error(fix_xy(dummy), "unused argument")
})

test_that("named dots-arguments can be fixed", {
  f <- function(x, ..., y = 3) c(x, ..., y)

  expect_all_equal(
    c(1, a = 2, 3),
    partial(f, a = 2)(1),
    partial(f, 1, a = 2)(),
    partial(f, a = 2, 1)()
  )

  expect_all_equal(
    c(1, a = 2, b = 2.5, 3),
    partial(f, a = 2, b = 2.5)(1),
    partial(f, a = 2, b = 2.5, 1)(),
    partial(partial(f, a = 2), b = 2.5)(1),
    partial(partial(partial(f, 1), a = 2), b = 2.5)(),
    partial(partial(partial(f, a = 2), 1), b = 2.5)(),
    partial(partial(partial(f, a = 2), b = 2.5), 1)()
  )

  expect_all_equal(
    c(1, a = 2, b = 3, 4),
    partial(f, a = 2, b = 3, y = 4)(1),
    partial(partial(f, a = 2, b = 3), y = 4)(1),
    partial(partial(f, a = 2, y = 4), b = 3)(1),
    partial(partial(f, y = 4, a = 2), b = 3)(1),
    partial(partial(f, a = 2), b = 3, y = 4)(1),
    partial(partial(f, y = 4), a = 2, b = 3)(1),
    partial(partial(partial(f, y = 4), a = 2), b = 3)(1),
    partial(partial(partial(f, a = 2), y = 4), b = 3)(1),
    partial(partial(partial(f, a = 2), b = 3), y = 4)(1)
  )
})

test_that("arguments can be matched by position", {
  f <- function(x, y, ...) c(x, y, ...)
  expect_all_equal(
    f(1, 2, 3),
    partial(f)(1, 2, 3),
    partial(f, 1)(2, 3),
    partial(f, , 2)(1, 3),
    partial(f, 1, 2)(3)
  )
})

test_that("dots persist", {
  f <- function(x, y, ..., z = 3) c(x, y, ..., z)
  fs <- list(
    partial(f),
    partial(f, 1),
    partial(f, y = 2),
    partial(f, z = 3),
    partial(f, 1, 2),
    partial(f, 1, z = 3),
    partial(f, y = 2, z = 3),
    partial(f, 1, 2, a = 3),
    partial(partial(f, 1), 2),
    partial(partial(f, 1), 2, a = 3),
    partial(partial(f, 1, 2), a = 3),
    partial(partial(partial(f, 1), 2), a = 3),
    partial(f, 1, 2, a = 3, b = 4, z = 5)
  )
  for (f in fs)
    expect_true("..." %in% names(formals(f)))
})

test_that("fixed named dot-argument doesn't pollute function's lexical scope", {
  a <- "In lexical scope"
  f <- function(x = a, ...) c(x, ...)
  fp <- partial(f, a = "Not in lexical scope")
  expect_identical(fp(), c("In lexical scope", a = "Not in lexical scope"))
})

test_that("partial() is operationally idempotent", {
  f <- function(x, y, ..., z = 3) c(x, y, ..., z)

  expect_equal(
    departial(partial(partial(f), 1)),
    departial(partial(f, 1))
  )
  expect_equal(partial(partial(f), 1)(2), partial(f, 1)(2))
  expect_equal(partial(partial(f), 1)(2), c(1, 2, 3))

  expect_equal(
    departial(partial(partial(f, 1), 2)),
    departial(partial(f, 1, 2))
  )
  expect_equal(partial(partial(f, 1), 2)(), partial(f, 1, 2)())
  expect_equal(partial(partial(f, 1), 2)(), c(1, 2, 3))
})

test_that("arguments values are matched according to R's calling convention", {
  f <- function(x, yyy, ..., zzz = 0) c(x, yyy, ..., zzz)

  expect_equal(partial(f, x = 1)(2), c(1, 2, 0))
  expect_equal(partial(f, 1)(2),     c(1, 2, 0))

  # 'yyy' may be partially matched
  expect_all_equal(
    c(1, 2, 0),
    partial(f, x = 1, yyy = 2)(),
    partial(f, x = 1, y = 2)(),
    partial(f, x = 1, 2)(),
    partial(f, 2, x = 1)(),
    partial(f, yyy = 2, 1)(),
    partial(f, y = 2, 1)(),
    partial(f, 1, yyy = 2)(),
    partial(f, 1, y = 2)(),
    partial(f, 1, 2)()
  )

  # 'z' not matched to 'zzz', since 'zzz' follows '...'
  expect_all_equal(
    c(1, 2, z = 3, 0),
    partial(f, x = 1, yyy = 2, z = 3)(),
    partial(f, x = 1, y = 2, z = 3)(),
    partial(f, 1, 2, z = 3)(),
    partial(f, 1, z = 3, 2)(),
    partial(f, z = 3, 1, 2)(),
    partial(f, x = 1, z = 3, 2)(),
    partial(f, x = 1, 2, z = 3)(),
    partial(f, 2, x = 1, z = 3)(),
    partial(f, yyy = 2, z = 3, 1)(),
    partial(f, y = 2, z = 3, 1)(),
    partial(f, yyy = 2, 1, z = 3)(),
    partial(f, y = 2, 1, z = 3)(),
    partial(f, 1, yyy = 2, z = 3)(),
    partial(f, 1, y = 2, z = 3)()
  )

  # 'zzz' must be matched exactly, since it follows '...'
  expect_all_equal(
    c(1, 2, 3),
    partial(f, zzz = 3)(1, 2),
    partial(f, 1, zzz = 3)(2),
    partial(f, zzz = 3, 1)(2),
    partial(f, 1, 2, zzz = 3)(),
    partial(f, 1, zzz = 3, 2)(),
    partial(f, zzz = 3, 1, 2)()
  )
})

test_that("arguments values are matched across function calls", {
  f <- function(x, yyy, ..., zzz = 0) c(x, yyy, ..., zzz)

  partial_f <- local({
    top <- function(...) mid(...)
    mid <- function(...) bot(f, ...)
    bot <- function(...) partial(...)

    function(...) top(...)
  })

  expect_equal(partial_f(x = 1)(2), c(1, 2, 0))
  expect_equal(partial_f(1)(2),     c(1, 2, 0))

  # 'yyy' may be partially matched
  expect_all_equal(
    c(1, 2, 0),
    partial_f(x = 1, yyy = 2)(),
    partial_f(x = 1, y = 2)(),
    partial_f(x = 1, 2)(),
    partial_f(2, x = 1)(),
    partial_f(yyy = 2, 1)(),
    partial_f(y = 2, 1)(),
    partial_f(1, yyy = 2)(),
    partial_f(1, y = 2)(),
    partial_f(1, 2)()
  )

  # 'z' not matched to 'zzz', since 'zzz' follows '...'
  expect_all_equal(
    c(1, 2, z = 3, 0),
    partial_f(x = 1, yyy = 2, z = 3)(),
    partial_f(x = 1, y = 2, z = 3)(),
    partial_f(1, 2, z = 3)(),
    partial_f(1, z = 3, 2)(),
    partial_f(z = 3, 1, 2)(),
    partial_f(x = 1, z = 3, 2)(),
    partial_f(x = 1, 2, z = 3)(),
    partial_f(2, x = 1, z = 3)(),
    partial_f(yyy = 2, z = 3, 1)(),
    partial_f(y = 2, z = 3, 1)(),
    partial_f(yyy = 2, 1, z = 3)(),
    partial_f(y = 2, 1, z = 3)(),
    partial_f(1, yyy = 2, z = 3)(),
    partial_f(1, y = 2, z = 3)()
  )

  # 'zzz' must be matched exactly, since it follows '...'
  expect_all_equal(
    c(1, 2, 3),
    partial_f(zzz = 3)(1, 2),
    partial_f(1, zzz = 3)(2),
    partial_f(zzz = 3, 1)(2),
    partial_f(1, 2, zzz = 3)(),
    partial_f(1, zzz = 3, 2)(),
    partial_f(zzz = 3, 1, 2)()
  )
})

test_that("argument values are captured lazily (by default)", {
  expect_error(partial(identity, x = stop("!")), NA)
  expect_error(partial(identity, x = stop("!"))(), "!")
})

test_that("argument values are captured eagerly with unquoting", {
  rnd_value <- function() {
    is_value_set <<- TRUE
    runif(1)
  }
  is_value_set <- FALSE

  # `rnd_value()` captured lazily, so `is_value_set` remains FALSE
  f_lazy <- partial(identity, x = rnd_value())
  expect_identical(is_value_set, FALSE)

  # Random value generated anew each time
  out1 <- {set.seed(1); f_lazy()}
  out2 <- {set.seed(42); f_lazy()}
  expect_false(isTRUE(all.equal(out1, out2)))

  # Force `rnd_value()`
  f_eager <- partial(identity, x = !!rnd_value())
  expect_identical(is_value_set, TRUE)

  # Random value generated once, when `f_eager` was bound
  out1 <- {set.seed(1); f_eager()}
  out2 <- {set.seed(42); f_eager()}
  expect_true(isTRUE(all.equal(out1, out2)))
})

test_that("argument values are tidily evaluated", {
  env <- local({
    value <- "x"
    environment()
  })
  value <- local({
    value <- "y"
    quo(value)
  })

  f <- function(x, y) c(x, y)
  fp <- evalq(partial(f, x = value), env)
  fpp <- partial(fp, y = !!value)

  expect_identical(fpp(), c("x", "y"))
})

test_that("argument defaults are evaluated in the evaluation environment", {
  f <- function(x, y = x) {
    x <- x + 1
    c(x, y)
  }
  expect_equal(partial(f, 1)(), c(2, 2))
  expect_equal(partial(f, 1)(1), c(2, 1))
  expect_equal(partial(f, y = 2)(0), c(1, 2))
  expect_equal(partial(f, , 2)(0), c(1, 2))
})

test_that("argument values can be spliced and matched (#38)", {
  f <- function(x, y) c(x, y)
  out <- c(0, 1)

  fp <- partial(f, !!!list(x = 0))
  expect_identical(fp(1), out)

  fp <- partial(f, !!!list(0))
  expect_identical(fp(1), out)

  fp <- partial(f, !!!list(y = 1))
  expect_identical(fp(0), out)

  fp <- partial(f, !!!list(x = 0, y = 1))
  expect_identical(fp(), out)

  fp <- partial(f, !!!list(y = 1, x = 0))
  expect_identical(fp(), out)

  fp <- partial(f, !!!list(0, y = 1))
  expect_identical(fp(), out)

  fp <- partial(f, !!!list(y = 1, 0))
  expect_identical(fp(), out)

  fp <- partial(f, !!!list(x = 0, 1))
  expect_identical(fp(), out)

  fp <- partial(f, !!!list(1, x = 0))
  expect_identical(fp(), out)

  fp <- partial(f, !!!list(0, 1))
  expect_identical(fp(), out)
})

test_that("error is signaled when value to fix doesn't match an argument", {
  f <- function(x, y) NULL
  expect_errors_with_message(
    "unused argument",
    partial(f, 0, 1, 2),
    partial(f, z = 0)
  )
  expect_errors_with_message(
    NA,
    partial(f),
    partial(f, x = 0),
    partial(f, x = 0, y = 1)
  )
})

test_that("error is signaled when trying to fix a previously fixed argument", {
  f <- function(x, y, ...) NULL
  expect_errors_with_message(
    "can't set the value of a named '...' argument more than once",
    partial(partial(f, 1, a = 2), a = 2)
  )
  expect_errors_with_message(
    "formal argument \"x\" matched by multiple actual arguments",
    partial(partial(f, x = 1), x = 1),
    partial(partial(f, 1), x = 1)
  )
  expect_errors_with_message(
    "formal argument \"y\" matched by multiple actual arguments",
    partial(partial(f, x = 1, y = 2), y = 2)
  )
})

test_that("error is signaled when trying to fix a value that matches '...'", {
  f <- function(x, ...) NULL
  expect_errors_with_message(
    "only named arguments can be fixed",
    partial(f, 1, 2),
    partial(f, x = 1, 2),
    partial(f, 2, x = 1),
    partial(f, , 2)
  )
})

test_that("error is signaled when trying to call a fixed argument", {
  expect_error(partial(identity, x = 0)(x = 1), "unused argument \\(x = 1\\)")
})

test_that("formals are truncated and omit default values", {
  f <- function(x, y = x, ..., z = 0) NULL
  expect_equal(
    formals(partial(f)),
    formals(f)
  )
  expect_equal(
    formals(partial(f, x = 1)),
    formals(function(y, ..., z) {})
  )
  expect_equal(
    formals(partial(f, x = one)),
    formals(function(y, ..., z) {})
  )
  expect_equal(
    formals(partial(f, y = 2)),
    formals(function(x, ..., z) {})
  )
  expect_equal(
    formals(partial(f, z = 3)),
    formals(function(x, y, ...) {})
  )
  expect_equal(
    formals(partial(f, x = 1, y = 2)),
    formals(function(..., z) {})
  )
  expect_equal(
    formals(partial(f, x = 1, z = 3)),
    formals(function(y, ...) {})
  )
  expect_equal(
    formals(partial(f, y = 2, z = 3)),
    formals(function(x, ...) {})
  )
  expect_equal(
    formals(partial(f, x = 1, y = 2, z = 3)),
    formals(function(...) {})
  )
})

test_that("fixed arguments are not missing", {
  f <- function(x) {
    if (missing(x))
      x <- "x is missing"
    x
  }
  fix_x <- partial(f, x = "x is fixed")
  expect_equal(formals(fix_x), formals(function() {}))
  expect_identical(fix_x(), "x is fixed")
  expect_identical(f(), "x is missing")
})

test_that("function is not mutated, i.e., partial() has no side effects", {
  exprs_partial_fun <- alist(
    partial(f),
    partial(f, 0),
    partial(f, y = 0),
    partial(f, y = 0, 0),
    partial(f, a = 0)
  )
  expect_no_mutation_when_partializing <- function(...) {
    exprs_fun <- eval(substitute(alist(...)))
    for (expr in exprs_fun) {
      expr <- substitute(expr)
      f0 <- local({
        local <- "local"
        eval(expr)
      })
      lockEnvironment(environment(f0), bindings = TRUE)
      f <- f0
      for (expr_partial in exprs_partial_fun) {
        # No change in environment of f0 (which is locked)
        expect_error(eval(expr_partial), NA)
        # No mutation of formals, body or attributes of f0
        expect_true(identical(f, f0, ignore.bytecode = FALSE))
      }
    }
  }

  expect_no_mutation_when_partializing(
    function(x, ..., y = "y") local,
    partial(function(x, ..., y = "y") local, b = 0),
    partial(function(z, x, ..., y = "y") local, 0),
    partial(partial(function(z, x, ..., y = "y") local, 0), b = 0)
  )
})

test_that("partializing a composition partializes the first function applied", {
  f <- log

  vals <- {set.seed(1); runif(10, 0.5, 1)}

  for (. in seq_len(5)) {
    f <- top: f %>>>% identity
    fp <- partial(f, base = 2)
    fst <- unlist(as.list(fp))[[1]]

    expect_equal(fp(vals), log(vals, 2))
    expect_true(inherits(fp, "CompositeFunction"))
    expect_false(inherits(fp, "PartialFunction"))
    expect_true(inherits(fst, "PartialFunction"))
  }
})

test_that("departial() of a partial function is the closure of the original function", {
  f <- function(x, y) c(x, y)
  fp <- partial(f, x = 0)
  fpp <- partial(fp, y = 1)
  expect_identical(departial(fp), f)
  expect_identical(departial(fpp), f)

  cc <- closure(c)
  cp <- partial(c, a = 0)
  cpp <- partial(cp, b = 1)
  expect_equal(departial(cp), cc)
  expect_equal(departial(cpp), cc)
})

test_that("departial() is the identity for non-partial functions", {
  not_partial <- names(fn_kinds) != "partial"
  for (f in fn_kinds[not_partial])
    expect_identical(departial(f), f)
})

test_that("departializing a partialized composition departializes the first function", {
  f <- log

  vals <- {set.seed(1); runif(10, 0.5, 1)}

  for (. in seq_len(5)) {
    f <- top: f %>>>% identity
    dp <- departial(partial(f, base = 2))
    fst <- unlist(as.list(dp))[[1]]

    expect_equal(dp(vals), log(vals))
    expect_true(inherits(dp, "CompositeFunction"))
    expect_false(inherits(dp, "PartialFunction"))
    expect_false(inherits(fst, "PartialFunction"))
  }
})

test_that("error is signaled when applying departial() to a non-function", {
  foo <- quote(foo)
  expect_error(
    departial(foo),
    'Cannot interpret object of class "name" as a function'
  )
  expect_error(
    departial(NULL),
    'Cannot interpret object of class "NULL" as a function'
  )
})
