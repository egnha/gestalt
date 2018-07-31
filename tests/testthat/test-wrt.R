context("Dynamic scoping of lexical variables")

test_that("dots precede lexical variables in formals", {
  f <- wrt(a, b = value ~ identity)
  expect_identical(formals(f), formals(function(..., a, b = value) NULL))
})

test_that("lexical variables can be referenced, dynamically, as arguments", {
  f <- wrt(a, b = 1 ~ function(x, y) 2^x * 3^y * 5^a * 7^b)
  expect_equal(f(1, 2, a = 3), 2 * 3^2 * 5^3 * 7)
  expect_equal(f(1, 2, a = 3, b = 4), 2 * 3^2 * 5^3 * 7^4)
})

test_that("lexical variables are referenced lazily", {
  f <- wrt(a, b = stop("!"), c ~ function(x) x + a)
  expect_equal(f(1, a = 2), 3)
  expect_equal(f(1, a = 2, c = stop("!")), 3)
})

test_that("enclosing environment can be set", {
  # By default, the enclosing environment is the calling environment
  b <- 1
  g <- wrt(a ~ function(x) 2^x * 3^a * 5^b)
  expect_equal(g(1, a = 2), 2 * 3^2 * 5)

  env <- local({
    b <- 2
    environment()
  })
  g <- wrt(a ~ function(x) 2^x * 3^a * 5^b, ..env = env)
  expect_equal(g(1, a = 2), 2 * 3^2 * 5^2)
})

test_that("quasiquotation is supported", {
  fs <- list(
    unquote_value = wrt(a = !!"A", b ~ function(x) paste(x, a, b)),
    unquote_body  = wrt(a = "A", b ~ !!quote(function(x) paste(x, a, b))),
    unquote_name  = wrt(!!"a" := "A", b ~ function(x) paste(x, a, b)),
    splice_args   = wrt(!!!list(a = "A"), b ~ function(x) paste(x, a, b))
  )
  for (f in fs)
    expect_equal(f("x", b = "b"), "x A b")
})

test_that("error signaled if body is not a function", {
  expect_error(wrt(a ~ function() NULL), NA)
  expect_error(wrt(a ~ NULL), "Body must be a function")
  expect_error(wrt(a ~ log(x)), "Body cannot be evaluated")
})
