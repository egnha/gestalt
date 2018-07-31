context("Dynamic scoping of lexical variables")

test_that("in formals, dots precede lexically scoped names", {
  f <- wrt(a ~ function(x) x + a)
  expect_identical(formals(f)[1L], alist(... = ))
})

test_that("unbound lexically scoped names can be referenced as arguments", {
  f <- wrt(a, b = 1 ~ function(x, y) 2^x * 3^y * 5^a * 7^b)
  expect_equal(f(1, 2, a = 3), 2 * 3^2 * 5^3 * 7)
  expect_equal(f(1, 2, a = 3, b = 4), 2 * 3^2 * 5^3 * 7^4)
})

test_that("enclosing environment can be set", {
  env <- local({
    b <- 1L
    environment()
  })
  f <- wrt(a ~ function(x) 2^x * 3^a * 5^b, ..env = env)
  expect_equal(f(1, a = 2), 2 * 3^2 * 5)

  # By default, the enclosing environment is the calling environment
  b <- 2L
  f <- wrt(a ~ function(x) 2^x * 3^a * 5^b)
  expect_equal(f(1, a = 2), 2 * 3^2 * 5^2)
})

test_that("quasiquotation is supported", {
  f <- wrt(a = !!"A", b ~ function(x) paste(x, a, b))
  expect_equal(f("x", b = "b"), "x A b")

  f <- wrt(!!!list(a = "A"), b ~ function(x) paste(x, a, b))
  expect_equal(f("x", b = "b"), "x A b")

  f <- wrt(!!"a" := "A", b ~ function(x) paste(x, a, b))
  expect_equal(f("x", b = "b"), "x A b")

  body <- quote(function(x) paste(x, a, b))
  f <- wrt(a = "A", b ~ !!body)
  expect_equal(f("x", b = "b"), "x A b")
})

test_that("error signaled if body doesn't evaluate to a function", {
  expect_error(wrt(a ~ function(x) x + a), NA)
  expect_error(wrt(a ~ NULL), "Body must evaluate to a function")
  expect_error(wrt(a ~ log(x)), "Body cannot be evaluated")
})
