context("Dynamic scoping of lexical variables")

test_that("dots precede lexical variables in formals", {
  f <- posure(a, b = value ~ identity %>>>% NULL)
  expect_identical(formals(f), formals(function(..., a, b = value) NULL))
})

test_that("lexical variables can be referenced, dynamically, as arguments", {
  f <- posure(a, b = 1 ~ fn(x, y ~2^x * 3^y * 5^a * 7^b) %>>>% NULL)
  expect_equal(f(1, 2, a = 3), 2 * 3^2 * 5^3 * 7)
  expect_equal(f(1, 2, a = 3, b = 4), 2 * 3^2 * 5^3 * 7^4)
})

test_that("lexical variables are referenced lazily", {
  f <- posure(a, b = stop("!"), c ~ fn(x ~ x + a) %>>>% NULL)
  expect_equal(f(1, a = 2), 3)
  expect_equal(f(1, a = 2, c = stop("!")), 3)
})

test_that("enclosing environment can be set", {
  # By default, the enclosing environment is the calling environment
  b <- 1
  g <- posure(a ~ fn(x ~ 2^x * 3^a * 5^b) %>>>% NULL)
  expect_equal(g(1, a = 2), 2 * 3^2 * 5)

  env <- local({
    b <- 2
    environment()
  })
  g <- posure(a ~ fn(x ~ 2^x * 3^a * 5^b) %>>>% NULL, ..env = env)
  expect_equal(g(1, a = 2), 2 * 3^2 * 5^2)
})

test_that("quasiquotation is supported", {
  fs <- list(
    posure(a = !!"A", b ~ paste(a, b) %>>>% NULL),
    posure(a = "A", b ~ !!quote(paste(a, b) %>>>% NULL)),
    posure(!!"a" := "A", b ~ paste(a, b) %>>>% NULL),
    posure(!!!list(a = "A"), b ~ paste(a, b) %>>>% NULL),
    posure(!!!list(a = "A", b ~ paste(a, b) %>>>% NULL)),
    posure(a = "A", !!(b ~ paste(a, b) %>>>% NULL))
  )
  for (f in fs)
    expect_equal(f("x", b = "b"), "x A b")
})

test_that("error signaled if body is not a '%>>>%'-composite function", {
  expect_error(posure(a ~ identity %>>>% NULL), NA)

  msg <- "Posure body must be a composite function expressed using '%>>>%'"
  expect_error(posure(a ~ NULL),               msg)
  expect_error(posure(a ~ log),                msg)
  expect_error(posure(a ~ log(x, a)),          msg)
  expect_error(posure(a ~ function(...) NULL), msg)
  expect_error(posure(a ~ compose(identity)),  msg)
})
