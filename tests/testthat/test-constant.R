context("Constant functions")

val <- local({
  computing_value <- FALSE

  function(...) {
    computing_value <<- TRUE
    "value"
  }
})

const <- constant(val)

test_that("constant function has void formals", {
  expect_null(formals(const))
})

test_that("value of constant function is computed at most once", {
  expect_identical(const(), "value")
  expect_true(environment(val)$computing_value)

  environment(val)$computing_value <- FALSE

  for (. in seq_len(5)) {
    expect_identical(const(), "value")
    expect_false(environment(val)$computing_value)
  }
})

test_that("attributes aside from class are preserved", {
  non_class_attrs <- function(x) {
    attributes(x)[names(attributes(x)) != "class"]
  }

  attr(val, "anAttribute") <- "an attribute"
  attr(val, "anotherAttribute") <- "another attribute"
  const <- constant(val)
  expect_identical(non_class_attrs(const), non_class_attrs(val))
})

test_that("visibility of cached function value is preserved", {
  const <- constant(function() "visible")
  expect_identical(const(), "visible")
  expect_true(withVisible(const())$visible)

  const <- constant(function() invisible("invisible"))
  expect_identical(const(), "invisible")
  expect_false(withVisible(const())$visible)
})

test_that("constant() is idempotent", {
  const <- constant(function() NULL)
  expect_identical(constant(const), const)
})

test_that("variable() recovers the original function", {
  val_var <- variable(const)
  expect_identical(val_var, val)

  environment(val)$computing_value <- FALSE

  for (. in seq_len(5)) {
    expect_identical(val_var(), "value")
    expect_true(environment(val)$computing_value)
  }
})
