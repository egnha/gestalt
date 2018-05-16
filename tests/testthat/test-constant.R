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

test_that("variable() recovers the original function", {
  val_var <- variable(const)
  expect_identical(val_var, val)

  environment(val)$computing_value <- FALSE

  for (. in seq_len(5)) {
    expect_identical(val_var(), "value")
    expect_true(environment(val)$computing_value)
  }
})
