context("Cached functions")

f <- local({
  computing_value <- FALSE

  function(...) {
    computing_value <<- TRUE
    "value"
  }
})

f_cached <- cache(f)

test_that("cached function has void formals", {
  expect_null(formals(f_cached))
})

test_that("value of cached function is computed at most once", {
  expect_identical(f_cached(), "value")
  expect_true(environment(f)$computing_value)

  environment(f)$computing_value <- FALSE

  for (. in seq_len(5)) {
    expect_identical(f_cached(), "value")
    expect_false(environment(f)$computing_value)
  }
})

test_that("attributes aside from class are preserved", {
  non_class_attrs <- function(x) {
    attributes(x)[names(attributes(x)) != "class"]
  }

  attr(f, "anAttribute") <- "an attribute"
  attr(f, "anotherAttribute") <- "another attribute"
  f_cached <- cache(f)
  expect_identical(non_class_attrs(f_cached), non_class_attrs(f))
})

test_that("uncaching recovers the original function", {
  f_uncached <- uncache(f_cached)
  expect_identical(f_uncached, f)

  environment(f)$computing_value <- FALSE

  for (. in seq_len(5)) {
    expect_identical(f_uncached(), "value")
    expect_true(environment(f)$computing_value)
  }
})
