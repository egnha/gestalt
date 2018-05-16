context("Cached functions")

computing_value <- FALSE

f <- function(...) {
  computing_value <<- TRUE
  "computed value"
}

f_cached <- cache(f)

test_that("cached function has void formals", {
  expect_null(formals(f_cached))
})

test_that("value of cached function is computed at most once", {
  expect_identical(f_cached(), "computed value")
  expect_true(computing_value)

  computing_value <- FALSE

  for (. in seq_len(5)) {
    expect_identical(f_cached(), "computed value")
    expect_false(computing_value)
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
