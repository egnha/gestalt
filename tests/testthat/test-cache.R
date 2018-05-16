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
  expect_equal(f_cached(), "computed value")
  expect_true(computing_value)

  computing_value <- FALSE

  for (. in seq_len(5)) {
    expect_equal(f_cached(), "computed value")
    expect_false(computing_value)
  }
})
