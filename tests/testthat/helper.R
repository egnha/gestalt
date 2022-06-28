expectations <- function(expect) {
  expect <- substitute(expect)
  function(target, ...) {
    exprs <- eval(substitute(alist(...)))
    for (expr in exprs) {
      expectation <- bquote(.(expect)(.(expr), .(target)))
      eval.parent(expectation)
    }
  }
}

expect_errors_with_message <- expectations(expect_error)

expect_all_equal <- expectations(expect_equal)

expect_all_identical <- expectations(expect_identical)

# Pre-empt interpretation of unquoting operators
expect_equal_ <- function(object, expected, ...) {
  force(object)
  force(expected)
  expect_equal(object, expected, ...)
}

# testthat::expect_equivalent() was deprecated in the 3rd edition.
expect_equivalent <- function(...) {
  expect_equal(ignore_attr = TRUE, ...)
}
