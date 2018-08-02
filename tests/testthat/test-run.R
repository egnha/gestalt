context("Running actions in ordered contexts")

test_that("context can inherit from a list or data frame", {
  cxt <- let(list(a = 1), b = a + 1)
  expect_equal(run(cxt, c(a, b)), c(1, 2))

  cxt <- let(mtcars, mpg = mpg, half = wt / 2)
  expect_equal(run(cxt, list(mpg, half)), list(mtcars$mpg, mtcars$wt / 2))
})

test_that("names in context resolve from right to left", {
  a <- "a"
  cxt <- let(a = paste0(a, "a"), b = paste0(a, "b"), c = paste0(a, b, "c"))
  expect_equal(run(cxt, c(a, b, c)), c("aa", "aab", "aaaabc"))

  cxt <- let(cxt, a = "new a")
  expect_equal(run(cxt, c(a, b, c)), c("new a", "aab", "aaaabc"))
})

test_that("names in context are lazily evaluated", {
  b <- "ok"
  expect_error(let(a = b, b = stop("!")), NA)
  expect_equal(run(let(a = b, b = stop("!")), a), "ok")
  expect_error(run(let(a = b, b = stop("!")), b), "!")
})

test_that("contexts are composable", {
  expect_equal(run(let(a = 1), a), 1)
  expect_equal(run(let(let(a = 1), a = a + 1), a), 2)
})

test_that("named bound by run() override names in the given context", {
  cxt <- let(a = "a")
  expect_equal(run(cxt, a), "a")
  expect_equal(run(cxt, a, a = "new a"), "new a")
})
