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

test_that("let() comprehends quasiquotation", {
  cxt <- let(a = !!"a")
  expect_equal(run(cxt, a), "a")

  # 'NULL' context resolves to calling environment
  cxt <- let(NULL, !!"a" := "a")
  expect_equal(run(cxt, a), "a")

  cxt <- let(NULL, !!!alist(a = "a", b = "b"))
  expect_equal(run(cxt, c(a, b)), c("a", "b"))

  b <- "b"
  cxt <- let(NULL, a = !!b, b = "new b")
  expect_equal(run(cxt, c(a, b)), c("b", "new b"))
})

test_that("run() comprehends quasiquotation", {
  one <- 1
  expect_equal(run(NULL, a + !!one, a = 2, one = stop("!")), 3)
  expect_equal(run(NULL, c(a, !!!c(2, 3)), a = 1), c(1, 2, 3))
})

test_that("context names are in the given order", {
  cxt <- let(a = 1, b = a, c = a + b)
  expect_named(cxt, c("a", "b", "c"))
  expect_named(let(cxt, d = d), c(names(cxt), "d"))
})
