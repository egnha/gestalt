context("Printing")

# Suppress ANSI escape codes in printed output
local_test_context(.env = parent.frame())
local_reproducible_output(crayon = FALSE, .env = parent.frame())

test_that("partially applied function shows function with arguments fixed", {
  replace <- TRUE
  draw <- function(x, size = length(x), replace = FALSE, prob = NULL) {
    sample(x, size, replace, prob)
  }
  draw_letters <- partial(draw, letters, replace = !!replace)
  out <- c(
    "<Partially Applied Function>",
    "",
    "function(size = length(^letters), prob = NULL) {",
    "  draw(x = ^letters, size = size, replace = ^TRUE, prob = prob)",
    "}",
    "",
    "Recover the inner function with 'departial()'."
  )
  expect_identical(capture.output(print(draw_letters)), out)
})

test_that("composition of functions shows composite functions", {
  f <- abs %>>>%
    inc: !!partial(`+`, 1) %>>>%
    res: (log %>>>% agg: sum)
  out <- c(
    "<Function Composition>",
    "In order of application:",
    "",
    "[[1]]",
    "  function (x)  .Primitive(\"abs\")",
    "",
    "[[\"inc\"]]",
    "  <Partially Applied Function>",
    "  function(e2) {",
    "    (^1) + e2",
    "  }",
    "",
    "[[\"res\"]][[1]]",
    "  function (x, base = exp(1))  .Primitive(\"log\")",
    "",
    "[[\"res\"]][[\"agg\"]]",
    "  function (..., na.rm = FALSE)  .Primitive(\"sum\")",
    "",
    "Recover the list of functions with 'as.list()'."
  )
  expect_identical(capture.output(print(f)), out)
})

test_that("constant function prints according to its original class", {
  three <- constant(partial(`+`, 1, 2))
  out <- c(
    "Constant Function:",
    "<Partially Applied Function>",
    "",
    "function() {",
    "  (^1) + (^2)",
    "}",
    "",
    "Recover the inner function with 'departial()'."
  )
  expect_identical(capture.output(print(three)), out)

  void <- constant(list %>>>% length)
  out <- c(
    "Constant Function:",
    "<Function Composition>",
    "In order of application:",
    "",
    "[[1]]",
    "  function (...)  .Primitive(\"list\")",
    "",
    "[[2]]",
    "  function (x)  .Primitive(\"length\")",
    "",
    "Recover the list of functions with 'as.list()'."
  )
  expect_identical(capture.output(print(void)), out)
})

test_that("posure shows composite-function expression", {
  f <- posure(base = 2, n ~ {
    sample %>>>%
      log(base = base) %>>>%
      rep(n)
  })
  out <- c(
    "<Posure>",
    "",
    "function (..., base = 2, n) ",
    "{",
    "    (sample %>>>% log(base = base) %>>>% rep(n))(...)",
    "}"
  )
  out_f <- capture.output(print(f))
  # Strip ephemeral "<environment:...>", if present
  if (grepl("^<environment", out_f[length(out_f)]))
    out_f <- out_f[-length(out_f)]
  expect_identical(out_f, out)
})

test_that("let shows top environment and ordered bindings", {
  env <- new.env()
  out_env <- capture.output(print(env))
  cxt <- let(let(env, a = foo), b = bar)
  out <- c(
    "<Ordered Context>",
    "",
    "* Topmost environment:",
    paste0("\ \ ", out_env),
    "",
    "* Named expressions (resolved from the bottom up):",
    "  a: foo",
    "  b: bar"
  )
  out_cxt <- capture.output(print(cxt))
  expect_identical(out_cxt, out)
})
