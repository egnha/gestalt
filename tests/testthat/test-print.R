context("Printing")

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
    "  function(.y) {",
    "    (^1) + .y",
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
