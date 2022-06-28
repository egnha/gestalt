# Suppress ANSI escape codes in printed output
local_test_context(.env = parent.frame())
local_reproducible_output(crayon = FALSE, .env = parent.frame())

test_that("partially applied function shows function with arguments fixed", {
  replace <- TRUE
  draw <- function(x, size = length(x), replace = FALSE, prob = NULL) {
    sample(x, size, replace, prob)
  }
  expect_snapshot(partial(draw, letters, replace = !!replace))
})

test_that("composition of functions shows composite functions", {
  expect_snapshot(abs %>>>%
                    inc: !!partial(`+`, 1) %>>>%
                    res: (log %>>>% agg: sum))
})

test_that("constant function prints according to its original class", {
  expect_snapshot(constant(partial(`+`, 1, 2)))
  expect_snapshot(constant(list %>>>% length))
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
