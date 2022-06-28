test_that("closure() is the identity on closures", {
  clsr <- function() NULL
  expect_identical(closure(clsr), clsr)
})

test_that("closure() convert primitives to closures, when they have args()", {
  # special
  xs <- {set.seed(1); runif(10)}
  expect_equal(closure(log)(xs), log(xs))

  # builtin
  expect_equal(closure(`+`)(xs, 1), xs + 1)
})

test_that("closure() converts certain primitives without args()", {
  expect_equal(closure(`:`)(1, 2), 1:2)
  expect_equal(closure(`&&`)(TRUE, TRUE), TRUE)
  expect_equal(closure(`&&`)(FALSE, TRUE), FALSE)
  expect_equal(closure(`||`)(TRUE, FALSE), TRUE)
  expect_equal(closure(`||`)(FALSE, FALSE), FALSE)
  expect_equal(closure(`[`)(mtcars, c("wt", "mpg")), mtcars[c("wt", "mpg")])
  expect_equal(closure(`[[`)(mtcars, "mpg"), mtcars[["mpg"]])
  expect_equal(closure(`[[<-`)(mtcars, "mpg", value = 0), `[[<-`(mtcars, "mpg", value = 0))
  expect_equal(closure(`[<-`)(mtcars, "mpg", value = 0), `[<-`(mtcars, "mpg", value = 0))
})

test_that("closure() raises error for certain primitives without args()", {
  for (prim in c(
    "(", "{", "$", "$<-", "@", "@<-", "<-", "<<-", "=", "~",
    "if", "for", "while", "break", "next", "repeat", "function", "return"
  )) {
    expect_error(closure(get(prim)))
  }
})

test_that("closure() formals agree with those of args()", {
  for (prim in union(names(.ArgsEnv), names(.GenericArgsEnv))) {
    prim <- get(prim)
    expect_equal(formals(closure(prim)), formals(args(prim)))
  }
})
