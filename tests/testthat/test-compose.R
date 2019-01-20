make_funs <- function(n) {
  lapply(seq_len(n), function(i) {
    force(i)
    function(. = NULL) c(i, .)
  })
}

fs <- make_funs(3)

fn_kinds <- list(
  closure     = function(x) NULL,
  special     = log,
  builtin     = c,
  composition = compose(fs)
)

cmp <- function() {
  fs[[3]](fs[[2]](fs[[1]]()))
}

cmps <- list(
  compose(fs[[1]], fs[[2]], fs[[3]]),
  compose(compose(fs[[1]], fs[[2]]), fs[[3]]),
  compose(fs[[1]], compose(fs[[2]], fs[[3]])),
  fs[[1]] %>>>% fs[[2]] %>>>% fs[[3]],
  (!!(fs[[1]] %>>>% fs[[2]])) %>>>% fs[[3]],
  fs[[1]] %>>>% !!(fs[[2]] %>>>% fs[[3]])
)

context("Composing functions")

test_that("empty or NULL composition yields NULL", {
  expect_null(compose())
  expect_null(compose(NULL))
  expect_null(compose(list()))
  expect_null(compose(!!!list()))
})

test_that("NULL is dropped when composing", {
  expect_equal(compose(log), compose(NULL, log))
  expect_equal(compose(log), compose(log, NULL))
  expect_equal(compose(log), NULL %>>>% log)
  expect_equal(compose(log), log %>>>% NULL)

  inc <- function(x) x + 1
  cmp0 <- compose(inc, log)
  cmps <- list(
    compose(NULL, inc, log),
    compose(inc, NULL, log),
    compose(inc, log, NULL),
    inc %>>>% log %>>>% NULL,
    inc %>>>% NULL %>>>% log,
    NULL %>>>% inc %>>>% log
  )

  # Function equality by equality of return values and composite functions
  vals <- {set.seed(1); runif(10)}
  for (cmp in cmps) {
    expect_equal(cmp(vals), cmp0(vals))
    expect_equivalent(as.list(cmp), list(inc, log))
  }
})

test_that("error is signalled when composing a non-interpretable object", {
  errmsg <- function(x) {
    cls <- paste(deparse(class(x)), collapse = "")
    sprintf("Cannot interpret object of class %s as a function", cls)
  }

  noninterp <- list(
    as.data.frame(1:3),
    quote(x),
    quote(function() NULL),
    structure(NA, class = "foo")
  )

  for (obj in noninterp)
    expect_errors_with_message(
      errmsg(obj),
      compose(obj),
      compose(identity, obj),
      compose(obj, identity)
    )
})

test_that("composition is associative", {
  value <- cmp()
  expect_identical(value, 3:1)
  for (assoc in cmps)
    expect_identical(assoc(), value)
})

test_that("grouped compositions are flattened", {
  gs <- make_funs(4)

  # Test by call
  cmps <- list(
    compose(gs[[1]], gs[[2]], gs[[3]], gs[[4]]),
    compose(compose(gs[[1]], gs[[2]], gs[[3]]), gs[[4]]),
    compose(gs[[1]], compose(gs[[2]], gs[[3]], gs[[4]])),
    compose(compose(gs[[1]], gs[[2]]), compose(gs[[3]], gs[[4]])),
    gs[[1]] %>>>% gs[[2]] %>>>% gs[[3]] %>>>% gs[[4]],
    (!!(gs[[1]] %>>>% gs[[2]] %>>>% gs[[3]])) %>>>% gs[[4]],
    gs[[1]] %>>>% !!(gs[[2]] %>>>% gs[[3]] %>>>% gs[[4]]),
    (!!(gs[[1]] %>>>% gs[[2]])) %>>>% !!(gs[[3]] %>>>% gs[[4]])
  )
  for (cmp in cmps)
    expect_equivalent(as.list(cmp), gs)

  # Test by value
  cmps <- Reduce(compose, gs, init = NULL, accumulate = TRUE)[-1]
  for (i in seq_along(gs))
    expect_equivalent(as.list(cmps[[i]]), gs[seq_len(i)])
})

test_that("compositions are called as flattened pipeline", {
  foo <- inc: {. + 1} %>>>%
    out: (log %>>>% agg: sum %>>>% dbl: {2 * .})
  bar <- {. ^ 2} %>>>% foo
  baz <- bar: bar %>>>% dec: {. - 1}

  # Ensure that composition itself has expected return value
  vals <- {set.seed(1); runif(10, 1, 2)}
  expect_equal(baz(vals), 2 * sum(log(vals ^ 2 + 1)) - 1)

  # Body reveals flattened pipeline
  expect_length(unlist(as.list(baz)), 6)
  expect_identical(
    body(baz),
    quote(`__6__`(`__5__`(`__4__`(`__3__`(`__2__`(`__1__`(..., . = .)))))))
  )
})

test_that("for compose(), list of functions can be spliced", {
  cmp <- compose(fs[[1]], fs[[2]], fs[[3]])
  out <- cmp()

  expect_equal(compose(fs), cmp)
  expect_identical(compose(fs)(), out)

  expect_equal(compose(fs[[1]], fs[-1]), cmp)
  expect_identical(compose(fs[[1]], fs[-1])(), out)

  expect_equal(compose(fs[-3], fs[[3]]), cmp)
  expect_identical(compose(fs[-3], fs[[3]])(), out)
})

test_that("for compose(), list of functions can be spliced using `!!!`", {
  cmp <- compose(fs[[1]], fs[[2]], fs[[3]])
  out <- cmp()

  expect_equal(compose(!!!fs), cmp)
  expect_identical(compose(!!!fs)(), out)

  expect_equal(compose(fs[[1]], !!!fs[-1]), cmp)
  expect_identical(compose(fs[[1]], !!!fs[-1])(), out)

  expect_equal(compose(!!!fs[-3], fs[[3]]), cmp)
  expect_identical(compose(!!!fs[-3], fs[[3]])(), out)
})

test_that("for `%>>>%`, list of functions can be unquoted using `!!`", {
  cmp <- compose(fs[[1]], fs[[2]], fs[[3]])
  out <- cmp()

  expect_equal(NULL %>>>% !!fs, cmp)
  expect_identical((NULL %>>>% !!fs)(), out)

  expect_equal((!!fs) %>>>% NULL, cmp)
  expect_identical((NULL %>>>% !!fs)(), out)

  expect_equal(fs[[1]] %>>>% !!fs[-1], cmp)
  expect_identical((fs[[1]] %>>>% !!fs[-1])(), out)

  expect_equal((!!fs[-3]) %>>>% fs[[3]], cmp)
  expect_identical(((!!fs[-3]) %>>>% fs[[3]])(), out)
})

test_that("composition has formals of innermost function (as a closure)", {
  outer <- function(.) NULL
  fs <- list(
    closure = function(x, y, ..., z = "default") NULL,
    special = `[[`,
    builtin = `+`
  )
  for (inner in fs) {
    fmls_inner <- formals(closure(inner))
    expect_identical(formals(compose(inner, outer)), fmls_inner)
  }
})

test_that("environment of composition is child of inner-function environment", {
  fs <- c(
    fn_kinds[names(fn_kinds) != "composition"],
    local(function() NULL)
  )
  for (f in fs) {
    cmp <- compose(f, function(...) NULL)
    env <- if (is.null(environment(f))) baseenv() else environment(f)
    expect_identical(parent.env(environment(cmp)), env)
  }
})

test_that("composition operator obeys magrittr semantics (#39)", {
  # Insertion of initial '.', in case it doesn't appear among the arguments
  f0 <- function(x) {
    upper <- sprintf("%s", toupper(x[[1]]))
    paste(upper, collapse = "")
  }
  f1 <- {.[[1]]} %>>>% toupper %>>>% sprintf("%s", .) %>>>% paste(collapse = "")
  f2 <- {.[[1]]} %>>>% toupper() %>>>% sprintf("%s", .) %>>>% paste(collapse = "")

  x <- list(letters)
  expect_identical(f0(x), paste(LETTERS, collapse = ""))
  expect_identical(f1(x), f0(x))
  expect_identical(f2(x), f0(x))

  x <- mtcars
  expect_identical(f0(x), paste(mtcars[[1]], collapse = ""))
  expect_identical(f1(x), f0(x))
  expect_identical(f2(x), f0(x))

  # Anonymous function of '.' using {...}
  f0 <- function(x) log(abs(x) + 1)
  f1 <- abs %>>>% {. + 1} %>>>% log
  vals <- {set.seed(1); runif(10, -1, 1)}
  for (val in vals)
    expect_equal(f1(val), f0(val))

  f0 <- function(x) {
    out <- list(result = x)
    paste(out$result, collapse = "")
  }
  f1 <- {list(result = .)} %>>>% {paste(.$result, collapse = "")}
  expect_identical(f0(letters), paste(letters, collapse = ""))
  expect_identical(f1(letters), f0(letters))
})

test_that("when calling a composition, the point may assume any name (#10)", {
  f0 <- function(x, y, z = 2) c(x, y, z)
  fs <- list(
    f0(1)          %>>>% identity,
    f0(., 1)       %>>>% identity,
    f0(1, x = .)   %>>>% identity,
    {f0(., 1)}     %>>>% identity,
    {f0(1, x = .)} %>>>% identity
  )

  out <- f0(0, 1, 2)

  for (f in fs) {
    # The first argument will always match the point
    expect_identical(out, f(0))
    expect_identical(out, f(. = 0))

    # The original name can match the point
    expect_identical(out, f(x = 0))

    # In fact, any other name can match the point
    expect_identical(out, f(blah = 0))

    # Even other formal argument names can match the point (but don't do this!)
    expect_identical(out, f(z = 0))
  }
})

test_that("composition operator operands can be unquoted", {
  f <- list(log)
  inc <- 1
  f0 <- function(x) log(abs(x) + 1)
  f1 <- abs %>>>% {. + !!inc} %>>>% !!f[[1]]
  f2 <- (!!(abs %>>>% {. + !!inc})) %>>>% !!f[[1]]
  vals <- {set.seed(1); runif(10, -1, 1)}
  for (val in vals) {
    expect_equal(f1(val), f0(val))
    expect_equal(f2(val), f0(val))
  }
})

test_that("composition operator yields flattened compositions", {
  sq <- function(x) x^2
  id <- function(x) x

  p <- sq %>>>% sq
  q <- p %>>>% id %>>>% sq
  r <- q %>>>% id
  expect_equivalent(as.list(r), list(sq, sq, id, sq, id))
})

test_that("in pipeline, void call is interpreted as its caller", {
  id <- function(f) f
  cmps <- list(
    abs %>>>% log(),
    abs %>>>% id(log)()
  )
  for (cmp in cmps)
    expect_identical(log, as.list(cmp)[[2]])
})

test_that("error is signaled if void call in pipeline doesn't yield function", {
  foo <- quote(foo)

  expect_errors_with_message(
    "object 'foo' of mode 'function' was not found",
    abs %>>>% foo(),
    abs %>>>% identity(foo)()
  )
})

test_that("namespace operators are literally interpreted", {
  f <- stats::runif %>>>% base:::log(base = 2)
  expect_equal(
    {set.seed(1); f(10)},
    {set.seed(1); log(runif(10), base = 2)}
  )
})

test_that("parentheses evaluate and group", {
  . <- seq_len

  f <- identity(.) %>>>% (identity(.)) %>>>% (log2 %>>>% sum)

  expect_equal(
    f(10),
    sum(log2(seq_len(10)))
  )
  expect_equivalent(
    as.list(f),
    list(f[[1]], seq_len, list(log2, sum))
  )
})

test_that("subsetters are literally interpreted (#51)", {
  fs <- list(log, sum, exp, sq = function(x) x^2)
  vals <- {set.seed(1); runif(10, 1, 2)}

  f <- fs[[1]] %>>>% fs[2:3] %>>>% fs$sq
  expect_equal(f(vals), exp(sum(log(vals)))^2)
})

test_that("fn() is literally interpreted", {
  f <- fn(x ~ x^2) %>>>% log
  g <- log %>>>% fn(x ~ x^2)

  vals <- {set.seed(1); runif(10, 1, 2)}
  expect_equal(f(vals), log(vals ^ 2))
  expect_equal(g(vals), log(vals) ^ 2)
})

test_that("partial() is literally interpreted", {
  f <- partial(log, base = 2) %>>>% identity
  g <- identity %>>>% partial(log, base = 2)

  vals <- {set.seed(1); runif(10, 1, 2)}
  expect_equal(f(vals), log(vals, base = 2))
  expect_equal(g(vals), log(vals, base = 2))
})

test_that("functions in composition can be named", {
  f0 <- function(x) log(abs(x) + 1)

  nm <- "logarithm"
  fs <- list(
    compose(abs, inc = function(x) x + 1, !!nm := log),
    abs %>>>% inc: {. + 1} %>>>% !!nm: log
  )

  vals <- {set.seed(1); runif(10)}

  for (f in fs) {
    expect_equal(f(vals), f0(vals))

    pipeline <- as.list(f)
    expect_identical(names(pipeline), c("", "inc", "logarithm"))
    expect_equal(pipeline[[1]](vals), abs(vals))
    expect_equal(pipeline$logarithm(vals), log(vals))
    expect_equal(pipeline$inc(vals), vals + 1)
  }
})

test_that("error is signaled when implicit partialization is invalid (#43)", {
  f <- function(x, y) NULL
  notfun <- quote(notfun)

  expect_error(
    identity %>>>% f(a, b),
    "`f\\(\\., a, b\\)` is an invalid call"
  )
  expect_error(
    identity %>>>% f(z = .),
    "`f\\(z = \\.\\)` is an invalid call"
  )
  expect_error(
    identity %>>>% notfun(.),
    "object 'notfun' of mode 'function' was not found"
  )
})

test_that("unquoting operators can be literally expressed", {
  . <- list(4, 5)
  f <- list %>>>% {rlang::list2(QUQS(.), !!!.)}
  expect_equal(f(1, 2, 3), rlang::list2(1, 2, 3, 4, 5))

  x <- 1
  . <- 2
  f <- identity %>>>% {rlang::exprs(QUQ(.), !!.)}
  expect_equal(f(x), rlang::exprs(1, 2))
})

test_that("arguments matched by position/name before/after dots (#6)", {
  f <- (function(x, ...) list(x = x, ...)) %>>>% identity
  expect_identical(f(1), list(x = 1))
  expect_identical(f(1, 2), list(x = 1, 2))

  f <- (function(..., y = "y") list(..., y = y)) %>>>% identity
  expect_identical(f(1), list(1, y = "y"))
  expect_identical(f(y = "Y", 1), list(1, y = "Y"))

  f <- (function(x, ..., y = "y") list(x = x, ..., y = y)) %>>>% identity
  expect_identical(f(1), list(x = 1, y = "y"))
  expect_identical(f(1, 2), list(x = 1, 2, y = "y"))
  expect_identical(f(1, 2, y = "Y"), list(x = 1, 2, y = "Y"))

  # "degenerate" case
  f <- (function(...) list(...)) %>>>% identity
  expect_identical(f(x = 1, 2), list(x = 1, 2))
})

context("Decomposing compositions")

test_that("tree structure of composition preserved when converting to list", {
  f <- identity %>>>%
    mix: (sample %>>>% glue: paste) %>>>%
    (toupper %>>>% identity) %>>>%
    out: identity

  expect_equivalent(
    as.list(f),
    list(identity,
         mix = list(sample, glue = paste),
         list(toupper, identity),
         out = identity)
  )
})

test_that("as.list() inverts compose()", {
  expect_equivalent(as.list(compose(fs)), fs)
  expect_equivalent(as.list(compose(fs[[1]], fs[[2]], fs[[3]])), fs)
  expect_equivalent(as.list(compose(fs[[1]], fs[[2]])), fs[1:2])
})

test_that("compose() inverts as.list()", {
  cmp1 <- compose(fs)
  cmp2 <- compose(as.list(compose(fs)))

  # Test by call
  expect_equal(cmp1, cmp2)

  # Test by value
  vals <- {set.seed(1); runif(10)}
  for (val in vals)
    expect_equal(cmp1(val), cmp2(val))
})

context("Generic methods")

sq <- function(x) x^2
foo <- sq %>>>% inc:{. + 1} %>>>% log:log

vals <- {set.seed(1); runif(10, 1, 2)}

test_that("function in composition can be extracted by name", {
  expect_equal(foo$inc(vals), vals + 1)
  expect_equal(foo[["inc"]](vals), vals + 1)
  expect_equal(foo$log(vals), log(vals))
  expect_equal(foo[["log"]](vals), log(vals))
})

test_that("function in composition can be extracted by index list", {
  bar <- sq %>>>%
    inc: {. + 1} %>>>%
    out: (log %>>>% agg: (sum %>>>% dbl: {2 * .}))

  is_composite <- function(x) inherits(x, "CompositeFunction")

  expect_equal(bar[[list(1)]](vals), sq(vals))
  expect_all_equal(
    vals + 1,
    bar[[list(2)]](vals),
    bar[[list("inc")]](vals)
  )
  expect_all_equal(
    2 * sum(log(vals)),
    bar[[list(3)]](vals),
    bar[[list("out")]](vals)
  )
  expect_all_identical(
    FALSE,
    is_composite(bar[[list(1)]]),
    is_composite(bar[[list(2)]]),
    is_composite(bar[[list("inc")]])
  )
  expect_all_identical(
    TRUE,
    is_composite(bar[[list(3)]]),
    is_composite(bar[[list("out")]])
  )

  expect_all_equal(
    log(vals),
    bar[[list(3, 1)]](vals),
    bar[[list("out", 1)]](vals)
  )
  expect_all_equal(
    2 * sum(vals),
    bar[[list(3, 2)]](vals),
    bar[[list(3, "agg")]](vals),
    bar[[list("out", 2)]](vals),
    bar[[list("out", "agg")]](vals)
  )
  expect_all_identical(
    FALSE,
    is_composite(bar[[list(3, 1)]]),
    is_composite(bar[[list("out", 1)]])
  )
  expect_all_identical(
    TRUE,
    is_composite(bar[[list(3, 2)]]),
    is_composite(bar[[list(3, "agg")]]),
    is_composite(bar[[list("out", 2)]]),
    is_composite(bar[[list("out", "agg")]])
  )

  expect_all_equal(
    sum(vals),
    bar[[list(3, 2, 1)]](vals),
    bar[[list("out", 2, 1)]](vals),
    bar[[list(3, "agg", 1)]](vals),
    bar[[list("out", "agg", 1)]](vals)
  )
  expect_all_equal(
    2 * vals,
    bar[[list(3, 2, 2)]](vals),
    bar[[list("out", 2, 2)]](vals),
    bar[[list(3, "agg", 2)]](vals),
    bar[[list("out", "agg", 2)]](vals),
    bar[[list(3, 2, "dbl")]](vals),
    bar[[list("out", 2, "dbl")]](vals),
    bar[[list(3, "agg", "dbl")]](vals),
    bar[[list("out", "agg", "dbl")]](vals)
  )
  expect_all_identical(
    FALSE,
    is_composite(bar[[list(3, 2, 1)]]),
    is_composite(bar[[list("out", 2, 1)]]),
    is_composite(bar[[list(3, "agg", 1)]]),
    is_composite(bar[[list("out", "agg", 1)]]),
    is_composite(bar[[list(3, 2, 2)]]),
    is_composite(bar[[list("out", 2, 2)]]),
    is_composite(bar[[list(3, "agg", 2)]]),
    is_composite(bar[[list("out", "agg", 2)]]),
    is_composite(bar[[list(3, 2, "dbl")]]),
    is_composite(bar[[list("out", 2, "dbl")]]),
    is_composite(bar[[list(3, "agg", "dbl")]]),
    is_composite(bar[[list("out", "agg", "dbl")]])
  )
})

test_that("compositions can be filtered by name", {
  expect_equal(foo["inc"](vals), vals + 1)
  expect_equal(foo["log"](vals), log(vals))
  expect_equal(foo[c("inc", "log")](vals), log(vals + 1))
  expect_equal(foo[c("log", "inc")](vals), log(vals) + 1)
})

test_that("compositions can be filtered by position", {
  expect_equal(foo[1](vals), sq(vals))
  expect_equal(foo[2](vals), vals + 1)
  expect_equal(foo[3](vals), log(vals))
  expect_equal(foo[-1](vals), log(vals + 1))
  expect_equal(foo[-2](vals), log(sq(vals)))
  expect_equal(foo[-3](vals), sq(vals) + 1)
  expect_equal(foo[c(1, 2)](vals), sq(vals) + 1)
  expect_equal(foo[c(2, 1)](vals), sq(vals + 1))
  expect_equal(foo[c(1, 3)](vals), log(sq(vals)))
  expect_equal(foo[c(3, 1)](vals), sq(log(vals)))
  expect_equal(foo[c(2, 3)](vals), log(vals + 1))
  expect_equal(foo[c(3, 2)](vals), log(vals) + 1)
  expect_equal(foo[-c(1, 2)](vals), log(vals))
  expect_equal(foo[-c(1, 3)](vals), vals + 1)
  expect_equal(foo[-c(2, 3)](vals), sq(vals))
  expect_equal(foo[c(1, 2, 3)](vals), log(sq(vals) + 1))
  expect_equal(foo[c(1, 3, 2)](vals), log(sq(vals)) + 1)
  expect_equal(foo[c(2, 3, 1)](vals), sq(log(vals + 1)))
  expect_equal(foo[c(2, 1, 3)](vals), log(sq(vals + 1)))
  expect_equal(foo[c(3, 1, 2)](vals), sq(log(vals)) + 1)
  expect_equal(foo[c(3, 2, 1)](vals), sq(log(vals) + 1))
})

test_that("compositions can be filtered by predicate", {
  expect_null(foo[c(F, F, F)])
  expect_equal(foo[c(F, F, T)](vals), log(vals))
  expect_equal(foo[c(F, T, F)](vals), vals + 1)
  expect_equal(foo[c(F, T, T)](vals), log(vals + 1))
  expect_equal(foo[c(T, F, F)](vals), sq(vals))
  expect_equal(foo[c(T, F, T)](vals), log(sq(vals)))
  expect_equal(foo[c(T, T, F)](vals), sq(vals) + 1)
  expect_equal(foo[c(T, T, T)](vals), log(sq(vals) + 1))
})

test_that("can take head or tail of a composition", {
  f <- {. + 1} %>>>% log %>>>% sum

  expect_equal(head(f, 4)(vals), sum(log(vals + 1)))
  expect_equal(head(f, 3)(vals), sum(log(vals + 1)))
  expect_equal(head(f, 2)(vals), log(vals + 1))
  expect_equal(head(f, 1)(vals), vals + 1)
  expect_equal(head(f)(vals), vals + 1)
  expect_null(head(f, 0))
  expect_equal(head(f, -1)(vals), log(vals + 1))
  expect_equal(head(f, -2)(vals), vals + 1)
  expect_null(head(f, -3))
  expect_null(head(f, -4))

  expect_equal(tail(f, 4)(vals), sum(log(vals + 1)))
  expect_equal(tail(f, 3)(vals), sum(log(vals + 1)))
  expect_equal(tail(f, 2)(vals), sum(log(vals)))
  expect_equal(tail(f, 1)(vals), sum(vals))
  expect_equal(tail(f)(vals), sum(vals))
  expect_null(tail(f, 0))
  expect_equal(tail(f, -1)(vals), sum(log(vals)))
  expect_equal(tail(f, -2)(vals), sum(vals))
  expect_null(tail(f, -3))
  expect_null(tail(f, -4))
})

test_that("error signaled when predicate is of unequal length", {
  expect_error(
    foo[c(T, T, T, T)],
    "Predicate length \\(4\\) must be 3"
  )
})

test_that("filtering composition out-of-bounds yields NULL", {
  expect_null(foo[0])
  expect_null(foo[length(foo) + 1])
  expect_null(foo$nonexistent)
  expect_null(foo[["nonexistent"]])
})

test_that("compositions can be replaced by name", {
  bar <- sq %>>>% inc:{. + 1} %>>>% log:log

  bar$log <- NULL
  expect_equal(bar(vals), sq(vals) + 1)

  bar$inc <- sin
  expect_equal(bar(vals), sin(sq(vals)))

  bar[["inc"]] <- cos
  expect_equal(bar(vals), cos(sq(vals)))

  bar[["inc"]] <- NULL
  expect_equal(bar(vals), sq(vals))
})

test_that("compositions can be replaced by index", {
  bar <- sq %>>>% inc:{. + 1} %>>>% log:log

  bar[[3]] <- NULL
  expect_equal(bar(vals), sq(vals) + 1)

  bar[[2]] <- sin
  expect_equal(bar(vals), sin(sq(vals)))
})

test_that("compositions can be replaced by index list", {
  bar <- sq %>>>%
    inc: {. + 1} %>>>%
    out: (log %>>>% agg: (sum %>>>% dbl: {2 * .}))

  bar[[list(1)]] <- identity
  expect_equal(bar(vals), 2 * sum(log(vals + 1)))
  bar[[list(1)]] <- sq

  bar[[list(2)]] <- function(x) 2 * x
  expect_equal(bar(vals), 2 * sum(log(2* sq(vals))))
  bar[[list(2)]] <- function(x) x + 1

  bar[[list("inc")]] <- function(x) 2 * x
  expect_equal(bar(vals), 2 * sum(log(2* sq(vals))))
  bar[[list("inc")]] <- function(x) x + 1

  bar[[list(3)]] <- identity
  expect_equal(bar(vals), sq(vals) + 1)
  bar[[list(3)]] <- log %>>>% agg: (sum %>>>% dbl: {2 * .})

  bar[[list("out")]] <- identity
  expect_equal(bar(vals), sq(vals) + 1)
  bar[[list("out")]] <- log %>>>% agg: (sum %>>>% dbl: {2 * .})

  bar[[list(3, 1)]] <- sin
  expect_equal(bar(vals), 2 * sum(sin(sq(vals) + 1)))
  bar[[list(3, 1)]] <- log

  bar[[list("out", 1)]] <- sin
  expect_equal(bar(vals), 2 * sum(sin(sq(vals) + 1)))
  bar[[list("out", 1)]] <- log

  bar[[list(3, 2)]] <- sum
  expect_equal(bar(vals), sum(log(sq(vals) + 1)))
  bar[[list(3, 2)]] <- sum %>>>% dbl: {2 * .}

  bar[[list("out", 2)]] <- sum
  expect_equal(bar(vals), sum(log(sq(vals) + 1)))
  bar[[list("out", 2)]] <- sum %>>>% dbl: {2 * .}

  bar[[list(3, "agg")]] <- sum
  expect_equal(bar(vals), sum(log(sq(vals) + 1)))
  bar[[list(3, "agg")]] <- sum %>>>% dbl: {2 * .}

  bar[[list("out", "agg")]] <- sum
  expect_equal(bar(vals), sum(log(sq(vals) + 1)))
  bar[[list("out", "agg")]] <- sum %>>>% dbl: {2 * .}

  bar[[list(3, 2, 1)]] <- function(x) sum(x ^ 2)
  expect_equal(bar(vals), 2 * sum(log(sq(vals) + 1) ^ 2))
  bar[[list(3, 2, 1)]] <- sum

  bar[[list("out", 2, 1)]] <- function(x) sum(x ^ 2)
  expect_equal(bar(vals), 2 * sum(log(sq(vals) + 1) ^ 2))
  bar[[list("out", 2, 1)]] <- sum

  bar[[list(3, "agg", 1)]] <- function(x) sum(x ^ 2)
  expect_equal(bar(vals), 2 * sum(log(sq(vals) + 1) ^ 2))
  bar[[list(3, "agg", 1)]] <- sum

  bar[[list("out", "agg", 1)]] <- function(x) sum(x ^ 2)
  expect_equal(bar(vals), 2 * sum(log(sq(vals) + 1) ^ 2))
  bar[[list("out", "agg", 1)]] <- sum

  bar[[list(3, 2, 2)]] <- function(x) 3 * x
  expect_equal(bar(vals), 3 * sum(log(sq(vals) + 1)))
  bar[[list(3, 2, 2)]] <- sum

  bar[[list(3, "agg", 2)]] <- function(x) 3 * x
  expect_equal(bar(vals), 3 * sum(log(sq(vals) + 1)))
  bar[[list(3, "agg", 2)]] <- sum

  bar[[list("out", 2, 2)]] <- function(x) 3 * x
  expect_equal(bar(vals), 3 * sum(log(sq(vals) + 1)))
  bar[[list("out", 2, 2)]] <- sum

  bar[[list("out", "agg", 2)]] <- function(x) 3 * x
  expect_equal(bar(vals), 3 * sum(log(sq(vals) + 1)))
  bar[[list("out", "agg", 2)]] <- sum

  bar[[list(3, 2, "dbl")]] <- function(x) 3 * x
  expect_equal(bar(vals), 3 * sum(log(sq(vals) + 1)))
  bar[[list(3, 2, "dbl")]] <- sum

  bar[[list(3, "agg", "dbl")]] <- function(x) 3 * x
  expect_equal(bar(vals), 3 * sum(log(sq(vals) + 1)))
  bar[[list(3, "agg", "dbl")]] <- sum

  bar[[list("out", 2, "dbl")]] <- function(x) 3 * x
  expect_equal(bar(vals), 3 * sum(log(sq(vals) + 1)))
  bar[[list("out", 2, "dbl")]] <- sum

  bar[[list("out", "agg", "dbl")]] <- function(x) 3 * x
  expect_equal(bar(vals), 3 * sum(log(sq(vals) + 1)))
  bar[[list("out", "agg", "dbl")]] <- sum
})

test_that("compositions can be replaced by filter", {
  vals <- {set.seed(1); runif(10)}

  bar <- sq %>>>% inc:{. + 1} %>>>% log:log

  # By position
  bar[1] <- list(function(x) x ^ 3)
  expect_equal(bar(vals), log(vals ^ 3 + 1))

  bar[3:2] <- list(sin, cos)
  expect_equal(bar(vals), sin(cos(vals ^ 3)))

  bar[3:2] <- list(sq)
  expect_equal(bar(vals), sq(sq(vals ^ 3)))

  # By name
  bar["inc"] <- list(function(x) x + 2)
  expect_equal(bar(vals), sq(vals ^ 3 + 2))

  bar[c("log", "inc")] <- list(log, function(x) x + 1)
  expect_equal(bar(vals), log(vals ^ 3 + 1))

  bar[c("log", "inc")] <- list(sin)
  expect_equal(bar(vals), sin(sin(vals ^ 3)))

  # By predicate
  bar[c(TRUE, FALSE, FALSE)] <- list(sq)
  expect_equal(bar(vals), sin(sin(sq(vals))))

  bar[c(TRUE, FALSE, TRUE)] <- list(function(x) x + 1, cos)
  expect_equal(bar(vals), cos(sin(vals + 1)))

  bar[c(TRUE, FALSE, TRUE)] <- list(sq)
  expect_equal(bar(vals), sq(sin(sq(vals))))

  # Blanket replacement
  bar[] <- list(sq, function(x) x + 1, sum)
  expect_equal(bar(vals), sum(vals ^ 2 + 1))

  bar[] <- list(sq)
  expect_equal(bar(vals), sq(sq(sq(vals))))
})

test_that("non-singleton recycling is disallowed when replacing by filter", {
  bar <- sq:sq %>>>% inc:{. + 1} %>>>% log:log %>>>% out:identity

  value <- list(sq, sq)
  expect_error(
    bar[] <- value,
    "Replacement length \\(2\\) must be 1 or 4"
  )
  expect_errors_with_message(
    "Replacement length \\(2\\) must be 1 or 3",
    bar[c(3, 4, 1)] <- value,
    bar[c("inc", "log", "sq")] <- value,
    bar[c(TRUE, FALSE, TRUE, TRUE)] <- value
  )

  value <- list(sq, sq, sq, sq, sq)
  expect_error(
    bar[] <- value,
    "Replacement length \\(5\\) must be 1 or 4"
  )
  expect_errors_with_message(
    "Replacement length \\(5\\) must be 1 or 3",
    bar[c(3, 4, 1)] <- value,
    bar[c("inc", "log", "sq")] <- value,
    bar[c(TRUE, FALSE, TRUE, TRUE)] <- value
  )
})

test_that("composition names are in call-order", {
  expect_named(foo, c("", "inc", "log"))
})

test_that("unnamed compositions have empty-string names", {
  expect_named(abs %>>>% log %>>>% sin, c("", "", ""))
})

test_that("compositions can be renamed", {
  bar <- sq %>>>% inc:{. + 1} %>>>% log:log

  names(bar) <- c("square", "increment", "logarithm")
  expect_named(bar, c("square", "increment", "logarithm"))
  expect_equal(bar$square(vals), sq(vals))
  expect_equal(bar$increment(vals), vals + 1)
  expect_equal(bar$logarithm(vals), log(vals))

  names(bar) <- NULL
  expect_named(bar, rep("", length(bar)))
})

test_that("composition length is the number of top-level component functions", {
  expect_length(compose(sin), 1)
  expect_all_identical(
    2L,
    length(sin %>>>% cos),
    length(sin %>>>% snd: (cos %>>>% tan)),
    length(fst: (sin %>>>% cos) %>>>% tan),
    length((sin %>>>% cos) %>>>% tan),
    length(sin %>>>% (cos %>>>% tan))
  )
  expect_all_identical(
    3L,
    length(sin %>>>% cos %>>>% tan),
    length((sin %>>>% cos) %>>>% tan %>>>% identity)
  )
})

test_that("unlist() flattens the list of composite functions", {
  # Default values: recursive = TRUE, use.names = TRUE
  expect_identical(
    unlist(abs: abs %>>>% (log %>>>% (sum: sum %>>>% identity))),
    list(abs = abs, log, sum = sum, identity)
  )
  expect_identical(
    unlist(abs: abs %>>>% (log %>>>% (sum: sum %>>>% identity)),
           recursive = TRUE, use.names = TRUE),
    list(abs = abs, log, sum = sum, identity)
  )
  expect_identical(
    unlist(abs: abs %>>>% (log %>>>% (sum: sum %>>>% identity)),
           recursive = FALSE),
    list(abs = abs, log, list(sum = sum, identity))
  )
  expect_identical(
    unlist(abs: abs %>>>% (log %>>>% (sum: sum %>>>% identity)),
           use.names = FALSE),
    list(abs, log, sum, identity)
  )
  expect_identical(
    unlist(abs: abs %>>>% (log %>>>% (sum: sum %>>>% identity)),
           recursive = FALSE, use.names = FALSE),
    list(abs, log, list(sum = sum, identity))
  )
})
