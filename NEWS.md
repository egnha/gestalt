# gestalt

## 0.0.0.9000

  * Compositions are available with the `compose()` function and its operator
    forms `%<<<%` and `%>>>%` (forward composition). Crucially, `compose()`
    is associative, both semantically and operationally, which prevents nested
    compositions from piling up. Additionally, `compose()` preserves the formals
    of the initial function called, i.e., `compose()` returns a function of the
    proper “type.” `decompose()` inverts `compose()` by returning the list of
    composite functions, enabling you to inspect and modify function
    compositions.

  * Partial application is available via the operator `partial()`, along with
    the operator `departial()` that retrieves the original function. Like
    genuine partial application, `partial()` contracts formals, rather than
    producing a function with indefinite formals `...`, i.e., `partial()`
    returns a function of the proper “type.” Quasiquotation and splicing of
    arguments are supported, in addition to argument-wise lazy or eager (tidy)
    evaluation.

  * `fn_()` is a variant of `fn()` that does _not_ comprehend quasiquotation.
    This provides a way, complementary to the use of `QUQ()` and `QUQS()`,
    to combine `fn()` syntax with literal unquoting and splicing, e.g.,
    `fn_(x ~ fn(f ~ f(!!x)))`.

  * Derived from nofrills 0.3.0.
