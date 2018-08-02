# gestalt

## 0.1.4

### New features

  * `posure()` is a means of creating _efficient_ variable (i.e., parameterized)
    composite functions.
    
    In particular, this addresses a shortcoming of the use of the magrittr `%>%`
    in functions. For instead of writing
    ```
    function(..., b = 2, n) {
      sample(...) %>% log(base = b) %>% rep(n)
    }
    ```
    which is inefficient because the function chain is created anew with each
    call, you can more directly write
    ```
    posure(b = 2, n ~ {
      sample %>>>% log(base = b) %>>>% rep(n)
    })
    ```
    Not only is the `posure()` version more succinct (you don’t need to specify
    a placeholder ‘`...`’), it is also robuster, and faster than the version
    with `%>%`, thanks to a non-standard scoping mechanism under the hood.
    
  * `let()` enables you to create **contexts**: local environments in which
    named expressions are _lazily_ resolved in a given order. Tidyverse
    quasiquotation of expressions is supported, allowing you to exercise
    fine-grained control over the evaluation of subexpressions.
    
  * As a companion to `let()`, `run()` evaluates an expression relative to a
    context. Unlike `base::with()`, `run()` supports quasiquotation and provides
    a means of overriding bindings in the given context.
  
### Minor improvements

  * When calling a composite function, the point (`.`) in an implicitly curried
    function may now assume any name (#10). This is useful when you want to call
    the argument assumed by the point by its original name, e.g., in a
    `do.call()` or `lapply()` invocation.
    
  * `partial()` is now literally interpreted by `%>>>%` (#11). For instance, you
     you can succinctly write `abs %>>>% partial(log, base = 2)` instead of
     `abs %>>>% !!partial(log, base = 2)`.

## 0.1.2

  * In a composite function, default argument values following `...` are no
    longer absorbed by `...` (#6).
    
  * Improvements to the documentation throughout.

## 0.1.1

  * Initial release.
  
  * `fn()` is extracted from nofrills 0.3.0.
