# gestalt

## 0.1.5.9000

 * The minimum required rlang version has been bumped to 0.3.1. This version
   fixed a bug which prevented certain operator "sections" from being expressed.
   For example, a chain like `` `/`(2) %>>>% sin`` (halve and apply sine) now
   works as expected.
 
 * Testing on R 3.1 has been dropped. (It may still work.)

## 0.1.5

  * Fixed a segfault caused by leakage of rlang internals (thanks @lionel-).
  
  * `names()` now gets the names of bindings in a context (as made by `let()`).

## 0.1.4

### New features

  * `posure()` is a means of creating _efficient_ variable (i.e., parameterized)
    composite functions.
    
    In particular, this addresses a shortcoming of the use of the magrittr `%>%`
    in functions. Instead of writing
    ```
    function(..., b = 2, n) {
      sample(...) %>% log(base = b) %>% rep(n)
    }
    ```
    which is inefficient because the function chain is created anew with each
    call, you can more directly [curry](https://en.wikipedia.org/wiki/Currying)
    it by writing
    ```
    posure(b = 2, n ~ {
      sample %>>>% log(base = b) %>>>% rep(n)
    })
    ```
    Not only is the `posure()` version more succinct, it is robuster and faster
    than the version with `%>%`, thanks to the non-standard mechanism of a
    closure that is “partially dynamically scoped.” (Whence the portmanteau
    “posure,” due to @henryaj; see the package documentation for details.)
    
  * `let()` enables you to create **contexts**: _composable_ local environments
    in which named expressions are _lazily_ resolved in a given order. Tidyverse
    quasiquotation of expressions is supported, allowing you to exercise
    fine-grained control over the evaluation of subexpressions.
    
  * As a companion to `let()`, `run()` evaluates an expression relative to a
    context. Unlike `base::with()`, `run()` supports quasiquotation and provides
    a means of overriding bindings in a given context.
  
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
