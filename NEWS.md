# gestalt

## 0.1.2.9000

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
