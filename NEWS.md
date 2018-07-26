# gestalt

## 0.1.2.9000

  * When calling a composite function, the point (`.`) in an implicitly curried
    function may assume any name (#10). This is permitted so that the point may
    be called by its original name, which is handy when invoking a composite
    function with `do.call()`. For example:
    ```
    scramble <- sample(size = 5) %>>>% paste(collapse = "")
    do.call(scramble, list(x = letters))  # Argument need not be `. = letters`
    ```

## 0.1.2

  * In a composite function, default argument values following `...` are no
    longer absorbed by `...` (#6).
    
  * Improvements to the documentation throughout.

## 0.1.1

  * Initial release.
  
  * `fn()` is extracted from nofrills 0.3.0.
