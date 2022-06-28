# partially applied function shows function with arguments fixed

    Code
      partial(draw, letters, replace = !!replace)
    Output
      <Partially Applied Function>
      
      * FUNCTION:
      function(size, prob) ...
      
      Calling 'FUNCTION(...)' is equivalent to calling
      draw(x = ^letters, replace = ^TRUE, ...)
      
      The function 'draw()' has the form
      function(x, size = length(x), replace = FALSE, prob = NULL) ...
      
      Recover the called function with 'departial()'.

# composition of functions shows composite functions

    Code
      abs %>>>% inc:(!!partial(`+`, 1) %>>>% res:(log %>>>% agg:sum))
    Output
      <Function Composition>
      In order of application:
      
      [[1]]
        function (x)  .Primitive("abs")
      
      [["inc"]][[1]]
        <Partially Applied Function>
        * FUNCTION:
        function(e2) ...
        
        Calling 'FUNCTION(...)' is equivalent to calling
        (^1) + ...
        
        The function '+()' has the form
        function(e1, e2) ...
      
      [["inc"]][["res"]][[1]]
        function (x, base = exp(1))  .Primitive("log")
      
      [["inc"]][["res"]][["agg"]]
        function (..., na.rm = FALSE)  .Primitive("sum")
      
      Recover the list of functions with 'as.list()'.

# constant function prints according to its original class

    Code
      constant(partial(`+`, 1, 2))
    Output
      Constant Function:
      <Partially Applied Function>
      
      * FUNCTION:
      function() ...
      
      Calling 'FUNCTION(...)' is equivalent to calling
      (^1) + (^2)
      
      The function '+()' has the form
      function(e1, e2) ...
      
      Recover the called function with 'departial()'.

---

    Code
      constant(list %>>>% length)
    Output
      Constant Function:
      <Function Composition>
      In order of application:
      
      [[1]]
        function (...)  .Primitive("list")
      
      [[2]]
        function (x)  .Primitive("length")
      
      Recover the list of functions with 'as.list()'.

