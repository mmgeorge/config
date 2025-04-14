; Function Definition (Outer)
; Captures the entire 'fn foo(...) { ... }' block
(function_item) @xfunction.outer

(
 (line_comment) @_start
 (function_item) @_end  
 (#make-range! "xfunction.outer" @_start @_end)
 )

(function_item body: (block) @xfunction.inner)

