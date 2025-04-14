; --- Keep existing function/method captures ---
; (function_declaration) @function.outer
; (function_declaration body: (statement_block) @function.inner)
;
; (method_definition) @function.outer
; (method_definition body: (statement_block) @function.inner)

; --- Add captures for Arrow Functions assigned to variables ---

; Capture 'const/let foo = () => {}' as outer function
; (lexical_declaration
;   (variable_declarator
;     value: (arrow_function))) @function.outer

; Capture the body of the arrow function as inner function
; This captures either the block body '{...}' or the expression body 'expr'
; (arrow_function body: (_) @function.inner)


; --- Optional: Capture 'const bar = function() {}' (Function Expressions) ---
; Case 1: Non-exported arrow function: 'const foo = () => {}'
; (lexical_declaration
  ; (variable_declarator
    ; value: (arrow_function))) @arrow.outer

; Case 2: Exported arrow function: 'export const foo = () => {}'
;        We capture the entire export statement.

;; top-level functions
(program 
   (export_statement
     declaration: (function_declaration
                    name: (identifier)
                    parameters: (formal_parameters)
                    body: (statement_block))) @xfunction.outer)

(program 
   (export_statement
     declaration: (function_declaration
                    name: (identifier)
                    parameters: (formal_parameters)
                    body: (statement_block))) @xfunction.outer)

(program 
  (function_declaration
       name: (identifier)
       parameters: (formal_parameters)
       body: (statement_block)) @xfunction.outer)



;; top-level functions, with comments
; (program 
;   (comment) @_start
;   (export_statement
;     declaration: (function_declaration
;                    name: (identifier)
;                    parameters: (formal_parameters)
;                    body: (statement_block))) @_end
;   (#make-range! "xfunction.outer" @_start @_end)
;   )
;
;
; (program 
;   (comment) @_start
;   (function_declaration
;     name: (identifier)
;     parameters: (formal_parameters)
;     body: (statement_block)) @_end
;   (#make-range! "xfunction.outer" @_start @_end))


;; top-level arrow functions
(program 
  (lexical_declaration
    (variable_declarator
      value: (arrow_function))) @xfunction.outer)


(program 
   (export_statement
     declaration: (lexical_declaration
                    (variable_declarator
                      value: (arrow_function)))) @xfunction.outer)


; top-level arrow functions, with comments
; (program 
;   (comment) @_start
;   (lexical_declaration
;     (variable_declarator
;       value: (arrow_function))) @xfunction.outer)
(program 
  (comment) @_start
  (lexical_declaration
    (variable_declarator
      value: (arrow_function)) @_end)
  (#make-range! "xfunction.outer" @_start @_end))

(program 
  (comment) @_start
  (export_statement
    declaration: (lexical_declaration
                   (variable_declarator
                     value: (arrow_function))) @_end)
  (#make-range! "xfunction.outer" @_start @_end))

; (program 
;   (lexical_declaration
;     (variable_declarator
;       value: (arrow_function) @xfunction.inner)))

; Inner capture for ANY arrow function's body (covers both cases above)
; This captures either the block body '{...}' or the expression body 'expr'
; (arrow_function body: (_) @function.inner)

; --- Other captures (parameters, etc. - copy from defaults if needed) ---
; (formal_parameters (required_parameter) @parameter.outer)
; (formal_parameters (optional_parameter) @parameter.outer)

; Add inner parameter captures if desired, e.g.:
; (required_parameter pattern: (_) @parameter.inner) ; Capture name/pattern
; (optional_parameter pattern: (_) @parameter.inner) ; Capture name/pattern

; Add class captures if needed
; (class_declaration) @class.outer
; (class_declaration body: (class_body) @class.inner)
