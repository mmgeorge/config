["if" "otherwise"] @keyword.conditional
["for" "each" "while"] @keyword.repeat
"return" @keyword.return
[(continue_statement) (break_statement)] @keyword.return
(reference) @variable
(for_statement binding: (identifier) @variable)
(binding_group (identifier) @variable)
(builtin) @constant.builtin
(invocation function: (reference) @function.call)
((invocation function: (reference) @type)
  (#match? @type "(^|.*[.:])[A-Z][A-Za-z0-9_]*$"))
((reference . (identifier) @type (identifier))
  (#match? @type "^[A-Z]"))
(named_argument name: (identifier) @variable.member)
(number) @number
(string) @string
(operator) @operator
"←" @operator
"=" @operator
["." "," "::"] @punctuation.delimiter
["(" ")" "[" "]"] @punctuation.bracket
":" @punctuation.delimiter
(comment) @comment
(ERROR) @error
