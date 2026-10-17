(expression) @comment
["if" "otherwise"] @keyword.conditional
["for" "each" "while"] @keyword.repeat
"return" @keyword.return
[(continue_statement) (break_statement)] @keyword.return
((reference) @variable
  (#match? @variable "[_.:]"))
(assignment_statement target: (expression (reference) @variable))
(expression . (reference) @variable .)
(argument_value . (reference) @variable .)
(for_statement binding: (identifier) @variable)
((expression (reference) @variable . (operator) @_operand_operator)
  (#any-of? @_operand_operator "=" "≠" "≤" "≥" "<" ">" "+" "-" "×" "÷" "*" "/" "%" "and" "or"))
((expression (operator) @_operand_operator . (reference) @variable)
  (#any-of? @_operand_operator "=" "≠" "≤" "≥" "<" ">" "+" "-" "×" "÷" "*" "/" "%" "and" "or"))
((identifier) @constant.builtin
  (#any-of? @constant.builtin "none" "true" "false"))
(invocation function: (reference) @function.call)
((invocation function: (reference) @type)
  (#match? @type "(^|.*[.:])[A-Z][A-Za-z0-9_]*$"))
(named_argument name: (identifier) @variable.member)
(number) @number
(string) @string
((operator) @operator
  (#not-any-of? @operator "and" "or" "not" "in"))
(if_statement condition: (expression (operator) @operator))
(while_statement condition: (expression (operator) @operator))
"←" @operator
"=" @operator
(punctuation) @punctuation.delimiter
["(" ")" "[" "]"] @punctuation.bracket
":" @punctuation.delimiter
(comment) @comment
(ERROR) @error
