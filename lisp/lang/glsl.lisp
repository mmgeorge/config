;;;; GLSL
(flycheck-define-checker glsl-lang-validator
  "A GLSL checker using glslangValidator.
   See URL https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"
  :command ("glslangValidator" source)
  :error-patterns
  ((error line-start "ERROR: " column ":" line ": " (message) line-end))
  :modes glsl-mode)

(add-to-list 'flycheck-checkers 'glsl-lang-validator)
