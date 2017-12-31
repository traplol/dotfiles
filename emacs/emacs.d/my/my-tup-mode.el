(require 'generic-x)

(define-generic-mode 'tup-mode
  ()
  '("foreach" "ifeq" "ifneq" "ifdef" "ifndef" "else" "endif" "error"
    "include" "include_rules" "run" "preload" "export")
  '(("\\(#.*\\)" 1 'font-lock-comment-face)
    ("\\.gitignore" . 'font-lock-keyword-face)
    ("[$@](.*?)" . 'font-lock-variable-name-face)
    ("%[fbBeo0dg]" . 'font-lock-constant-face)
    (":\\||>" . 'font-lock-type-face)
    )
  '("\\Tupfile$" "\\.tup$")
  ()
  "A mode for Tupfiles"
  )

(provide 'tup-mode)
