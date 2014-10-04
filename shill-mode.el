(require 'generic-x) ;; we need this

(define-generic-mode
    'shill-mode                     ;; name of the mode to create
  '("#")                            ;; comments start with '#'
  '("if" "do" "for" "val" "var" "vals"
    "in" "provide" "require" "->"
    "init" "update"
    "+optional" "+rest" "fun"
    "else" "then")                  ;; some keywords
  '(("=" . 'font-lock-operator)
    (";" . 'font-lock-builtin))
  '("\\.cap$" "\\.amb$")            ;; files for which to activate this mode
  nil                               ;; other functions to call
  "A mode for shill files"          ;; doc string for this mode
  )

(provide 'shill-mode)
