#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(provide parse-program current-source-name color-lexer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/cap lexer tokens ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-tokens value-tokens
  (NUMBER STRING BOOLEAN IDENTIFIER))

(define-empty-tokens
  empty-operator-tokens
  
  (;;; arithmetic operators   
   + - * /
     < >
     
     ;;; list/string concatination
     PP            ; ++
     ))


(define-tokens
  operator-tokens
  
  (;;; boolean operators
   AND   ; "and" or "//\" ∧ 
   OR    ; "or" or "\\/" ∨
   NOT   ; "not" or ¬ 
   
   ;;; arithmetic binary operators
   LESS-EQUAL    ; <= or ≤
   GREATER-EQUAL ; >= or ≥
   EQUAL         ; ==   
   ))


;; token definitions that group the privileges of 
;; files, directories, pipe-factories and pipe-ends
;; based on their structure
(define-tokens
  filesystem-privilege-tokens 
  (;;; filesystem common privileges without further behavioral refinements
   SIMPLE-FS-COMMON-PRIVILEGE
   ;;; filesystem common privileges with "if" behavioral refinements
   COMPLEX-FS-COMMON-IF-PRIVILEGE 
   ;;; file/directory common privileges without further behavioral refinements
   SIMPLE-FILE/DIR-COMMON-PRIVILEGE                   
   ;;; dir privileges with "only" behavioral refinements
   SIMPLE-DIR-PRIVILEGE                   
   ;;; dir privileges with "only" behavioral refinements
   COMPLEX-DIR-ONLY-PRIVILEGE
   ;;; dir privileges with "if" behavioral refinements
   COMPLEX-DIR-IF-PRIVILEGE
   ;;; dir privileges with "if" and "&" behavioral refinements
   COMPLEX-DIR-IF-&-PRIVILEGE
   ;;; dir privileges with "if" and "with" behavioral refinements
   COMPLEX-DIR-IF-WITH-PRIVILEGE
   ;;; pipe-factory privileges with "with" behavioral refinements
   COMPLEX-PIPE-FACTORY-WITH-PRIVILEGE))

;; token definitions that group the privileges of
;; sockets based on their structure
(define-tokens
  net-privilege-tokens
  (;;; family
   FAMILY
   ;;; permissions (this is the keyword for providing permissions)
   PERMISSION-ITEM
   ;;; permission
   PERMISSION))

(define-tokens 
  empty-punctuation-tokens 
  (;;; new line
   NEWLINE
   
   ;;; end of file
   EOF
   
   ;; definitions
   =      ;=
   
   ;; contracts
   &      ; &
   :      ; :
   
   ;; import modifier
   AS     ; as
   ))

(define-tokens 
  punctuation-tokens 
  
  (;;; general punctuation
   SEQ    ; ; 
   COMMA  ; ,
   
   ;;; general group markers
   OP CP  ; ( )
   OB CB  ; [ ]
   OC CC  ; { }
   
   ;;; script import/export
   REQ    ; require
   PROV   ; provide
   
   ;;; definitions
   VAL    ; val
   VALS   ; vals
   
   ;;; anonymous functions
   FUN    ; fun or λ 
   
   ;;; rest arguments
   DOTS   ; ... 
   
   ;;; contracts
   ARROW  ; -> or →
   DEP    ; <- or ← 
   FORALL ; forall or ∀
   OPT    ; +optional
   REST   ; +rest 
   WITH   ; with
   ONLY   ; only
   WHEN   ; when
   DOT    ; .
   DIR            ; dir
   FILE           ; file
   PIPE-END       ; pipe-end
   PIPE-FACTORY   ; pipe-factory
   SOCKET-FACTORY ; socket-factory
   
   ;;; if expressions   
   IF     ; if
   THEN   ; then
   ELSE   ; else
   
   ;;; for/while loops 
   WHILE  ; while
   FOR    ; for
   IN     ; in 
   DO     ; do   
   UPDATE ; update
   
   ;;; list abbreviations
   BAR    ; |
   
   ;;; for/while loops 
   INIT    ; init
   ASSIGN  ; :=
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/cap lexer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; useful abbreviations that help simplify the lexer definition
(define-lex-abbrevs 
  [greek (:or (char-range #\α #\ω) (char-range #\Α #\Β))]
  [roman (:or (char-range "a" "z") (char-range "A" "Z"))]
  [letter (:or roman greek)]
  [digit (:/ #\0 #\9)]
  [number (:or (:: (:* digit) "." (:+ digit)) (:+ digit))]
  [string #\"]
  [fun  (:or "fun" #\λ)]
  [identifier (:: letter 
                  (:* (:or letter digit #\_ #\? #\- #\+ #\/ #\* #\= #\> #\<)))]
  [arithmetic-op (:or "+" "-" "*" "/")]
  [less-equal (:or "=>" #\≤)]
  [greater-equal (:or "<=" #\≥)]
  [boolean (:or "true" "false")]
  [boolean-and (:or "and" "/\\" #\∧)]
  [boolean-or (:or "or" "\\/" #\∨)]
  [boolean-not (:or "not" #\¬)]
  [arrow-contract (:or "->" #\→)]
  [dependent-contract (:or "<-" #\←)]
  [forall-contract (:or "forall" #\∀)]
  [comment (:: "#" (complement (:: any-string #\newline any-string)) #\newline)])


;; useful abbreviations for privilege groupings
(define-lex-abbrevs
  
  ;;; abbreviations that group the privileges of 
  ;;; files, directories, pipe-factories and pipe-ends  
  ;;; based on their structure
  
  ;;;; file/directory/pipe-end common simple privileges
  ;;;; (for a directory these specify privileges for 
  ;;;;  files in the directory or its sub-directories)
  [simple-fs-common-privilege
   (:or "+stat" "+close")]
  
  ;;;; file/directory/pipe-end common "if" privileges
  ;;;; (for a directory these specify privileges for 
  ;;;;  files in the directory or its sub-directories)
  [complex-fs-common-if-privilege
   (:or "+read" "+write" "+append" "+path")]
  
  ;;;; file/directory simple privileges
  ;;;; (for a directory these specify privileges for 
  ;;;;  files in the directory or its sub-directories)
  [simple-file/dir-common-privilege
   (:or "+exec" 
        "+link" 
        "+unlink" 
        "+rename"
        "+chown"
        "+chtimes"
        "+chmod" 
        "+chflags" 
        "+read-extarrt"
        "+write-extarrt")]
  
  ;;;; directory simple privileges
  [simple-dir-privilege
   (:or "+chdir"
        "+chroot")]
  
  ;;;; directory complex "only" privileges
  [complex-dir-only-privilege
   "+contents"]
  
  ;;;; directory complex "if" privileges
  [complex-dir-if-privilege
   (:or "+rename-to"
        "+rename-from"
        "+add-link"
        "+unlink-dir"
        "+unlink-file"
        "+unlink-symlink")]
  
  ;;;; directory complex "if" and "&" privileges
  [complex-dir-if-&-privilege
   (:or "+add-symlink"
        "+read-symlink")]
  
  ;;;; directory complex "if" and "with" privileges
  [complex-dir-if-with-privilege
   (:or "+lookup"
        "+create-dir"
        "+create-file")]
  
  ;;;; pipe-factory complex "with" privileges
  [complex-pipe-factory-with-privilege
   (:or "+first"
        "+second")]
  
  ;;; socket privileges
  
  ;;;; socket families
  [family
   (:or "+transfer-family"
        "+address-family")]
  
  ;;; socket permissions
  [permission
   "+permissions"]
  [permission-item
   (:or  "+poll"
         "+bind"
         "+connect"
         "+listen"
         "+accept"
         "+send"
         "+recv")])

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; the shill/cap string lexer
(define get-string-token
  (lexer
   [(:~ #\" #\\) (cons (car (string->list lexeme))
                       (get-string-token input-port))]
   [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(:: #\\ #\n) (cons #\newline (get-string-token input-port))]
   [(:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))


;; the shill/cap lexer
(define expression-lexer
  (lexer-src-pos
   [(eof) 'EOF]
   [comment (return-without-pos (expression-lexer input-port))]
   [whitespace    ; this skips whitespace
    (return-without-pos (expression-lexer input-port))] 
   [#\newline 'NEWLINE]
   
   ;; base values 
   [number (token-NUMBER (string->number lexeme))]
   [string (token-STRING (list->string (get-string-token input-port)))]
   [boolean (token-BOOLEAN (make-boolean lexeme))]
   
   ;;; anonymous functions
   [fun (token-FUN (string->symbol lexeme))]
   
   ;;; rest arguments
   ["..." (token-DOTS (string->symbol lexeme)) ]
   
   ;;; operators
   [arithmetic-op (string->symbol lexeme)]
   [less-equal (token-LESS-EQUAL (string->symbol lexeme))]
   [greater-equal (token-GREATER-EQUAL (string->symbol lexeme))]
   ["==" 'EQUAL] 
   [boolean-and (token-AND (string->symbol lexeme))]
   [boolean-or (token-OR (string->symbol lexeme))]
   [boolean-not (token-NOT (string->symbol lexeme))]
   ["++" 'PP]
   
   ;;; general punctuation
   [";" (token-SEQ (string->symbol lexeme))]
   ["," (token-COMMA (string->symbol lexeme))]
   
   ;;; general group markers
   ["(" (token-OP (string->symbol lexeme))]
   [")" (token-CP (string->symbol lexeme))]
   ["{" (token-OC (string->symbol lexeme))]
   ["}" (token-CC (string->symbol lexeme))]
   ["[" (token-OB (string->symbol lexeme))]
   ["]" (token-CB (string->symbol lexeme))]
   
   ;;; script import/export
   ["provide" (token-PROV (string->symbol lexeme))]
   ["require" (token-REQ (string->symbol lexeme))]
   ["as" 'AS]
   
   
   ;; definitions
   ["val" (token-VAL (string->symbol lexeme))]
   ["vals" (token-VALS (string->symbol lexeme))]
   ["=" (string->symbol lexeme)]
   
   
   ;;; contracts
   [arrow-contract (token-ARROW (string->symbol lexeme))]
   [dependent-contract (token-DEP (string->symbol lexeme))]
   ["with" (token-WITH (string->symbol lexeme))]
   ["only" (token-ONLY (string->symbol lexeme))]
   ["when" (token-WHEN (string->symbol lexeme))]
   ["+rest" (token-REST (string->symbol lexeme))]
   ["+optional" (token-OPT (string->symbol lexeme))]
   [forall-contract (token-FORALL (string->symbol lexeme))]
   ["." (token-DOT (string->symbol lexeme))]
   [(:or "&" ":") (string->symbol lexeme)]
   ["dir/c" (token-DIR "dir/c")]
   ["file/c" (token-FILE "file/c")]
   ["pipe-end/c" (token-PIPE-END "pipe-end/c")]
   ["pipe-factory/c" (token-PIPE-FACTORY "pipe-factory/c")]
   ["socket-factory/c" (token-SOCKET-FACTORY "socket-factory/c")]
   
   ;;; list abbreviations
   ["|" (token-BAR (string->symbol lexeme))] 
   
   ;;; if expressions
   ["if" (token-IF (string->symbol lexeme))]
   ["then" (token-THEN (string->symbol lexeme))]
   ["else" (token-ELSE (string->symbol lexeme))]
   
   ;;; for/while loops
   ["while" (token-WHILE (string->symbol lexeme))]
   ["for" (token-FOR (string->symbol lexeme))]
   ["in" (token-IN (string->symbol lexeme))]
   ["do" (token-DO (string->symbol lexeme))]
   ["update" (token-UPDATE (string->symbol lexeme))]
   ["init" (token-INIT (string->symbol lexeme))]
   [":=" (token-ASSIGN (string->symbol lexeme))]
   
   ;;; privileges
   [simple-fs-common-privilege (token-SIMPLE-FS-COMMON-PRIVILEGE (string-drop-first lexeme))]
   [simple-file/dir-common-privilege (token-SIMPLE-FILE/DIR-COMMON-PRIVILEGE (string-drop-first lexeme))]
   [complex-fs-common-if-privilege (token-COMPLEX-FS-COMMON-IF-PRIVILEGE (string-drop-first lexeme))]
   [simple-dir-privilege (token-SIMPLE-DIR-PRIVILEGE (string-drop-first lexeme))]
   [complex-dir-only-privilege (token-COMPLEX-DIR-ONLY-PRIVILEGE (string-drop-first lexeme))]
   [complex-dir-if-privilege (token-COMPLEX-DIR-IF-PRIVILEGE (string-drop-first lexeme))]
   [complex-dir-if-&-privilege (token-COMPLEX-DIR-IF-&-PRIVILEGE (string-drop-first lexeme))]
   [complex-dir-if-with-privilege (token-COMPLEX-DIR-IF-WITH-PRIVILEGE (string-drop-first lexeme))]
   [complex-pipe-factory-with-privilege (token-COMPLEX-PIPE-FACTORY-WITH-PRIVILEGE (string-drop-first lexeme))]
   [family (token-FAMILY (string-drop-first lexeme))]
   [permission-item (token-PERMISSION-ITEM (string-drop-first lexeme))]
   [permission (token-PERMISSION (string-drop-first lexeme))] 
   
   ;;;; identifiers 
   [identifier (token-IDENTIFIER (string->symbol lexeme))]
   
   ;;;;;;;;;;;;;;;; 
   [(eof) 'EOF])) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/cap color lexer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (syn-val a b c d e)
  (values a 
          b
          c
          (position-offset d)
          (max
           (position-offset e)
           (+ (position-offset d) 1))))

(define color-lexer
  (lexer 
   [(eof)
    (syn-val lexeme 'eof #f start-pos end-pos)]
   [(:or #\tab #\space #\newline)
    (syn-val lexeme 'white-space #f start-pos end-pos)]
   [(:or arithmetic-op boolean-and boolean-or "++") 
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [boolean
    (syn-val lexeme 'constant #f start-pos end-pos)]
   [(:or "fun" "val" "vals" #\λ)
    (syn-val lexeme 'keyword #f start-pos end-pos)]
   [(:or "if" "then" "else")
    (syn-val lexeme 'keyword #f start-pos end-pos)]
   [(:or "while" "for" "in" "do" "update" "init")
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   ["|"
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [(:or arrow-contract
         dependent-contract
         forall-contract
         "dir/c"
         "file/c" 
         "pipe-end/c"
         "pipe-factory/c"
         "socket-factory/c")
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [(:or "+rest" "+optional")
    (syn-val lexeme 'keyword #f start-pos end-pos)]
   [(:or "with" "only" "as" "when" "." "&")
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [(:or "require" "provide")
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [(:or ";" "," "..." "=" ":" ":=")
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [(:or "(" )
    (syn-val lexeme 'parenthesis '|(| start-pos end-pos)]
   [(:or "[")
    (syn-val lexeme 'parenthesis '|[| start-pos end-pos)]
   [(:or  "{" )
    (syn-val lexeme 'parenthesis  '|{| start-pos end-pos)]
   [(:or  ")" "]" "}")
    (syn-val lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos)]
   [(:or simple-fs-common-privilege
         simple-file/dir-common-privilege
         complex-fs-common-if-privilege
         simple-dir-privilege
         complex-dir-only-privilege
         complex-dir-if-privilege
         complex-dir-if-&-privilege
         complex-dir-if-with-privilege
         complex-pipe-factory-with-privilege
         family
         permission
         permission-item)
    (syn-val lexeme 'hash-colon-keyword #f start-pos end-pos)]
   [string 
    (syn-val lexeme 'string #f start-pos end-pos)]
   [number
    (syn-val lexeme 'constant #f start-pos end-pos)]
   [identifier 
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [comment 
    (syn-val lexeme 'comment #f start-pos end-pos)]
   [any-char 
    (syn-val lexeme 'error #f start-pos end-pos)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/cap support for syntax arrows  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sym->original-syntax sym srcloc)
  (define p (open-input-string (symbol->string sym)))
  (port-count-lines! p)
  (match-define (list source-name line column position span) srcloc)
  (set-port-next-location! p line column position)
  (read-syntax source-name p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/cap parser  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (borg stx)
  (build stx #t))

(define-syntax (b stx)
  (build stx #f))

(define-for-syntax (build stx original?)
  (syntax-case stx ()
    [(_ o value start end)
     (with-syntax
         ([start-pos 
           (datum->syntax #'start
                          (string->symbol
                           (format "$~a-start-pos"
                                   (syntax->datum #'start))))]
          [end-pos  
           (datum->syntax #'end
                          (string->symbol
                           (format "$~a-end-pos"
                                   (syntax->datum #'end))))]
          [org original?])
       #`
       (datum->syntax
        o
        (if (and org (or (symbol? value) (identifier? value)))
            (sym->original-syntax 
             value
             (list (if (syntax? o) (syntax-source o) 'unknown)
                   (if (and o (syntax-line o))
                       (position-line start-pos)
                       #f)
                   (if (and o (syntax-column o))
                       (position-col start-pos)
                       #f)
                   (if (and o (syntax-position o))
                       (position-offset start-pos)
                       #f)
                   (- (position-offset end-pos)
                      (position-offset start-pos))))
            value)
        (list (if (syntax? o) (syntax-source o) 'unknown)
              (if (and o (syntax-line o))
                  (position-line start-pos)
                  #f)
              (if (and o (syntax-column o))
                  (position-col start-pos)
                  #f)
              (if (and o (syntax-position o))
                  (position-offset start-pos)
                  #f)
              (- (position-offset end-pos)
                 (position-offset start-pos)))
        o
        o))]))


(define (display-position-token pos)
  (display (position->list pos)))

(define (position->list pos)
  (list (position-offset pos)
        (position-line pos)
        (position-col pos)))

(define source-name #f)
(define orig-stx #f)
(define o #f)

(define (expression-parser src-name orig)
  (λ (gen)
    (set! source-name src-name)
    (set! orig-stx orig)
    (set! o orig)
    (the-parser gen)))


(define (present name val)
  (define (convert name)
    name)
  (or val (convert name))) 

(define parser-error
  (λ (token-ok? name val start end)
    (error-print-source-location #t)
    (raise-read-error
     (format "bad syntax `~a'" (present name val))
     source-name
     (position-line start)
     (position-col start)
     (position-offset start)
     (- (position-offset end) (position-offset end)))))


(define the-parser 
  (parser
   
   ;; debugging options
   ;;(debug "parse.debug")
   ;(yacc-output "cap/parse.yacc")
   (suppress)
   
   ;; source position option
   ;; causes the parser to expect a (make-position-token token)
   ;; instead of token (lexer-src-pos lexers produce this format)
   (src-pos)
   
   ;; tokens
   (tokens 
    value-tokens 
    empty-operator-tokens
    operator-tokens
    net-privilege-tokens 
    filesystem-privilege-tokens
    empty-punctuation-tokens
    punctuation-tokens)
   
   ;; parsing start and end states
   (start start)
   (end EOF)
   
   ;; error handling
   (error parser-error)
   
   ;; precedence 
   (precs 
    
    (left - +)
    (left * /)
    (left NOT AND OR)
    (left EQUAL LESS-EQUAL GREATER-EQUAL)
    (left PP))
   
   
   ;; shill/cap grammar
   (grammar
    (start [(program) (b o $1 1 1)]) 
    (program [() empty]
             [(final-pre-statement) `(,$1)]
             [(pre-statement program) `(,$1 ,@$2)])
    (pre-statement [(REQ import-specs SEQ) 
                    (b o `(,(b o `shill-require 1 1) ,@$2) 1 3)]
                   [(PROV export-specs SEQ) 
                    (b o `(,(b o `shill-provide 1 1) ,@$2) 1 3)]
                   [(statement) $1])
    (final-pre-statement [(REQ import-specs)
                          (b o `(,(b o `shill-require 1 1) ,@$2) 1 2)]
                         [(PROV export-specs)
                          (b o `(, (b o `shill-provide 1 1) ,@$2) 1 2)]
                         [(final-statement) $1])
    (import-specs [(import-spec) `(,$1)]
                  [(import-spec import-specs) `(,$1 ,@$2)])
    (export-specs [(export-spec) `(,$1)]
                  [(export-spec export-specs) `(,$1 ,@$2)])
    (import-spec [(STRING) (b o $1 1 1)]
                 [(STRING WITH OB renames CB) 
                  (b o `((b `rename-in 4 4) ,$1 ,@$4) 1 5)]
                 [(IDENTIFIER) 
                  (b o `(,(b o `lib 1 1),(borg o (sym->relstring $1) 1 1)) 1 1)]
                 [(IDENTIFIER WITH OB renames CB)
                  (b o `(,(b o `rename-in 1 1) (b o `(,(b o `lib 1 1) ,(borg o (sym->relstring $1) 1 1) ,@$4) 1 5)) 1 5)])
    (rename [(IDENTIFIER AS IDENTIFIER) 
             (b o `(,(borg o $1 1 1) ,(borg o $3 3 3)) 1 3)])
    (renames [(rename) `(,$1)]
             [(rename COMMA renames) `(,$1 ,@$3)])
    (export-spec [(OC IDENTIFIER : expr CC) 
                  (b o `(,(borg o $2 2 2) ,$4) 1 4)])
    (statements [(final-statement) `(,$1)] 
                [(statement) `(,$1)]
                [(statement statements)  `(,$1 ,@$2)])
    (final-expr [(expr) $1]
                [(expr SEQ) $1])
    (statements+final-expr [(final-expr) `(,$1)] 
                           [(statement statements+final-expr)  `(,$1 ,@$2)])
    (pre-ids   [(IDENTIFIER COMMA IDENTIFIER) 
                `(,(borg o $1 1 1) ,(borg o $3 1 3))]
               [(IDENTIFIER COMMA pre-ids) `(,(borg o $1 1 1) ,@$3)])
    (statement [(expr SEQ) $1]
               [(pre-ids = expr SEQ) 
                (b o `(,(b o `define-values 1 1) ,(b o $1 1 1) ,$3) 1 4)]
               [(IDENTIFIER = expr SEQ) 
                (b o `(,(b o `val 1 1) ,(borg o $1 1 1) ,$3) 1 4)]
               [(VALS pre-ids = expr SEQ)
                (b o `(,(b o `define-values 1 1) ,(b o $2 2 2) ,$4) 1 5)]
               [(VAL IDENTIFIER = expr SEQ) 
                (b o `(,(b o `val 1 1) ,(borg o $2 2 2) ,$4) 1 5)])
    (final-statement [(expr) $1]
                     [(pre-ids = expr)
                      (b o `(,(b o `define-values 1 1) ,(b o $1 1 1) ,$3) 1 3)]
                     [(IDENTIFIER = expr)
                      (b o `(,(b o `val 1 1) ,(borg o $1 1 1) ,$3) 1 3)]
                     [(VALS pre-ids = expr) 
                      (b o `(,(b o `define-values 1 1) ,(b o $2 2 2) ,$4) 1 4)]
                     [(VAL IDENTIFIER = expr)
                      (b o `(,(b o `val 1 1) ,(borg o $2 1 1) ,$4) 1 4)])
    (pre-list  [(expr) `(,$1)]
               [(expr COMMA pre-list) `(,$1 ,@$3)])
    (operation [(expr PP expr)  
                (b o `(,(b o `++ 2 2)  ,$1 ,$3) 1 3)]
               [(expr + expr)  
                (b o `(,(b o `+ 2 2)  ,$1 ,$3) 1 3)]
               [(expr - expr)  
                (b o `(,(b o `- 2 2) ,$1 ,$3) 1 3)]
               [(expr * expr)  
                (b o `(,(b o `* 2 2) ,$1 ,$3) 1 3)]
               [(expr / expr) 
                (b o `(,(b o `/ 2 2) ,$1 ,$3) 1 3)]
               [(expr LESS-EQUAL expr) 
                (b o `(,(b o `<= 2 2) ,$1 ,$3) 1 3)]
               [(expr GREATER-EQUAL expr) 
                (b o `(,(b o `=> 2 2) ,$1 ,$3) 1 3)]
               [(expr EQUAL expr) 
                (b o `(,(b o `equal? 2 2) ,$1 ,$3) 1 3)]
               [(expr AND expr)
                (b o `(,(b o `and 2 2) ,$1 ,$3) 1 3)]
               [(expr OR expr) 
                (b o `(,(b o `or 2 2) ,$1 ,$3) 1 2)]
               [(NOT expr) 
                (b o `(,(b o `not 2 2) ,$2) 1 2)])
    (expr [(IDENTIFIER) (borg o $1 1 1)]
          [(BOOLEAN) (b o $1 1 1)]
          [(NUMBER) (b o $1 1 1)]
          [(STRING) (b o $1 1 1)]
          [(operation) $1]
          [(OB CB) (b o `empty 1 2)]
          [(OB pre-list CB) 
           (b o `(,(b o `list 1 1) ,@$2) 1 3)]
          [(OB expr BAR expr CB) 
           (b o `(,(b o `cons 1 1) ,$2 ,$4) 1 5)]
          [(FOR for-clauses DO OC statements+final-expr CC) 
           (b o `(,(b o `shill-for 1 1) ,(b o $2 2 2) ,@$5) 1 6)] 
          [(FOR for-clauses INIT for-acc-clauses DO OC statements CC UPDATE for-acc-clauses)
           (b o `(,(b o `shill-for-acc 1 1) 
                  ,(b o $2 2 2) 
                  ,(b o $4 4 4) 
                  ,@$7 
                  ,(b o $10 10 10))
              1 
              10)]
          [(FOR for-clauses INIT for-acc-clauses UPDATE for-acc-clauses)
           (b o
              `(,(b o `shill-for 1 1)
                ,(b o $2 2 2) 
                ,(b o $4 4 4) 
                ,(b o `(,(b o `void 1 1)) 1 1) 
                ,(b o $6 6 6))
              1
              6)]
          [(IF expr THEN expr) 
           (b o `(,(b o `if  1 1) ,$2  ,$4 ,(b o `(void) 1 1)) 1 4)] 
          [(IF  expr THEN expr ELSE expr)
           (b o `(,(b o `if 1 1) ,$2 ,$4 ,$6) 1 6)]
          [(WHILE expr DO expr) 
           (b o `(,(b o `do 1 1) ,(b o `() 1 1) ,(b o `(,$2) 2 2) ,$4) 1 4)]
          [(FUN OP params CP expr) 
           (b o `(,(b o `lambda 1 1) ,(b o $3 3 3) ,$5) 1 5)]
          [(FUN rest-param expr) 
           (b o `(,(b o `lambda 1 1) ,$2 ,$3) 1 3)]
          [(op OP args CP) (b o `(,$1 ,@$3) 1 4)]
          [(OC statements CC) (b o `(let () ,@$2) 1 3)]
          [(top-contract) $1]
          [(OP expr CP) $2])
    (op [(IDENTIFIER) (borg o $1 1 1)]
        [(OP expr CP) $2])
    (contract [(IDENTIFIER : expr) 
               (b o `(,(borg o $1 1  1) ,$3) 1 3)]
              [(IDENTIFIER DEP ids  : expr) 
               (b o `(,(borg o $1 1 1) ,(b o $3 3 3) ,$5) 1 4)])
    (top-contract [(FORALL ids+privileges DOT top-contract) 
                   (b o `(,(b o `forall/c 1 1)  ,(b  o $2 2 2) ,$4) 1 4)]
                  [(contracts ARROW expr) 
                   (b o `(,(b o `->i 1 1) ,(b o $1 1 1) ,(b o (contract-range $3) 3 3)) 1 3)]
                  [(contracts ARROW OC contract CC)
                   (b o `(,(b o `->i 1 1) ,(b o $1 1 1) ,$4) 1 5)]
                  [(contracts OPT key-named-contracts ARROW expr) 
                   (b o `(,(b o `->i 1 1) 
                          ,(b o $1 1 1)
                          ,(b o $3 3 3)
                          ,(b o (contract-range $5) 5 5))
                      1 
                      5)]
                  [(contracts OPT key-named-contracts ARROW OC contract CC)
                   (b o `(,(b o `->i 1 1) ,(b o $1 1 1) ,(b o $3 3 3) ,$6) 1 7)]
                  [(contracts REST OC contract CC ARROW expr)
                   (b o `(,(b o `->i 1 1) ,(b o $1 1 1) #:rest ,$4 ,(b o (contract-range $7) 7 7)) 1 7)]
                  [(contracts REST OC contract CC ARROW OC contract CC)
                   (b o `(,(b o `->i 1 1) ,(b o $1 1 1) #:rest ,$4 ,$8) 1 8)]
                  [(contracts OPT key-named-contracts REST OC contract CC ARROW expr)
                   (b o `(,(b o `->i 1 1) ,(b o $1 1 1) ,(b o $3 3 3) #:rest ,$6 ,(b o (contract-range $9) 9 9)) 1 9)]
                  [(contracts OPT key-named-contracts REST OC contract CC ARROW OC contract CC)
                   (b o `(,(b o `->i 1 1) ,(b o $1 1 1) ,(b o $3 3 3) #:rest ,$6 ,$10) 1 11)]
                  [(contracts REST OC contract CC  OPT key-named-contracts ARROW expr)
                   (b o `(,(b o `->i 1 1) 
                          ,(b o $1 1 1)
                          ,(b o $7 7 7) 
                          #:rest ,$4 
                          ,(b o (contract-range $9) 9 9))
                      1 
                      9)]
                  [(contracts REST OC contract CC  OPT key-named-contracts ARROW OC contract CC) 
                   (b o `(,(b o `->i 1 1) ,(b o $1 1 1) ,(b o $7 7 7) #:rest ,$4 ,$10) 1 11)]
                  [(DIR OP top-dir-privileges CP)
                   (b o `(,(b o `dir/c 1 1) ,@$3) 1 4)]
                  [(FILE OP top-file-privileges CP)
                   (b o `(,(b o `file/c 1 1) ,@$3) 1 4)]
                  [(PIPE-END OP top-pipe-end-privileges CP)
                   (b o `(,(b o `pipe-end/c 1 1) ,@$3) 1 4)]
                  [(PIPE-FACTORY OP top-pipe-factory-privileges CP)
                   (b o `(,(b o `pipe-factory/c 1 1) ,@$3) 1 4)]
                  [(SOCKET-FACTORY OP top-socket-factory-privileges CP)
                   (b o `(,(b o `socket-factory/c 1 1) ,@$3) 1 4)])
    (contracts [() `()]
               [(OC pre-contracts CC) $2]) 
    (pre-contracts [(contract) `(,$1)]
                   [(contract COMMA pre-contracts) `(,$1 ,@$3)])
    (key-named-contract [(IDENTIFIER : expr) 
                         `(,(b o (id->keyword $1) 1 1) ,(b o `(,(borg o $1 1 1) ,$3) 1 3))]
                        [(IDENTIFIER DEP ids : expr)
                         `(,(b o (id->keyword $1) 1 1) ,(b o `(,(borg o $1 1 1) ,(b o $3 3 3) ,$5) 2 5))])
    (pre-key-named-contracts [(key-named-contract) $1]
                             [(key-named-contract COMMA pre-key-named-contracts)
                              `(,@$1 ,@$3)])
    (key-named-contracts [(OC pre-key-named-contracts CC) $2])
    (id+privilege [(OB IDENTIFIER : dir-privileges CB) 
                   (b o `(,(borg o $2 2 2) ,$4) 1 5)])
    (ids+privileges [(id+privilege) `(,$1)]
                    [(id+privilege ids+privileges) `(,@1 ,@$2)])
    (ids [(IDENTIFIER) `(,(borg o  $1 1 1))]
         [(IDENTIFIER COMMA ids) `(,(borg o  $1 1 1) ,@$3)])
    (param [(IDENTIFIER) `(,(borg o $1 1 1))]
           [(IDENTIFIER = expr) 
            `(,(b o (id->keyword $1) 1 1) ,(b o `(,(borg o $1 1 1) ,$3) 1 3))])
    (rest-param [(IDENTIFIER DOTS) (borg o $1  1 1)])
    (pre-params [(param) $1]
                [(param COMMA pre-params) `(,@$1 ,@$3)]
                [(param COMMA rest-param) `(,@$1 . ,$3)])
    (params [() empty]
            [(pre-params) $1])
    (for-clause [(IDENTIFIER IN expr) 
                 `(,(borg o $1 1 1) ,$3)])
    (for-clauses [(for-clause)  `(,$1)]
                 [(for-clause COMMA for-clauses) `(,@$1 ,@$3)])
    (for-acc-clause [(IDENTIFIER ASSIGN expr) 
                     `(,(borg o $1 1 1) ,$3)])
    (for-acc-clauses [(for-acc-clause) `(,$1)]
                     [(for-acc-clause COMMA for-acc-clauses) 
                      `(,$1 ,@$3)])
    (arg [(expr)  `(,$1)]
         [(IDENTIFIER = expr) 
          `(,(borg o (id->keyword $1) 1 1) ,$3)])
    (pre-args  [(arg) $1]
               [(arg COMMA pre-args) `(,@$1 ,@$3)])
    (args [() empty]
          [(pre-args) $1])  
    (dir-privilege-modifier [(OC dir-privileges CC) $2])
    (pipe-factory-privilege-modifier [(OC pipe-end-privileges CC) $2])
    (permission-list [(PERMISSION-ITEM) 
                      `(',(b o (string->symbol $1) 1 1))]
                     [(PERMISSION-ITEM COMMA permission-list)
                      `(',(b o (string->symbol $1) 1 1) ,@$3)]) 
    (top-fs-common-privilege [(SIMPLE-FS-COMMON-PRIVILEGE) 
                              `(,(b o  (key->keyword $1) 1 1) 
                                ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1))]
                             [(COMPLEX-FS-COMMON-IF-PRIVILEGE) 
                              `(,(b o  (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1))]
                             [(COMPLEX-FS-COMMON-IF-PRIVILEGE IF expr) 
                              `(,(b o  (key->keyword $1) 1 1) 
                                ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 1))])
    (top-file/dir-common-privilege [(SIMPLE-FILE/DIR-COMMON-PRIVILEGE) 
                                    `(,(b o  (key->keyword $1) 1 1) 
                                      ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1))])
    (top-dir-privilege [(SIMPLE-DIR-PRIVILEGE) 
                        `(,(b o  (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1))]
                       [(COMPLEX-DIR-ONLY-PRIVILEGE) 
                        `(,(b o  (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1))]
                       [(COMPLEX-DIR-ONLY-PRIVILEGE ONLY expr) 
                        `(,(b o  (key->keyword $1) 1 1)
                          ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3))]
                       [(COMPLEX-DIR-IF-PRIVILEGE) 
                        `(,(b o  (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1))]
                       [(COMPLEX-DIR-IF-PRIVILEGE IF expr) 
                        `(,(b o  (key->keyword $1) 1 1) 
                          ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3))]
                       [(COMPLEX-DIR-IF-&-PRIVILEGE)
                        `(,(b o  (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1))]
                       [(COMPLEX-DIR-IF-&-PRIVILEGE IF expr)
                        `(,(b o  (key->keyword $1) 1 1) 
                          ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3))]
                       [(COMPLEX-DIR-IF-&-PRIVILEGE IF expr & expr)
                        `(,(b o  (key->keyword $1) 1 1) 
                          ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3 ,$5) 1 5))]
                       [(COMPLEX-DIR-IF-WITH-PRIVILEGE) 
                        `(,(b o  (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1))]
                       [(COMPLEX-DIR-IF-WITH-PRIVILEGE WITH dir-privilege-modifier)
                        `(,(b o  (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3))]
                       [(COMPLEX-DIR-IF-WITH-PRIVILEGE IF expr) 
                        `(,(b o  (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3))]
                       [(COMPLEX-DIR-IF-WITH-PRIVILEGE IF expr WITH dir-privilege-modifier)
                        `(,(b o  (key->keyword $1) 1 1) 
                          ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3 ,$5) 1 5))]
                       [(COMPLEX-DIR-IF-WITH-PRIVILEGE WITH dir-privilege-modifier IF expr)
                        `(,(b o  (key->keyword $1) 1 1) 
                          ,(b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$5 ,$3) 1 5))])
    (top-pipe-factory-privilege [(COMPLEX-PIPE-FACTORY-WITH-PRIVILEGE) 
                                 `(,(b o  (key->keyword $1) 1 1) 
                                   ,(b o `(,(b o `list 1 1)
                                           ,(b o `(,(b o `list 1 1) ,(b o `"read" 1 1) ,(b o `#t 1 1)) 1 1)
                                           ,(b o `(,(b o `list 1 1) ,(b o `"write" 1 1) ,(b o `#t 1 1)) 1 1)
                                           ,(b o `(,(b o `list 1 1) ,(b o `"close" 1 1) ,(b o `#t 1 1)) 1 1)
                                           ,(b o `(,(b o `list 1 1) ,(b o `"append" 1 1) ,(b o `#t 1 1)) 1 1)
                                           ,(b o `(,(b o `list 1 1) ,(b o `"stat" 1 1) ,(b o `#t 1 1)) 1 1))
                                       1
                                       1))]
                                [(COMPLEX-PIPE-FACTORY-WITH-PRIVILEGE WITH pipe-factory-privilege-modifier)
                                 `(,(b o  (key->keyword $1) 1 1) ,$3)])
    (top-socket-factory-privilege [(FAMILY expr)
                                   `(,(b o (key->keyword $1) 1 1) ,$2)]
                                  [(PERMISSION OC permission-list CC) 
                                   `(,(b o (key->keyword $1) 1 1) ,(b o `(,(b o `list 1 1) ,@$3) 3 3))])
    (all-top-dir-privilege [(top-fs-common-privilege) $1]
                           [(top-file/dir-common-privilege) $1]
                           [(top-dir-privilege) $1])
    (top-dir-pre-privileges [(all-top-dir-privilege) $1]
                            [(all-top-dir-privilege COMMA top-dir-pre-privileges) 
                             `(,@$1 ,@$3)]) 
    (top-dir-privileges [() empty]
                        [(top-dir-pre-privileges)  $1])
    (all-top-file-privilege [(top-fs-common-privilege) $1]
                            [(top-file/dir-common-privilege) $1])
    (top-file-pre-privileges [(all-top-file-privilege) $1]
                             [(all-top-file-privilege COMMA top-file-pre-privileges) 
                              `(,@$1 ,@$3)]) 
    (top-file-privileges [() empty]
                         [(top-file-pre-privileges)  $1])
    (all-top-pipe-end-privilege [(top-fs-common-privilege) $1])
    (top-pipe-end-pre-privileges [(all-top-pipe-end-privilege) $1]
                                 [(all-top-pipe-end-privilege COMMA top-pipe-end-pre-privileges) 
                                  `(,@$1 ,@$3)]) 
    (top-pipe-end-privileges [() empty]
                             [(top-pipe-end-pre-privileges)  $1])
    (all-top-pipe-factory-privilege [(top-pipe-factory-privilege) $1])
    (top-pipe-factory-pre-privileges [(all-top-pipe-factory-privilege) $1]
                                     [(all-top-pipe-factory-privilege COMMA top-pipe-factory-pre-privileges) 
                                      `(,@$1 ,@$3)]) 
    (top-pipe-factory-privileges [() empty]
                                 [(top-pipe-factory-pre-privileges)  $1])
    (all-top-socket-factory-privilege [(top-socket-factory-privilege) $1])
    (top-socket-factory-pre-privileges [(all-top-socket-factory-privilege) $1]
                                       [(all-top-socket-factory-privilege COMMA top-socket-factory-pre-privileges) 
                                        `(,@$1 ,@$3)]) 
    (top-socket-factory-privileges [() empty]
                                   [(top-socket-factory-pre-privileges)  $1])
    (fs-common-privilege [(SIMPLE-FS-COMMON-PRIVILEGE) 
                          (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1)]
                         [(COMPLEX-FS-COMMON-IF-PRIVILEGE)
                          (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1)]
                         [(COMPLEX-FS-COMMON-IF-PRIVILEGE IF expr) 
                          (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3)])
    (file/dir-common-privilege [(SIMPLE-FILE/DIR-COMMON-PRIVILEGE) 
                                (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1)])
    (dir-privilege [(SIMPLE-DIR-PRIVILEGE) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1)]
                   [(COMPLEX-DIR-ONLY-PRIVILEGE) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1)]
                   [(COMPLEX-DIR-ONLY-PRIVILEGE ONLY expr) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3)]
                   [(COMPLEX-DIR-IF-PRIVILEGE) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1)]
                   [(COMPLEX-DIR-IF-PRIVILEGE IF expr)
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3)]
                   [(COMPLEX-DIR-IF-&-PRIVILEGE) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1)]
                   [(COMPLEX-DIR-IF-&-PRIVILEGE IF expr) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3)]
                   [(COMPLEX-DIR-IF-&-PRIVILEGE IF expr & expr) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3 ,$5) 1 5)]
                   [(COMPLEX-DIR-IF-WITH-PRIVILEGE) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1)) 1 1)]
                   [(COMPLEX-DIR-IF-WITH-PRIVILEGE WITH dir-privilege-modifier)
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3)]
                   [(COMPLEX-DIR-IF-WITH-PRIVILEGE IF expr) 
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3) 1 3)]
                   [(COMPLEX-DIR-IF-WITH-PRIVILEGE IF expr WITH dir-privilege-modifier)
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$3 ,$5) 1 5)]
                   [(COMPLEX-DIR-IF-WITH-PRIVILEGE WITH dir-privilege-modifier IF expr)
                    (b o `(,(b o `list 1 1) ,$1 ,(b o '#t 1 1) ,$5 ,$3) 1 5)])
    (all-dir-privilege [(fs-common-privilege) $1]
                       [(file/dir-common-privilege) $1]
                       [(dir-privilege) $1])
    (dir-pre-privileges [(all-dir-privilege)
                         (b o `(,(b o `list 1 1) ,$1) 1 1)]
                        [(all-dir-privilege COMMA dir-pre-privileges) 
                         (b o `(,(b o `cons 1 1) ,$1 ,$3) 1 3)]) 
    (dir-privileges [() empty]
                    [(dir-pre-privileges)  $1])
    (all-pipe-end-privilege [(fs-common-privilege) $1])
    (pipe-end-pre-privileges [(all-pipe-end-privilege)
                              (b o `(,(b o `list 1 1) ,$1) 1 1)]
                             [(all-pipe-end-privilege COMMA pipe-end-pre-privileges) 
                              (b o `(,(b o `cons 1 1) ,$1 ,$3) 1 3)]) 
    (pipe-end-privileges [() empty]
                         [(pipe-end-pre-privileges)  $1]))))



(define (parse-program src ip)
  (port-count-lines! ip)
  (define out
    ((expression-parser
      (or src (object-name ip))
      (datum->syntax #f
                     'here
                     (list (or src (object-name ip))
                           1
                           0
                           1
                           #f)))
     (λ () (expression-lexer ip))))
  (or out eof))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc. auxiliary definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-source-name (make-parameter #f))

(define (make-boolean lexeme) (string=? lexeme "true"))

(define (key->keyword s) (string->keyword s))
(define (id->keyword id) (string->keyword (symbol->string id)))

(define (string-drop-right n s)
  (substring s 0 (- (string-length s) n)))

(define (string-drop-first s)
  (substring s 1 (string-length s)))

(define (contract-range expr)
  (cond
    [(eq? 'any (syntax-e expr)) 'any]
    [(and (list? expr) (eq? 'values (first expr)))
     `(values ,@(map contract-range (cdr expr)))]
    [else `(,(gensym 'cvar) ,expr)]))

(define (sym->relstring s)
  (string-append (symbol->string s) ".cap"))


