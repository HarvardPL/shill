#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(provide parse-program current-source-name color-lexer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/ambient lexer tokens ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define-tokens 
  empty-punctuation-tokens 
  (;;; new line
   NEWLINE
   
   ;;; end of file
   EOF
   
   ;; definitions
   =      ; =
   
   ;; script import modifier
   WITH   ; with
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
   
   ;;; script import
   REQ    ; require
  
   ;;; definitions
   VAL    ; val
   VALS   ; vals
   
   ;;; list abbreviations
   BAR    ; |
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/ambient lexer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; useful abbreviations that help simplify the lexer definition
(define-lex-abbrevs 
  [greek (:or (char-range #\α #\ω) (char-range #\Α #\Β))]
  [roman (:or (char-range "a" "z") (char-range "A" "Z"))]
  [letter (:or roman greek)]
  [digit (:/ #\0 #\9)]
  [number (:or (:: (:* digit) "." (:+ digit)) (:+ digit))]
  [string #\"]
  [identifier (:: letter 
                  (:* (:or letter digit #\_ #\? #\- #\+ #\/ #\* #\= #\> #\<)))]
  [arithmetic-op (:or "+" "-" "*" "/")]
  [less-equal (:or "=>" #\≤)]
  [greater-equal (:or "<=" #\≥)]
  [boolean (:or "true" "false")]
  [boolean-and (:or "and" "/\\" #\∧)]
  [boolean-or (:or "or" "\\/" #\∨)]
  [boolean-not (:or "not" #\¬)]
  [comment (:: "#" (complement (:: any-string #\newline any-string)) #\newline)])


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; the shill/ambient string lexer
(define get-string-token
  (lexer
   [(:~ #\" #\\) (cons (car (string->list lexeme))
                       (get-string-token input-port))]
   [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(:: #\\ #\n) (cons #\newline (get-string-token input-port))]
   [(:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))

;; the shill/ambient lexer
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
   
   ;;; script import
   ["require" (token-REQ (string->symbol lexeme))]
   ["with"    'WITH]
   ["as"      'AS]
   
   ;; definitions
   ["val" (token-VAL (string->symbol lexeme))]
   ["vals" (token-VALS (string->symbol lexeme))]
   ["=" (string->symbol lexeme)]
   
   ;;; list abbreviations
   ["|" (token-BAR (string->symbol lexeme))] 
   
   ;;;; identifiers 
   [identifier (token-IDENTIFIER (string->symbol lexeme))]
   
   ;;;;;;;;;;;;;;;; 
   [(eof) 'EOF])) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/ambient color lexer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
   [(:or "val" "vals")
    (syn-val lexeme 'keyword #f start-pos end-pos)]
   ["|"
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [(:or "+rest" "+optional")
    (syn-val lexeme 'keyword #f start-pos end-pos)]
   [(:or "require" "provide" "as" "with")
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [(:or ";" "," "=" )
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [(:or "(" )
    (syn-val lexeme 'parenthesis '|(| start-pos end-pos)]
   [(:or "[")
    (syn-val lexeme 'parenthesis '|[| start-pos end-pos)]
   [(:or  "{" )
    (syn-val lexeme 'parenthesis  '|{| start-pos end-pos)]
   [(:or  ")" "]" "}")
    (syn-val lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos)]
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
;; shill/ambient support for syntax arrows  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sym->original-syntax sym srcloc)
  (define p (open-input-string (symbol->string sym)))
  (port-count-lines! p)
  (match-define (list source-name line column position span) srcloc)
  (set-port-next-location! p line column position)
  (read-syntax source-name p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shill/ambient parser  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
   
   
   ;; shill/ambient grammar
   (grammar
    (start [(program) (b o $1 1 1)]) 
    (program [() empty]
             [(final-pre-statement) `(,$1)]
             [(pre-statement program) `(,$1 ,@$2)])
    (pre-statement [(REQ import-specs SEQ) 
                    (b o `(,(b o `shill-require 1 1) ,@$2) 1 3)]
                   [(statement) $1])
    (final-pre-statement [(REQ import-specs)
                          (b o `(,(b o `shill-require 1 1) ,@$2) 1 2)]
                         [(final-statement) $1])
    (import-specs [(import-spec) `(,$1)]
                  [(import-spec import-specs) `(,$1 ,@$2)])
    (import-spec [(STRING) (borg o $1 1 1)]
                 [(STRING WITH OB renames CB) 
                  (b o `((b `rename-in 4 4) ,$1 ,@$4) 1 5)]
                 [(IDENTIFIER) 
                  (b o `(,(b o `lib 1 1) ,(borg o (sym->relstring $1) 1 1)) 1 1)]
                 [(IDENTIFIER WITH OB renames CB)
                  (b o `(,(b o `rename-in 1 1) ,(b o `(,(b o `lib 1 1) ,(borg o (sym->relstring $1) 1 1) ,@$4) 1 5)) 1 5)])
    (rename [(IDENTIFIER AS IDENTIFIER) 
             (b o `(,(borg o $1 1 1) ,(borg o $3 3 3)) 1 3)])
    (renames [(rename) `(,$1)]
             [(rename COMMA renames) `(,$1 ,@$3)])
    (statements [(final-statement) `(,$1)] 
                [(statement) `(,$1)]
                [(statement statements)  `(,$1 ,@$2)])
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
          [(op OP args CP) (b o `(,$1 ,@$3) 1 4)]
          [(OC statements CC) (b o `(let () ,@$2) 1 3)]
          [(OP expr CP) $2])
    (op [(IDENTIFIER) (borg o $1 1 1)]
        [(OP expr CP) $2])
    (arg [(expr)  `(,$1)]
         [(IDENTIFIER = expr) 
          `(,(borg o (id->keyword $1) 1 1) ,$3)])
    (pre-args  [(arg) $1]
               [(arg COMMA pre-args) `(,@$1 ,@$3)])
    (args [() empty]
          [(pre-args) $1]))))




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

(define (key->keyword s) (string->keyword (string-drop-first s)))
(define (id->keyword id) (string->keyword (symbol->string id)))

(define (string-drop-right n s)
  (substring s 0 (- (string-length s) n)))

(define (string-drop-first s)
  (substring s 1 (string-length s)))


(define (sym->relstring s)
  (string-append (symbol->string s) ".cap"))


