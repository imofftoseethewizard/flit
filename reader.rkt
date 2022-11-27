#lang br/quicklang

(require brag/support flit/parser)

(define (read-syntax path port)
  (port-count-lines! port)
  (lexer-file-path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (strip-bindings
   (with-syntax ([PT parse-tree])
     #'(module flit-mod flit/expander
         PT))))

(provide read-syntax)

(define-lex-abbrev name (:+ (:~ whitespace)))
(define-lex-abbrev rest-of-line (complement (:seq (:* any-char) "\n" (:* any-char))))

(define (make-tokenizer port)
  (define (next-token)
    (define flit-lexer
      (lexer-srcloc
       ;; literate comment -- any line that doesn't start with 4 spaces
       [(:seq (:+ "\n") (:** 0 3 " ") (:~ whitespace) rest-of-line) (next-token)]

       ;; line comment
       [(:seq "\\ " rest-of-line) (next-token)]

       [(:+ whitespace) (next-token)]

       [(:or "[" "]" "{" "}" "(" ")" "((" "))") lexeme]

       [(:seq "'"
              (:or
               (complement (char-set "'\\"))
               (:seq "\\" any-char))
              "'")
        (token 'CHARACTER (string-trim lexeme "'")
               #:position (pos lexeme-start)
               #:line (line lexeme-start)
               #:column (col lexeme-start)
               #:span (- (pos lexeme-end)
                         (pos lexeme-start)))]

       [(:seq (:? "-") (:+ (char-set "0123456789")))
        (token 'NUMBER (string->number lexeme)
               #:position (pos lexeme-start)
               #:line (line lexeme-start)
               #:column (col lexeme-start)
               #:span (- (pos lexeme-end)
                         (pos lexeme-start)))]

       [(:seq "\""
              (:*
               (:or
                (:* (:~ "\"" "\\"))
                (:seq "\\" any-char)))
              "\"")
        (token 'STRING (string-trim lexeme "\"")
               #:position (pos lexeme-start)
               #:line (line lexeme-start)
               #:column (col lexeme-start)
               #:span (- (pos lexeme-end)
                         (pos lexeme-start)))]

       [(:seq "#" name)
        (token 'SYMBOL (string-trim lexeme "#" #:right? #f)
               #:position (pos lexeme-start)
               #:line (line lexeme-start)
               #:column (col lexeme-start)
               #:span (- (pos lexeme-end)
                         (pos lexeme-start)))]

       [name
        (token 'NAME lexeme
               #:position (pos lexeme-start)
               #:line (line lexeme-start)
               #:column (col lexeme-start)
               #:span (- (pos lexeme-end)
                         (pos lexeme-start)))]
       ))

    (flit-lexer port))

  next-token)
