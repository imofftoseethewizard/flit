#lang br/quicklang

(require racket/pretty)
(require racket/provide-syntax)

(define-macro (flit-module-begin PARSE-TREE)
  #'(#%module-begin (flit-program 'PARSE-TREE)))

(provide (rename-out [flit-module-begin #%module-begin]))

(struct runtime (memory dictionary stack return-stack))

(define (make-runtime memory-size stack-size return-stack-size)
  (runtime
   (make-vector memory-size 0)
   (make-hash)
   (make-vector stack-size 0)
   (make-vector return-stack-size 0)))

(define (flit-program program)
  (flit-evaluate (make-runtime 30000 100 100) (expand-phrases (cdr program))))

(provide flit-program)

(define (flit-evaluate rt exp)
  (pretty-print exp))

(define (expand-phrases phrases)
  (for/list ([phrase phrases])
    (expand-phrase phrase)))

(define (expand-phrase phrase)
  (case (car phrase)
    [(compiled-phrase)
     (expand-compiled-phrase phrase)]

    [(deferred-phrase)
     (expand-deferred-phrase phrase)]

    [(simple-executed-phrase executed-phrase-compiled-as-word executed-phrase-compiled-as-double-word)
     (expand-executed-phrase phrase)]))

(define (expand-compiled-phrase phrase)
  `(compiled-phrase
    ,(for/list ([atom (cdr phrase)])
       (compiled-atom atom))))

(define (expand-deferred-phrase phrase)
  `(deferred-phrase
     ,(for/list ([atom-or-phrase (cdr phrase)])
        (case (car atom-or-phrase)
          [(simple-executed-phrase executed-phrase-compiled-as-word executed-phrase-compiled-as-double-word)
           (expand-executed-phrase atom-or-phrase)]

          [(character name number string symbol)
           (deferred-atom atom-or-phrase)]))))

(define (expand-executed-phrase phrase)
  `(executed-phrase
    ,(case (car phrase)
       [(simple-executed-phrase) (executed-atoms (cdr phrase))]
       [(executed-phrase-compiled-as-word) `(,@(executed-atoms (cdr phrase)) (compile-word-literal))]
       [(executed-phrase-compiled-as-double-word) `(,@(executed-atoms (cdr phrase)) (compile-double-word-literal))])))

(define (executed-atoms atoms)
  (for/list ([atom atoms])
    (executed-atom atom)))

(define (compiled-atom atom)
  (case (car atom)
    [(character) `(compile-character ,@(cdr atom))]
    [(name) `(compile-named-procedure ,@(cdr atom))]
    [(number) `(compile-number ,@(cdr atom))]
    [(string) `(compile-string ,@(cdr atom))]
    [(symbol) `(compile-symbol ,@(cdr atom))]
    [else `(syntax-error "unknown-atom-type" ,atom)]))

(define (deferred-atom atom)
  (case (car atom)
    [(character) `(defer-character ,@(cdr atom))]
    [(name) `(defer-named-procedure ,@(cdr atom))]
    [(number) `(defer-number ,@(cdr atom))]
    [(string) `(defer-string ,@(cdr atom))]
    [(symbol) `(defer-symbol ,@(cdr atom))]
    [else `(syntax-error "unknown-atom-type" ,atom)]))

(define (executed-atom atom)
  (case (car atom)
    [(character) `(push-character ,@(cdr atom))]
    [(name) `(execute-named-procedure ,@(cdr atom))]
    [(number) `(push-number ,@(cdr atom))]
    [(string) `(push-string ,@(cdr atom))]
    [(symbol) `(push-symbol ,@(cdr atom))]
    [else `(syntax-error "unknown-atom-type" ,atom)]))
