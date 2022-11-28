#lang br/quicklang

(require racket/pretty)
(require racket/provide-syntax)

(define-macro (flit-module-begin PARSE-TREE)
  #'(#%module-begin (flit-program 'PARSE-TREE)))

(provide (rename-out [flit-module-begin #%module-begin]))

(struct stringset (index (storage #:mutable)))

(define (make-stringset)
  (stringset (make-hash) (vector #f)))

(define (stringset-find ss v)
  (hash-ref (stringset-index ss) v))

(define (stringset-ref ss v)
  (vector-ref (stringset-storage ss) v))

(define (stringset-inter! ss v)
  (hash-ref (stringset-index ss) v (lambda () (stringset-add! ss v))))

(define (stringset-add! ss v)
  (let ([string-idx (hash-count (stringset-index ss))]
        [storage (stringset-storage ss)])
    (when (stringset-full? ss)
      (stringset-expand! ss))
    (vector-set! (stringset-storage ss) string-idx v)
    (hash-set! (stringset-index ss) v string-idx)
    string-idx))

(define (stringset-full? ss)
  (= (hash-count (stringset-index ss)) (vector-length (stringset-storage ss))))

(define (stringset-expand! ss)
  (let* ([storage (stringset-storage ss)]
         [extension (make-vector (vector-length storage) #f)])
    (set-stringset-storage! ss (vector-append storage extension))))

(struct runtime
  ((state #:mutable)
   word-size
   (memory #:mutable)
   (memory-pointer #:mutable)
   dictionary
   symbols
   strings
   (stack #:mutable)
   (return-stack #:mutable)
   (control-stack #:mutable)
   (instruction-pointer #:mutable)))

(define (make-runtime)
  (let ([rt (runtime
             'ready
             4
             (vector #f)
             0
             (make-hash)
             (make-stringset)
             (make-stringset)
             '()
             '()
             '()
             #f)])
    (runtime-initialize-builtins! rt)
    rt))

(define (runtime-initialize-builtins! rt)
  (let ([d (runtime-dictionary rt)])
    (for ([def builtins])
      (hash-set! d (runtime-inter-symbol! rt (car def)) (cdr def)))))

(define (runtime-define! rt)
  (let ([symbol-ref (runtime-pop! rt)])
    (hash-set! (runtime-dictionary rt) (cadr symbol-ref) (runtime-memory-pointer rt))))

(define (runtime-exit! rt)
  (set-runtime-instruction-pointer! rt (runtime-return-pop! rt)))

(define (runtime-compile-if! rt)
  (runtime-control-push! rt (runtime-memory-pointer rt))
  (runtime-compile-atom! rt '(branch-if-0 #f)))

(define (runtime-compile-else! rt)
  (let* ([mp (runtime-memory-pointer rt)]
         [origin-mp (runtime-control-pop! rt)]
         [origin (runtime-ref-memory rt origin-mp)])
    (runtime-compile-atom! rt '(jump #f))
    (runtime-control-push! rt mp)
    (let ([offset (- (runtime-memory-pointer rt) origin-mp 1)])
      (runtime-set-memory! rt origin-mp `(,(car origin) ,offset)))))

(define (runtime-compile-then! rt)
  (let* ([origin-mp (runtime-control-pop! rt)]
         [origin (runtime-ref-memory rt origin-mp)]
         [offset (- (runtime-memory-pointer rt) origin-mp 1)])
    (runtime-set-memory! rt origin-mp `(,(car origin) ,offset))))

(define (runtime-branch-if-0! rt offset)
  (let ([v (runtime-pop! rt)])
    (when (and (eq? 'number (car v))
               (zero? (cadr v)))
      (runtime-jump! rt offset))))

(define (runtime-jump! rt offset)
  (set-runtime-instruction-pointer! rt (+ offset (runtime-instruction-pointer rt))))

(define (runtime-print! rt)
  (let ([atom (runtime-pop! rt)])
    (displayln (case (car atom)
               [(character number)
                (cadr atom)]

               [(string-ref)
                (stringset-ref (runtime-strings rt) (cadr atom))]

               [(symbol-ref)
                (stringset-ref (runtime-symbols rt) (cadr atom))]))))

(define (runtime-compile-atom! rt atom)
  (when (runtime-memory-full? rt)
    (runtime-expand-memory! rt))
  (let ([memory-pointer (runtime-memory-pointer rt)])
    (vector-set! (runtime-memory rt) memory-pointer atom)
    (set-runtime-memory-pointer! rt (+ 1 memory-pointer))))

(define builtins
  `(("define" . ,runtime-define!)
    ("else"   . ,runtime-compile-else!)
    ("exit"   . ,runtime-exit!)
    ("if"     . ,runtime-compile-if!)
    ("print"  . ,runtime-print!)
    ("then"   . ,runtime-compile-then!)))

(define (runtime-memory-full? rt)
  (= (runtime-memory-pointer rt) (vector-length (runtime-memory rt))))

(define (runtime-expand-memory! rt)
  (set-runtime-memory! rt (vector-append (runtime-memory rt)
                                         (make-vector (runtime-memory-pointer rt) #f))))

(define (runtime-ref-memory rt mp)
  (vector-ref (runtime-memory rt) mp))

(define (runtime-set-memory! rt mp atom)
  (vector-set! (runtime-memory rt) mp atom))

(define (runtime-inter-string! rt v)
  (stringset-inter! (runtime-strings rt) v))

(define (runtime-inter-symbol! rt v)
  (stringset-inter! (runtime-symbols rt) v))

(define (runtime-find-definition rt v)
  (hash-ref (runtime-dictionary rt) (stringset-find (runtime-symbols rt) v)))

(define (runtime-push! rt v)
  (set-runtime-stack! rt (cons v (runtime-stack rt))))

(define (runtime-pop! rt)
  (let ([stack (runtime-stack rt)])
    (when (empty? stack)
      (error "stack-underflow"))
    (let ([atom (car stack)])
      (set-runtime-stack! rt (cdr stack))
      atom)))

(define (runtime-control-push! rt v)
  (set-runtime-control-stack! rt (cons v (runtime-control-stack rt))))

(define (runtime-control-pop! rt)
  (let* ([stack (runtime-control-stack rt)]
         [atom (car stack)])
    (set-runtime-control-stack! rt (cdr stack))
    atom))

(define (runtime-return-push! rt v)
  (set-runtime-return-stack! rt (cons v (runtime-return-stack rt))))

(define (runtime-return-pop! rt)
  (let* ([stack (runtime-return-stack rt)]
         [atom (car stack)])
    (set-runtime-return-stack! rt (cdr stack))
    atom))

(define (runtime-execute! rt v)
  (if (procedure? v)
      (v rt)
      (begin
        (set-runtime-return-stack! rt '(#f))
        (runtime-interpret! rt v))))

(define (runtime-interpret! rt v)
  (set-runtime-instruction-pointer! rt v)
  (let loop ()
    (let* ([ip (runtime-instruction-pointer rt)]
           [atom (vector-ref (runtime-memory rt) ip)])
      (runtime-trace rt)
      (set-runtime-instruction-pointer! rt (+ 1 ip))
      (interpret-atom rt atom)
      (unless (or (empty? (runtime-return-stack rt))
                  (not (eq? 'ready (runtime-state rt))))
        (loop)))))

(define (runtime-trace rt)
  (pretty-print
   `(,(runtime-instruction-pointer rt)
     ,(vector-ref (runtime-memory rt) (runtime-instruction-pointer rt))
     ,(runtime-stack rt))))

(define (runtime-call! rt v)
  (if (procedure? v)
      (v rt)
      (begin
        (runtime-return-push! rt (runtime-instruction-pointer rt))
        (set-runtime-instruction-pointer! rt v))))

(define (flit-program program)
  (let ([rt (make-runtime)])
    (flit-evaluate rt (expand-phrases (cdr program)))
    (pretty-print (vector-take (runtime-memory rt) (runtime-memory-pointer rt)))
    (pretty-print (stringset-index (runtime-strings rt)))
    (pretty-print (stringset-index (runtime-symbols rt)))
    (pretty-print (runtime-dictionary rt))
    (pretty-print (runtime-stack rt))))

(provide flit-program)

(define (flit-evaluate rt phrases)
  (set-runtime-state! rt 'ready)
  (for ([phrase phrases])
    #:break (eq? 'done (runtime-state rt))
    (case (car phrase)
      [(compiled-phrase)
       (compile-phrase rt phrase)]

      [(deferred-phrase)
       (defer-phrase rt phrase)]

      [(executed-phrase)
       (execute-phrase rt phrase)]

      [else
       (error "unknown phrase type" phrase)])))

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
     ,(for/list ([sub-phrase (cdr phrase)])
        (case (car sub-phrase)
          [(simple-executed-phrase executed-phrase-compiled-as-word executed-phrase-compiled-as-double-word)
           (expand-executed-phrase sub-phrase)]

          [(compiled-phrase)
           (expand-compiled-phrase sub-phrase)]

          [else (error "unknown deferred phrase type" sub-phrase)]))))

(define (expand-executed-phrase phrase)
  `(executed-phrase
    ,(case (car phrase)
       [(simple-executed-phrase) (executed-atoms (cdr phrase))]
       [(executed-phrase-compiled-as-word) `(,@(executed-atoms (cdr phrase)) (compile-literals 1))]
       [(executed-phrase-compiled-as-double-word) `(,@(executed-atoms (cdr phrase)) (compile-literals 2))])))

(define (executed-atoms atoms)
  (for/list ([atom atoms])
    (executed-atom atom)))

(define (compiled-atom atom)
  (case (car atom)
    [(character) `(compile-character ,atom)]
    [(name) `(compile-named-procedure ,atom)]
    [(number) `(compile-number ,atom)]
    [(string) `(compile-string ,atom)]
    [(symbol) `(compile-symbol ,atom)]
    [else `(syntax-error "unknown-atom-type" ,atom)]))

(define (executed-atom atom)
  (case (car atom)
    [(character) `(push-character ,atom)]
    [(name) `(execute-named-procedure ,atom)]
    [(number) `(push-number ,atom)]
    [(string) `(push-string ,atom)]
    [(symbol) `(push-symbol ,atom)]
    [else `(syntax-error "unknown-atom-type" ,atom)]))

(define (compile-phrase rt phrase)
  (for ([atom (cadr phrase)])
    (compile-atom rt atom)))

(define (compile-atom rt atom)
  (let ([v (cadr atom)])
    (case (car atom)
      [(compile-character compile-number)
       (runtime-compile-atom! rt v)]

      [(compile-named-procedure)
       (runtime-compile-atom! rt `(call ,(runtime-find-definition rt (cadr v))))]

      [(compile-string)
       (runtime-compile-atom! rt `(string-ref ,(runtime-inter-string! rt (cadr v))))]

      [(compile-symbol)
       (runtime-compile-atom! rt `(symbol-ref ,(runtime-inter-symbol! rt (cadr v))))])))

(define (defer-phrase rt phrase)
  (runtime-compile-atom! rt `(defer ,(cadr phrase))))

(define (execute-phrase rt phrase)
  (for ([atom (cadr phrase)])
    (execute-atom rt atom)))

(define (execute-atom rt atom)
  (let ([v (cadr atom)])
    (case (car atom)
      [(push-character push-number)
       (runtime-push! rt v)]

      [(execute-named-procedure)
       (runtime-execute! rt (runtime-find-definition rt (cadr v)))]

      [(push-string)
       (runtime-push! rt `(string-ref ,(runtime-inter-string! rt (cadr v))))]

      [(push-symbol)
       (runtime-push! rt `(symbol-ref ,(runtime-inter-symbol! rt (cadr v))))]

      [(compile-literals)
       (for ([w (for/fold
                    ([result '()])
                    ([i (in-range v)])
                  (cons (runtime-pop! rt) result))])
         (runtime-compile-atom! rt w))]

      [else
       (error "unknown executable atom" atom)])))

(define (interpret-atom rt atom)
  (case (car atom)
    [(call)
     (runtime-call! rt (cadr atom))]

    [(defer)
     (flit-evaluate rt (cadr atom))]

    [(branch-if-0)
     (runtime-branch-if-0! rt (cadr atom))]

    [(jump)
     (runtime-jump! rt (cadr atom))]

    [else
     (runtime-push! rt atom)]))
