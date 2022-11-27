(define (runtime-compile-bytes! rt bytes)
  (let ([memory-pointer (runtime-memory-pointer rt)])
    (bytes-copy! (runtime-memory rt) memory-pointer bytes)
    (set-runtime-memory-pointer! rt (+ (bytes-length bytes) memory-pointer))))

(define (runtime-compile-integer! rt v)
  (runtime-compile-word! rt v))

(define (runtime-compile-word! rt v)
  (runtime-compile-bytes! rt (runtime-integer->bytes rt v)))

(define (runtime-integer->bytes rt v)
  (integer->integer-bytes v (runtime-word-size rt) #f #t))

(define (runtime-compile-opcode! rt opcode . args)
  (runtime-align-memory-pointer! rt)
  (let ([opcode-value (runtime-opcode-value rt opcode)])
    (case opcode
      [(jump literal)
       (runtime-compile-word! rt opcode-value)
       (runtime-compile-word! rt (car args))]

      [(double-literal)
       (runtime-compile-word! rt opcode-value)
       (runtime-compile-word! rt (car args))
       (runtime-compile-word! rt (cadr args))])))

(define (runtime-opcode-value rt opcode)
  (hash-ref (runtime-opcodes rt) opcode))

(define (runtime-align-memory-pointer! rt)
  (set-runtime-memory-pointer! rt (runtime-align rt (runtime-memory-pointer rt))))

(define (runtime-align rt n)
  (let* ([word-size (runtime-word-size rt)]
         [rem (modulo n word-size)])
    (or (and (zero? rem) n)
        (+ n (- word-size rem)))))

(define opcodes
  '(double-literal
    jump
    literal))

(define (generate-opcode-table word-size)
  (let ([opcode-table (make-hash)]
        [opcode-flag (arithmetic-shift 1 (- (* 8 word-size) 1))])
    (for ([opcode opcodes]
          [i (in-naturals)])
      (hash-set! opcode-table opcode (bitwise-ior opcode-flag i)))
    opcode-table))
