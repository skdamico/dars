;;; An implementation of the Nat ADT in R6RS Scheme.

(library (testing Nat)

  (export zero succ pred asInt eqNat
          plus times power minus)

  (import (rnrs base))

  ;; Basic creators.

  (define (zero) '(zero))

  (define (succ m) (list 'succ m))

  ;; Other exported operations.

  (define (pred m)
    (if (and (list? m) (eq? 'succ (car m)))
        (cadr m)
        (error 'pred "illegal argument" m)))

  (define (asInt m)
    (case (car m)
     ((zero) 0)
     ((succ) (+ 1 (asInt (cadr m))))
     (else (error 'asInt "illegal argument" m))))

  (define (eqNat m n)
    (equal? m n))

  (define (plus m n)
    (case (car n)
     ((zero) m)
     (else (succ (plus m (cadr n))))))

  (define (times m n)
    (case (car n)
     ((zero) (zero))
     (else (plus m (times m (cadr n))))))

  (define (power m n)
    (case (car n)
     ((zero) (succ (zero)))
     (else (times m (power m (cadr n))))))

  (define (minus m n)
    (case (car n)
     ((zero) m)
     (else (pred (minus m (cadr n))))))

  )
