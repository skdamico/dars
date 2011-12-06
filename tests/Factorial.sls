;;; An implementation of the Factorial ADT in R6RS Scheme.
;;;
;;; Imports (testing Nat).

(library (testing Factorial)

  (export factorial)

  (import (rnrs base)
          (testing Nat))

  (define (factorial n)
    (if (eqNat n (zero))
        (succ (zero))
        (times n (factorial (pred n)))))

  )
