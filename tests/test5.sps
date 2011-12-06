;;; A black-box test program for the Nat and Fibonacci ADTs
;;; specified by test5.

(import (rnrs base)
        (rnrs exceptions)        ; for the guard syntax
        (rnrs io simple)         ; for display etc
        (testing Nat)
        (testing Factorial))

;;; Counters for the summary report when testing is complete.

(define tests-run 0)
(define tests-passed 0)
(define tests-failed 0)

;;; Syntax to make testing more convenient.
;;;
;;; (test <name> <expr>) evaluates <expr>.
;;; If <expr> evaluates to a true value (any value other #f),
;;; then the test has been passed.
;;; If <expr> evaluates to #f, then the test has been failed.
;;; If an exception occurs during evaluation of <expr>, then
;;; the test has been failed.

(define-syntax test
  (syntax-rules ()
    ((_ name expr)
     (begin (set! tests-run (+ tests-run 1))
            (if (guard (exn (else #f))
                  expr)
                (set! tests-passed (+ tests-passed 1))
                (begin (set! tests-failed (+ tests-failed 1))
                       (display "Failed test: ")
                       (display name)
                       (newline)))))))

;;; The black-box tests.

(test "pred1" (= 0 (asInt (pred (succ (zero))))))
(test "pred2" (= 1 (asInt (pred (succ (succ (zero)))))))
(test "pred4" (= 3 (asInt (pred (succ (succ (succ (succ (zero)))))))))

(test "asInt0" (= 0 (asInt (zero))))
(test "asInt1" (= 1 (asInt (succ (zero)))))
(test "asInt3" (= 3 (asInt (succ (succ (succ (zero)))))))

(test "eqNat00" (eqNat (zero) (zero)))
(test "eqNat01" (not (eqNat (zero) (succ (zero)))))
(test "eqNat10" (not (eqNat (succ (zero)) (zero))))
(test "eqNat22" (eqNat (succ (succ (zero))) (succ (succ (zero)))))
(test "eqNat21" (not (eqNat (succ (succ (zero))) (succ (zero)))))
(test "eqNat12" (not (eqNat (succ (zero)) (succ (succ (zero))))))

(test "plus00" (= 0 (asInt (plus (zero) (zero)))))
(test "plus20" (= 2 (asInt (plus (succ (succ (zero))) (zero)))))
(test "plus21" (= 3 (asInt (plus (succ (succ (zero))) (succ (zero))))))
(test "plus02" (= 2 (asInt (plus (zero) (succ (succ (zero)))))))
(test "plus23" (= 5 (asInt (plus (succ (succ (zero)))
                                 (succ (succ (succ (zero))))))))

(test "times00" (= 0 (asInt (times (zero) (zero)))))
(test "times20" (= 0 (asInt (times (succ (succ (zero))) (zero)))))
(test "times21" (= 2 (asInt (times (succ (succ (zero))) (succ (zero))))))
(test "times02" (= 0 (asInt (times (zero) (succ (succ (zero)))))))
(test "times23" (= 6 (asInt (times (succ (succ (zero)))
                                   (succ (succ (succ (zero))))))))

(test "power00" (= 1 (asInt (power (zero) (zero)))))
(test "power20" (= 1 (asInt (power (succ (succ (zero))) (zero)))))
(test "power21" (= 2 (asInt (power (succ (succ (zero))) (succ (zero))))))
(test "power02" (= 0 (asInt (power (zero) (succ (succ (zero)))))))
(test "power23" (= 8 (asInt (power (succ (succ (zero)))
                                   (succ (succ (succ (zero))))))))

(test "minus00" (= 0 (asInt (minus (zero) (zero)))))
(test "minus20" (= 2 (asInt (minus (succ (succ (zero))) (zero)))))
(test "minus21" (= 1 (asInt (minus (succ (succ (zero))) (succ (zero))))))
(test "minus33" (= 0 (asInt (minus (succ (succ (succ (zero))))
                                   (succ (succ (succ (zero))))))))
(test "minus31" (= 2 (asInt (minus (succ (succ (succ (zero))))
                                (succ (zero))))))

(test "fact0" (= 1 (asInt (factorial (zero)))))
(test "fact1" (= 1 (asInt (factorial (succ (zero))))))
(test "fact2" (= 2 (asInt (factorial (succ (succ (zero)))))))
(test "fact3" (= 6 (asInt (factorial (succ (succ (succ (zero))))))))
(test "fact5"
      (let* ((two (succ (succ (zero))))
             (five (succ (power two two))))
        (eqNat 120 (asInt (factorial five)))))

;;; Summary of results.

(display "SUMMARY: failed ")
(display tests-failed)
(display " of ")
(display tests-run)
(display " tests.")
(newline)

;;; Sanity check.

(if (not (= tests-run
            (+ tests-passed tests-failed)))
    (begin (display "Oops...") (newline)))
