;;; An implementation of the StackInt ADT in R6RS Scheme.

(library (testing StackInt)

  (export empty push isEmpty top pop)

  (import (rnrs base))

  ;; Basic creators.

  ;; empty:                    -->  StackInt

  (define (empty) '())

  ;; push:     StackInt x int  -->  StackInt

  (define (push s k)
    (cons k s))

  ;; Predicates.

  ;; isEmpty:  StackInt        -->  boolean

  (define (isEmpty s)
    (null? s))

  ;; Accessors.

  ;; top:      StackInt        -->  int

  (define (top s)
    (car s))

  ;; pop:      StackInt        -->  StackInt

  (define (pop s)
    (cdr s))

  ) ; end of library

