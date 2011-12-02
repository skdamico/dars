def writeBlackBoxer(signatures, output, fileName):
    output.write(';;; A black-box test program for the ')
    if len(signatures) == 1:
        output.write(signatures[0].typename + ' ')
    elif len(signatures) == 2:
        output.write(signatures[0].typename + ' and ' + signatures[1].typename)
    else :
        for sig in signatures[:-1]:
            output.write(sig.typename + ', ')
        output.write('and ' + signatures[-1].typename)
    output.write(' ADTS\n')
    output.write(';;; specified by ' + fileName + '.\n\n')
    output.write('''(import (rnrs base)
            (rnrs exceptions)        ; for the guard syntax
            (rnrs io simple)         ; for display etc\n''')
    if len(signatures) == 1:
        output.write('\t(testing ' + signatures[0].typename + ' ))\n')
    else:
        for sig in signatures[:-1]:
            output.write('        (testing ' + sig.typename + ')\n')
        output.write('\t(testing ' + signatures[-1].typename + '))\n\n')
    output.write(''';;; Counters for the summary report when testing is complete.
    
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
    
    ''')
    
    '''TEST STUFF HERE'''
    
    output.write('''
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
                                                ''')
    