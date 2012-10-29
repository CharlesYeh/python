#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  (CFunc (list 'to-print)
    (CPrim1 'print (CId 'to-print))))

(define assert-true-lambda
  (CFunc (list 'check-true)
    (CIf (CId 'check-true) (CTrue) (CError (CStr "Assert failed")))))

(define assert-equal-lambda
  (CFunc (list 'arg1 'arg2)
    (CIf (CPrim2 'Eq (CId 'arg1) (CId 'arg2))
         (CTrue)
         (CError (CStr "Assert failed")))))

#;(define len
  (CFunc (list 'the-list)
         (CPrim1 'len (CId 'the-list))))

(define true-val
  (CTrue))

(define false-val
  (CFalse))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        ;(bind 'len len)
        (bind 'True true-val)
        (bind 'False false-val)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertEqual assert-equal-lambda)

))

(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


