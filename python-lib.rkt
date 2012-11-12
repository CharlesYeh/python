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

(define assert-is-lambda
  (CFunc (list 'arg1 'arg2)
         (CIf (CPrim2 'Is (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-is-not-lambda
  (CFunc (list 'arg1 'arg2)
         (CIf (CPrim2 'IsNot (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define callable-lambda
  (CFunc (list 'arg1)
         (CPrim2 'Eq (CPrim1 'tagof (CId 'arg1)) (CStr "function"))))

(define len-lambda
  (CFunc (list 'arg1)
         (CPrim1 'len (CId 'arg1))))

(define bool-lambda
  (CFunc (list 'arg1)
    (CIf (CId 'arg1)
         (CTrue)
         (CFalse))))

(define int-lambda
  (CFunc (list 'arg1)
    (CIf (CId 'arg1)
         (CInt 1)
         (CInt 0))))

(define float-lambda
  (CFunc (list 'arg1)
    (CIf (CId 'arg1)
         (CFloat 1)
         (CFloat 0))))

(define str-lambda
  (CFunc (list 'arg1)
    (CPrim1 'to-string (CId 'arg1))))

(define true-val
  (CTrue))

(define false-val
  (CFalse))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val)
        (bind 'False false-val)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-is-not-lambda)
        (bind 'len len-lambda)
        (bind 'callable callable-lambda)
        (bind 'bool bool-lambda)
        (bind 'int int-lambda)
        (bind 'float float-lambda)
        (bind 'str str-lambda)
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


