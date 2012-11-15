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
  (CFunc (list 'to-print) empty
    (CPrim1 'to-print (CId 'to-print))))

(define assert-true-lambda
  (CFunc (list 'check-true) empty
    (CIf (CId 'check-true) (CTrue) (CError (CStr "Assert failed")))))

(define assert-false-lambda
  (CFunc (list 'check-false) empty
    (CIf (CId 'check-false) (CError (CStr "Assert failed"))
                           (CFalse))))

(define assert-equal-lambda
  (CFunc (list 'arg1 'arg2) empty
    (CIf (CPrim2 'Eq (CId 'arg1) (CId 'arg2))
         (CTrue)
         (CError (CStr "Assert failed")))))

(define assert-is-lambda
  (CFunc (list 'arg1 'arg2) empty
         (CIf (CPrim2 'Is (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-is-not-lambda
  (CFunc (list 'arg1 'arg2) empty
         (CIf (CPrim2 'IsNot (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-in-lambda
  (CFunc (list 'arg1 'arg2) empty
         (CIf (CPrim2 'In (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-not-in-lambda
  (CFunc (list 'arg1 'arg2) empty
         (CIf (CPrim2 'NotIn (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-raises-lambda
  (CFunc (list 'arg1 'arg2 'arg3) empty
         (CTry
           (CSeq (CApp (CId 'arg2) (list (CId 'arg3)))
                 (CError (CStr "Assert failed")))
           (CPass)
           (list (CTrue)))))

(define filter-lambda
  (CFunc (list 'func 'iter) empty
         (CReturn (CPrim2 'builtin-filter (CId 'func) (CPrim1 'to-list (CId 'iter))))))

(define callable-lambda
  (CFunc (list 'arg1) empty
         (CReturn (CPrim2 'Eq (CPrim1 'tagof (CId 'arg1)) (CStr "function")))))

(define len-lambda
  (CFunc (list 'arg1) empty
         (CReturn (CPrim1 'len (CId 'arg1)))))

(define bool-lambda
  (CFunc (list 'arg1) empty
    (CReturn
      (CIf (CId 'arg1)
           (CTrue)
           (CFalse)))))

(define int-lambda
  (CFunc (list 'arg1) empty
    (CReturn
      (CIf (CId 'arg1)
           (CInt 1)
           (CInt 0)))))

(define float-lambda
  (CFunc (list 'arg1) empty
    (CReturn
      (CIf (CId 'arg1)
           (CFloat 1)
           (CFloat 0)))))

(define str-lambda
  (CFunc (list 'arg1) empty
    (CReturn (CPrim1 'to-string (CId 'arg1)))))

(define tuple-lambda
  (CFunc (list 'arg1) (list (CList #f empty))
    (CReturn (CPrim1 'to-tuple (CId 'arg1)))))

(define list-lambda
  (CFunc (list 'arg1) (list (CList #t empty))
    (CReturn (CPrim1 'to-list (CId 'arg1)))))

(define true-val
  (CTrue))

(define false-val
  (CFalse))

(define none-val
  (CNone))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val)
        (bind 'False false-val)
        (bind 'None none-val)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-is-not-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-not-in-lambda)
        (bind '___assertRaises assert-raises-lambda)
        (bind 'filter filter-lambda)
        (bind 'len len-lambda)
        (bind 'callable callable-lambda)
        (bind 'bool bool-lambda)
        (bind 'int int-lambda)
        (bind 'float float-lambda)
        (bind 'str str-lambda)
        (bind 'tuple tuple-lambda)
        (bind 'list list-lambda)
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


