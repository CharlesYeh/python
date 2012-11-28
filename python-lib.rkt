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
  (CFunc #f (list 'to-print) empty
    (CPrim1 'to-print (CId 'to-print))))

(define all-lambda
  (CFunc #f (list 'l) empty
    (CPrim1 'builtin-all (CId 'l))))

(define any-lambda
  (CFunc #f (list 'l) empty
    (CPrim1 'builtin-any (CId 'l))))

(define assert-true-lambda
  (CFunc #f (list 'check-true) empty
    (CIf (CId 'check-true) (CTrue) (CError (CStr "Assert failed")))))

(define assert-false-lambda
  (CFunc #f (list 'check-false) empty
    (CIf (CId 'check-false) (CError (CStr "Assert failed"))
                           (CFalse))))

(define assert-equal-lambda
  (CFunc #f (list 'arg1 'arg2) empty
    (CIf (CPrim2 'Eq (CId 'arg1) (CId 'arg2))
         (CTrue)
         (CError (CStr "Assert failed")))))

(define assert-is-lambda
  (CFunc #f (list 'arg1 'arg2) empty
         (CIf (CPrim2 'Is (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-is-not-lambda
  (CFunc #f (list 'arg1 'arg2) empty
         (CIf (CPrim2 'IsNot (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-in-lambda
  (CFunc #f (list 'arg1 'arg2) empty
         (CIf (CPrim2 'In (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-not-in-lambda
  (CFunc #f (list 'arg1 'arg2) empty
         (CIf (CPrim2 'NotIn (CId 'arg1) (CId 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-raises-lambda
  (CFunc #t (list 'arg1 'arg2 'arg3) (list (CUndefined) (CNone))
         (CTry
           (CApp (CId 'arg2) (CId 'arg3))
           (CError (CStr "Assert failed"))
           (list (CExcept (CId 'arg1) (CTrue))))))

(define filter-lambda
  (CFunc #f (list 'func 'iter) empty
         (CReturn (CPrim2 'builtin-filter
                          (CId 'func)
                          (CPrim1 'to-list (CId 'iter))))))

(define isinstance-lambda
  (CFunc #f (list 'a 'b) empty
         (CReturn (CPrim2 'isinstance (CId 'a) (CId 'b)))))

(define range-lambda
  (CFunc #f (list 'start 'stop 'step) (list (CUndefined) (CNone) (CInt 1))
         ;(CIf (CPrim2 'Eq (CId 'stop) (CNone))
              ; use start as stop, start at 0 ##############
              ;(CReturn (CId 'start))
              (CReturn (CPrim1 'range (CId 'start)))))
;)

(define callable-lambda
  (CFunc #f (list 'arg1) empty
         (CReturn (CPrim2 'Or
                          (CPrim2 'Eq (CPrim1 'tagof (CId 'arg1)) (CStr "function"))
                          ; constructors are callable
                          (CPrim2 'Eq (CPrim1 'tagof (CId 'arg1)) (CStr "class"))))))

(define len-lambda
  (CFunc #f (list 'arg1) empty
         (CReturn (CPrim1 'len (CId 'arg1)))))

(define bool-lambda
  (CFunc #f (list 'arg1) (list (CFalse))
    (CReturn
      (CIf (CId 'arg1)
           (CTrue)
           (CFalse)))))

(define int-lambda
  (CFunc #f (list 'arg1) empty
    (CReturn
      (CIf (CId 'arg1)
           (CInt 1)
           (CInt 0)))))

(define float-lambda
  (CFunc #f (list 'arg1) empty
    (CReturn
      (CIf (CId 'arg1)
           (CFloat 1)
           (CFloat 0)))))

(define str-lambda
  (CFunc #f (list 'arg1) empty
    (CReturn (CPrim1 'to-string (CId 'arg1)))))

(define tuple-lambda
  (CFunc #f (list 'arg1) (list (CList #f empty))
    (CReturn (CPrim1 'to-tuple (CId 'arg1)))))

(define list-lambda
  (CFunc #f (list 'arg1) (list (CList #t empty))
    (CReturn (CPrim1 'to-list (CId 'arg1)))))

(define abs-lambda
  (CFunc #f (list 'arg1) empty
         (CLet 'n-arg (CApp (CId 'float) (CList #f (list (CId 'arg1))))
           (CReturn (CIf (CPrim2 'Gt (CInt 0) (CId 'n-arg))
                         (CPrim1 'USub (CId 'n-arg))
                         (CId 'n-arg))))))

(define true-val
  (CTrue))

(define false-val
  (CFalse))

(define none-val
  (CNone))

(define IndexError-def
  (CClass (list "IndexError" "BaseException") (make-hash empty)))

(define KeyError-def
  (CClass (list "KeyError" "BaseException") (make-hash empty)))

(define TypeError-def
  (CClass (list "TypeError" "BaseException") (make-hash empty)))

(define RuntimeError-def
  (CClass (list "RuntimeError" "BaseException")
          (local ([define fields (make-hash empty)])
            (begin
              (hash-set! fields (CStr "message") (CStr ""))
              (hash-set! fields (CStr "__init__") (CFunc #f
                                                         (list 'self 'message)
                                                         (list (CStr "No active exception"))
                                                         (CSetField (CId 'self) (CStr "message") (CId 'message))))
              (hash-set! fields (CStr "__str__") (CFunc #f
                                                        (list 'self)
                                                        empty
                                                        (CReturn (CGetField (CId 'self) (CStr "message")))))
              fields
              ))))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val)
        (bind 'False false-val)
        (bind 'None none-val)

        (bind 'IndexError KeyError-def)
        (bind 'KeyError KeyError-def)
        (bind 'TypeError TypeError-def)
        (bind 'RuntimeError RuntimeError-def)

        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-is-not-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-not-in-lambda)
        (bind '___assertRaises assert-raises-lambda)

        (bind 'filter filter-lambda)
        (bind 'isinstance isinstance-lambda)
        (bind 'all all-lambda)
        (bind 'any any-lambda)

        (bind 'range range-lambda)
        (bind 'len len-lambda)
        (bind 'callable callable-lambda)
        (bind 'bool bool-lambda)
        (bind 'int int-lambda)
        (bind 'float float-lambda)
        (bind 'str str-lambda)
        (bind 'tuple tuple-lambda)
        (bind 'list list-lambda)
        (bind 'abs abs-lambda)
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


