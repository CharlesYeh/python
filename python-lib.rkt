#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

;; get-id : symbol -> CExp
;; convenience method for getting a var value
(define (get-id [var : symbol]) : CExp
  (CGet (CIdLHS var)))

;; throw-error : symbol (listof CExp) -> CExp
;; core expression for throwing the specified error
(define (throw-error [error : symbol] [args : (listof CExp)]) : CExp
  (CApp (get-id error) (CList #f args)))

;-------------------START LIB DEFS-------------------
(define-type-alias Lib (CExp -> CExp))

(define BaseException-def
  (CClass (list "BaseException") (CPass)))

(define Exception-def
  (CClass (list "Exception" "BaseException") (CPass)))

(define StopIteration-def
  (CClass (list "StopIteration" "BaseException") (CPass)))

(define ZeroDivisionError-def
  (CClass (list "ZeroDivisionError" "BaseException") (CPass)))

(define IndexError-def
  (CClass (list "IndexError" "BaseException") (CPass)))

(define KeyError-def
  (CClass (list "KeyError" "BaseException") (CPass)))

(define ValueError-def
  (CClass (list "ValueError" "BaseException") (CPass)))

(define TypeError-def
  (CClass (list "TypeError" "BaseException") (CPass)))

(define NameError-def
  (CClass (list "NameError" "BaseException") (CPass)))

(define UnboundLocalError-def
  (CClass (list "UnboundLocalError" "BaseException") (CPass)))

(define AttributeError-def
  (CClass (list "AttributeError" "BaseException") (CPass)))

(define RuntimeError-def
  (CClass (list "RuntimeError" "BaseException") 
          (local ([define fields (make-hash empty)])
            (CLet 'message (CStr "")
                  (CLet '__init__
                        (CFunc #f
                               (list 'self 'message)
                               (list (CStr "No active exception"))
                               (CSet (CDotLHS (get-id 'self) (CStr "message")) (get-id 'message)))
                        (CLet '__str__
                              (CFunc #f
                                     (list 'self)
                                     empty
                                     (CReturn (CGet (CDotLHS (get-id 'self) (CStr "message")))))
                              (CPass))
              )))))

(define print-lambda
  (CFunc #f (list 'to-print) empty
    (CPrim1 'to-print (get-id 'to-print))))

(define all-lambda
  (CFunc #f (list 'l) empty
    (CReturn (CPrim1 'builtin-all (get-id 'l)))))

(define any-lambda
  (CFunc #f (list 'l) empty
    (CReturn (CPrim1 'builtin-any (get-id 'l)))))

(define locals-lambda
  (CFunc #f empty empty
    (CPrim1 'builtin-locals (CNone))))

(define assert-true-lambda
  (CFunc #f (list 'check-true) empty
    (CIf (get-id 'check-true) (CTrue) (CError (CStr "Assert failed")))))

(define assert-false-lambda
  (CFunc #f (list 'check-false) empty
    (CIf (get-id 'check-false) (CError (CStr "Assert failed"))
                               (CFalse))))

(define assert-equal-lambda
  (CFunc #f (list 'arg1 'arg2) empty
    (CIf (CPrim2 'Eq (get-id 'arg1) (get-id 'arg2))
         (CTrue)
         (CError (CStr "Assert failed")))))

(define assert-is-lambda
  (CFunc #f (list 'arg1 'arg2) empty
         (CIf (CPrim2 'Is (get-id 'arg1) (get-id 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-is-not-lambda
  (CFunc #f (list 'arg1 'arg2) empty
         (CIf (CPrim2 'IsNot (get-id 'arg1) (get-id 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-in-lambda
  (CFunc #f (list 'arg1 'arg2) empty
         (CIf (CPrim2 'In (get-id 'arg1) (get-id 'arg2))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-not-in-lambda
  (CFunc #f (list 'arg1 'arg2) empty
         (CIf (CPrim1 'Not (CPrim2 'In (get-id 'arg1) (get-id 'arg2)))
              (CTrue)
              (CError (CStr "Assert failed")))))

(define assert-raises-lambda
  (CFunc #t (list 'arg1 'arg2 'arg3) (list (CUndefined) (CNone))
         (CTry
           (CApp (get-id 'arg2) (get-id 'arg3))
           (CError (CStr "Assert failed"))
           (list (CExcept (get-id 'arg1) (CTrue))))))

(define filter-lambda
  (CFunc #f (list 'func 'iter) empty
         (CReturn (CPrim2 'builtin-filter
                          (get-id 'func)
                          (CPrim1 'to-list (get-id 'iter))))))

(define isinstance-lambda
  (CFunc #f (list 'a 'b) empty
         (CReturn (CPrim2 'isinstance (get-id 'a) (get-id 'b)))))

(define range-lambda
  (CFunc #f (list 'a) empty (CPass)))

(define callable-lambda
  (CFunc #f (list 'arg1) empty
         (CReturn (CPrim2 'Or
                          (CPrim2 'Eq (CPrim1 'tagof (get-id 'arg1)) (CStr "function"))
                          ; constructors are callable
                          (CPrim2 'Eq (CPrim1 'tagof (get-id 'arg1)) (CStr "class"))))))

(define min-lambda
  (CFunc #f (list 'arg1) empty
         (CReturn (CPrim1 'min (get-id 'arg1)))))

(define max-lambda
  (CFunc #f (list 'arg1) empty
         (CReturn (CPrim1 'max (get-id 'arg1)))))

(define len-lambda
  (CFunc #f (list 'arg1) empty
         (CReturn (CPrim1 'len (get-id 'arg1)))))

(define abs-lambda
  (CFunc #f (list 'arg1) empty
         (CLet 'n-arg (CApp (get-id 'float) (CList #f (list (get-id 'arg1))))
           (CReturn (CIf (CPrim2 'Gt (CInt 0) (get-id 'n-arg))
                         (CPrim1 'USub (get-id 'n-arg))
                         (get-id 'n-arg))))))

(define bool-lambda
  (CClass (list "bool" "int") (CPass)))

(define int-lambda
  (CClass (list "int") (CPass)))

(define float-lambda
  (CFunc #f (list 'arg1) empty
    (CReturn (CIf (get-id 'arg1) (CFloat 1) (CFloat 0)))))

(define str-lambda
  (CFunc #f (list 'arg1) empty
    (CReturn (CPrim1 'to-string (get-id 'arg1)))))

(define tuple-lambda
  (CFunc #f (list 'arg1) (list (CList #f empty))
    (CReturn (CPrim1 'to-tuple (get-id 'arg1)))))

(define list-lambda
  (CFunc #f (list 'arg1) (list (CList #t empty))
    (CReturn (CPrim1 'to-list (get-id 'arg1)))))

(define set-lambda
  (CFunc #f (list 'arg1) (list (CList #t empty))
    (CReturn (CPrim1 'to-set (get-id 'arg1)))))

(define true-val
  (CTrue))

(define false-val
  (CFalse))

(define none-val
  (CNone))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list
        (bind 'BaseException BaseException-def)
        (bind 'Exception Exception-def)
        (bind 'StopIteration StopIteration-def)
        (bind 'ZeroDivisionError ZeroDivisionError-def)
        (bind 'IndexError IndexError-def)
        (bind 'KeyError KeyError-def)
        (bind 'ValueError ValueError-def)
        (bind 'TypeError TypeError-def)
        (bind 'NameError NameError-def)
        (bind 'UnboundLocalError UnboundLocalError-def)
        (bind 'AttributeError AttributeError-def)
        (bind 'RuntimeError RuntimeError-def)

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

        (bind 'print print-lambda)
        (bind 'filter filter-lambda)
        (bind 'isinstance isinstance-lambda)
        (bind 'all all-lambda)
        (bind 'any any-lambda)
        (bind 'locals locals-lambda)
        (bind 'min min-lambda)
        (bind 'max max-lambda)

        (bind 'range range-lambda)
        (bind 'len len-lambda)
        (bind 'abs abs-lambda)
        (bind 'callable callable-lambda)
        
        ; TODO: convert these to classes
        (bind 'int int-lambda)
        (bind 'bool bool-lambda)
        (bind 'float float-lambda)
        (bind 'str str-lambda)
        (bind 'tuple tuple-lambda)
        (bind 'list list-lambda)
        (bind 'set set-lambda)
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


#|
(define range-lambda
  (CFunc #f (list 'start 'stop 'step) (list (CUndefined) (CUndefined) (CInt 1))
         (CSeq
           ; if only one arg, set (stop = start), and (start = 0)
           (CIf (CPrim2 'Eq (get-id 'stop) (CUndefined))
                (CSeq (CSet (CIdLHS 'stop) (get-id 'start)) (CSet (CIdLHS 'start) (CInt 0)))
                (CPass))
           ; test for errors, step = 0
           (CIf (CPrim2 'Eq (get-id 'step) (CInt 0))
                (throw-error 'TypeError empty)
                ; error: start, stop, or step are not ints
                (CIf (CPrim2 'Or
                             (CPrim2 'Or
                                     (CPrim2 'NotEq (CPrim1 'tagof (get-id 'start)) (CStr "int"))
                                     (CPrim2 'NotEq (CPrim1 'tagof (get-id 'start)) (CStr "int")))
                             (CPrim2 'NotEq (CPrim1 'tagof (get-id 'step)) (CStr "int")))
                     (throw-error 'TypeError empty)
                     ; no errors, generate!
                     (CGenerator
                       (CLet 'curr-value (get-id 'start)
                          ; end condition?
                          (CIf (CPrim2 'Or
                                       (CPrim2 'And (CPrim2 'Lt (get-id 'step) (CInt 0))
                                                    (CPrim2 'LtE (get-id 'curr-value) (get-id 'stop)))
                                       (CPrim2 'And (CPrim2 'Gt (get-id 'step) (CInt 0))
                                                    (CPrim2 'GtE (get-id 'curr-value) (get-id 'stop))))
                           (throw-error 'StopIteration empty)
                           (CSeq
                             (CSet (CIdLHS 'start) (CPrim2 'Add (get-id 'start) (get-id 'step)))
                             (CReturn (get-id 'start)))))))))))
|#