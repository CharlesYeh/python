#lang plai-typed

(require "python-core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (pretty arg)
  (type-case CVal arg
    [VInt (n) (to-string n)]
    [VFloat (n) (to-string n)]
    [VStr (s) (string-append s "\n")]
    
    [VTrue () "True"]
    [VFalse () "False"]
    [VNone () "None"]
    
    [VUndefined () (error 'pretty "Unbound")]
    [VList (mutable fields) (pretty-list fields)]
    [VDict (htable) (pretty-dict htable)]
    
    [VClosure (varargs args defaults body env) (error 'prim (to-string body))]
    [VMethod (inst varargs args defaults body env) (error 'prim (to-string body))]
    [VClass (bases fields) "class"]
    [VInstance (bases fields) "instance"]
    [VGenerator (expr env) "generator"]))
  
(define (pretty-list arg)
  (string-append
    (string-append
      "["
      (foldl
        (lambda (item str)
          (string-append
            str
            (string-append ", "
                           (pretty item))))
        (pretty (first arg))
        (rest arg)))
    "]"))

(define (pretty-dict-pair dict key)
  (string-append
   (string-append
    (pretty key)
    ": ")
   (pretty (some-v (hash-ref dict key)))))

(define (pretty-dict arg)
  (local ([define keys (hash-keys arg)])
   (string-append
    (string-append
     "{"
     (foldl
      (lambda (key str)
        (string-append
         (string-append str
                        ", ")
         (pretty-dict-pair arg key)))
     (pretty-dict-pair arg (first keys))
     (rest keys)))
   "}")))

(define (print arg)
  (display (pretty arg)))

