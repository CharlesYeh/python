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

(define (pretty [arg : CVal]) : string
  (type-case CVal arg
    [VDict (has-values htable) (pretty-dict htable has-values)]
    [VList (mutable fields) (pretty-list fields)]
    [VInt (n) (to-string n)]
    [VFloat (n) (to-string n)]
    [VStr (s) s]
    
    [VTrue () "True"]
    [VFalse () "False"]
    [VNone () "None"]
    
    [VUndefined () (error 'pretty "Unbound")]
    
    [VClosure (varargs args defaults body env) (error 'prim (to-string body))]
    [VMethod (inst varargs args defaults body env) (error 'prim (to-string body))]
    [VClass (bases classdefs fields) "class"]
    [VInstance (bases classdefs fields) (string-append "instanceof: " (first bases))]
    [VGenerator (expr env) "generator"]))
  
(define (pretty-list arg)
  (string-append
    (string-append
      "["
      (if (empty? arg)
          ""
          (foldl
            (lambda (item str)
              (string-append
                str
                (string-append ", "
                               (pretty item))))
            (pretty (first arg))
            (rest arg))))
    "]"))

(define (pretty-dict-pair dict key)
  (string-append
   (string-append
    (pretty key)
    ": ")
   (pretty (some-v (hash-ref dict key)))))

(define (pretty-dict arg has-values)
  (local ([define keys (hash-keys arg)])
   (string-append
    (string-append
     "{"
     (if (empty? keys)
         ""
         (foldl
          (lambda (key str)
            (string-append
             (string-append str
                            ", ")
             (if has-values
                 (pretty-dict-pair arg key)
                 (pretty key))))
          (pretty-dict-pair arg (first keys))
          (rest keys))))
   "}")))

(define (print arg)
  (display (pretty arg)))

