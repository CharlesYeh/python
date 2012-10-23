#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define (get-structured-python pyjson)
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map get-structured-python expr-list))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
    
    ; loops
    [(hash-table ('nodetype "For")
                 ('target target)
                 ('iter iter)
                 ('body body)
                 ('orelse orelse))
     (PyForElse iter
                (get-structured-python target)
                (get-structured-python body)
                (get-structured-python orelse))]
    [(hash-table ('nodetype "While")
                 ('test test)
                 ('body body)
                 ('orelse orelse))
     (PyWhile (get-structured-python test)
              (get-structured-python body))]
    
    ; control
    [(hash-table ('nodetype "If")
                 ('test test)
                 ('body body)
                 ('orelse orelse))
     (PyIf (get-structured-python test)
           (get-structured-python body)
           (get-structured-python orelse))]
    
    [(hash-table ('nodetype "Raise")
                 ('type type)
                 ('inst inst)
                 ('tback tback))
     (PyRaise (get-structured-python inst))]
    
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    
    ; bool
    [(hash-table ('nodetype "BoolOp")
                 ('values values))
     (PyPrim (map get-structured-python values))]
    
    ; primitives
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyInt n)]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    
    [_ (error 'parse (string-append "Haven't handled a case yet"
                                    pyjson))]))

