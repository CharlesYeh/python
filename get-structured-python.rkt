#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#
;; get-structured-python : hash-table -> *
;; turns the given parsed AST python tree to a PyExpr, the sugared expression

(define (get-structured-python pyjson)
  (begin
    ;(display pyjson)
    ;(display "\n\n")
    
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
    
    [(list a ...) (PySeq (map get-structured-python a))]
    
    [(hash-table ('nodetype "arguments")
                 ('args args)
                 ('defaults defaults)
                 ('kwargannotation kwargannotation)
                 ('vararg vararg)
                 ('kwarg kwarg)
                 ('varargannotation varargannotation)
                 ('kw_defaults kw_defaults)
                 ('kwonlyargs kwonlyargs))
     (map get-structured-python args)]
    
    [(hash-table ('nodetype "arg")
                 ('arg arg)
                 ('annotation annotation))
     (string->symbol arg)]
                 
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list decorator_list))
     (PyAssign (IdLHS (string->symbol name))
               (PyFunc (map get-structured-python args)
                       (get-structured-python body)))]
    
    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (PyFunc (get-structured-python args)
             (get-structured-python body))]
    
    [(hash-table ('nodetype "Return")
                 ('value value)
     (PyReturn (get-structured-python value))]
    [(hash-table ('nodetype "Break")) (PyBreak)]
    [(hash-table ('nodetype "Continue")) (PyContinue)]
    
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
    [(hash-table ('nodetype "Compare")
                 ('ops ops)
                 ('comparators comparators)
                 ('left left))
     (PyCompare (map (lambda (a) (string->symbol (hash-ref a 'nodetype))) ops)
                (get-structured-python left)
                (map get-structured-python comparators))]
    
    ; error control
    [(hash-table ('nodetype "TryExcept")
                 ('body body)
                 ('orelse orelse)  ; use this!!#############
                 ('handlers handlers))
     (PyTryExcept (get-structured-python body)
                  (map get-structured-python handlers))]
    [(hash-table ('nodetype "Raise")
                 ('exc exc)   ; what is this? ############
                 ('cause cause))
     ;(PyRaise (get-structured-python exc) (get-structured-python cause))
     (PyRaise (get-structured-python exc) (PyPass))]
    [(hash-table ('nodetype "ExceptHandler")
                 ('type type)
                 ('name name)    ; use this #############
                 ('body body))
     (local ([define pytype (get-structured-python type)]
             [define pybody (get-structured-python body)])
       (if (null? name)
           (PyExcept pytype pybody)
           (PyExcept pytype pybody)))]
           ;(PyNamedExcept pytype name pybody))))]
    
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id "None"))
     (PyNone)]
    
    ; operators
    [(hash-table ('nodetype "UnaryOp")
                 ('op op)
                 ('operand operand))
     (local ([define optype (string->symbol (hash-ref op 'nodetype))])
       (PyPrim optype (list (get-structured-python operand))))]
    [(hash-table ('nodetype "BinOp")
                 ('op op)
                 ('left left)
                 ('right right))
     (local ([define optype (string->symbol (hash-ref op 'nodetype))])
       (PyPrim optype (list (get-structured-python left)
                            (get-structured-python right))))]
    [(hash-table ('nodetype "BoolOp")
                 ('op op)
                 ('values values))
     (local ([define optype (string->symbol (hash-ref op 'nodetype))])
       (PyPrim optype (map get-structured-python values)))]
    
    [(hash-table ('nodetype "Assign")
                 ('targets targets)
                 ('value value))
     ; assume is id for now
       (PyAssign (IdLHS (PyId-x
                         (get-structured-python
                          (first targets))))
                 (get-structured-python value))]
    
    [(hash-table ('nodetype "Nonlocal")
                 ('names names))
     (PyNonlocal (map string->symbol names))]
    [(hash-table ('nodetype "Global")
                 ('names names))
     (PyGlobal (map string->symbol names))]

    ; primitives
    [(hash-table ('nodetype "Num")
                 ('n n))
     (match n
       [(hash-table ('nodetype "Complex")
                    ('value cn))
        (PyComplex (string->number (cond
                                     [(equal? (string-replace cn "+" "*") cn)
                                      (string-replace cn "j" "i")]
                                     [else (string-append "0+" (string-replace cn "j" "i"))])))]
       [else (PyInt n)])]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('nodetype "Tuple")
                 ('ctx ctx)
                 ('elts elts))
     (PyTuple
       (map (get-structured-python elts)))]
    [(hash-table ('nodetype "List")
                 ('elts elts)
                 ('ctx ctx))
     (PyList #t (map get-structured-python elts))]
    [(hash-table ('nodetype "Dict")
                 ('keys keys)
                 ('values values))
     (let ([h (make-hash)])
       (begin
         (map (lambda (k v) (hash-set! hs (get-structured-python k)
                                          (get-structured-python v)))
              keys values)
         (PyDict htable)))]

    [_ (begin
         (display pyjson)
         (display "\n")
         (error 'parse "Haven't handled a case yet"))]))

)



