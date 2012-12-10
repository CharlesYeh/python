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
     (if (empty? args)
         empty
         (map get-structured-python args))]
    [(hash-table ('nodetype "Attribute")
                 ('value value)
                 ('attr attr)
                 ('ctx ctx))
     (PyGetField (get-structured-python value) (PyStr attr))]
    
    [(hash-table ('nodetype "arg")
                 ('arg arg)
                 ('annotation annotation))
     (string->symbol arg)]

    [(hash-table ('nodetype "GeneratorExp")
                 ('elt elt)
                 ('generators generators))
     (PyGenerator (get-structured-python elt)
                  ; comprehensions
                  (map get-structured-python generators))]
    [(hash-table ('nodetype "comprehension")
                 ('target target)
                 ('iter iter)
                 ('ifs ifs))
     (local ([define t-sym (PyId-x (get-structured-python target))])
       (PyIterator t-sym (get-structured-python iter)))]
    
    ; lambdas automatically "return"
    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (PyFunc #f (get-structured-python args)
             (PyReturn (get-structured-python body)))]
    ; declare a lambda, but don't automatically return
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list decorator_list)
                 ('returns returns))
     (PyAssign (IdLHS (string->symbol name))
               (PyFunc
(if (member (PyId 'classmethod)
                                   (map get-structured-python decorator_list)) #t #f)
                       (get-structured-python args)
                       (get-structured-python body)))]
    [(hash-table ('nodetype "ClassDef")
                 ('name name)
                 ('bases bases)
                 ('keywords keywords)
                 ('starargs starargs)
                 ('kwargs kwargs)
                 ('body body)
                 ('decorator_list decorator_list))
     (PyAssign (IdLHS (string->symbol name))
               (PyClass (cons name (map (lambda (x)
                                          (symbol->string (PyId-x (get-structured-python x))))
                                        bases))
                        (get-structured-python body)))]
    
    [(hash-table ('nodetype "Return")
                 ('value value))
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
    [(hash-table ('nodetype "TryFinally")
                 ('body body)
                 ('finalbody finalbody))
     (PyTryFinally (get-structured-python body)
                   (get-structured-python finalbody))]
    [(hash-table ('nodetype "TryExcept")
                 ('body body)
                 ('orelse orelse)
                 ('handlers handlers))
     (PyTry (get-structured-python body)
            (if (equal? #\nul orelse)
                (PyPass)
                (get-structured-python orelse))
            (map get-structured-python handlers))]
    [(hash-table ('nodetype "Raise")
                 ('exc exc)
                 ('cause cause))
     (if (equal? #\nul exc)
         (PyReraise)
         (PyRaise (get-structured-python exc) (PyPass)))]
    [(hash-table ('nodetype "ExceptHandler")
                 ('type type)
                 ('name name)
                 ('body body))
     (local ([define pytype (if (equal? #\nul type)
                                (PyNone)
                                (get-structured-python type))]
             [define pybody (get-structured-python body)])
       (if (equal? #\nul name)
           (PyExcept pytype pybody)
           (PyNamedExcept (string->symbol name) pytype pybody)))]
    
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    
    ; operators
    [(hash-table ('nodetype "Subscript")
                 ('value value)
                 ('slice slice)
                 ('ctx ctx))
     (PySubscript (get-structured-python value) (get-structured-python slice))]
    [(hash-table ('nodetype "Index")
                 ('value value))
     (indexParams (get-structured-python value))]
#|
    [(hash-table ('nodetype "Slice")
                 ('upper upper)
                 ('lower lower)
                 ('step step))
     (sliceParams )]
|#
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
    
    [(hash-table ('nodetype "AugAssign")
                 ('target target)
                 ('op op)
                 ('value value))
     (local ([define optype (string->symbol (hash-ref op 'nodetype))])
       (PyPrimAssign optype
                     (local ([define lhs (get-structured-python target)])
                       (cond
                         [(PyId? lhs) (IdLHS (PyId-x lhs))]
                         [(PyGetField? lhs) (DotLHS (PyGetField-obj lhs) (PyGetField-field lhs))]
                         [else (error 'get-structured-python "Assignment to invalid destination")]))
                     (get-structured-python value)))]
    [(hash-table ('nodetype "Assign")
                 ('targets targets)
                 ('value value))
     (PyAssign
       (gsp-lhs (get-structured-python (first targets)))
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
     (local ([define numlist (string->list (number->string n))])
       (cond
         [(member (first (string->list ".")) numlist)
          (PyFloat n)]
         [(member (first (string->list "e")) numlist)
          (PyFloat n)]
         [else (PyInt n)]))]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('nodetype "Tuple")
                 ('ctx ctx)
                 ('elts elts))
     (PyList #f
       (map get-structured-python elts))]
    [(hash-table ('nodetype "List")
                 ('elts elts)
                 ('ctx ctx))
     (PyList #t (map get-structured-python elts))]
    [(hash-table ('nodetype "Dict")
                 ('keys keys)
                 ('values values))
     (local ([define htable (make-hash)])
       (begin
         (map (lambda (k v) (hash-set! htable (get-structured-python k)
                                              (get-structured-python v)))
              keys values)
         (PyDict #t htable)))]
    [(hash-table ('nodetype "Set")
                 ('elts elts))
     (local ([define htable (make-hash)])
       (begin
         (map (lambda (k) (hash-set! htable (get-structured-python k)
                                            (PyNone)))
              elts)
         (PyDict #f htable)))]

    [(hash-table ('nodetype "Delete")
                 ('targets targets))
     (PyDelete (ListLHS (map (lambda (t)
                             (gsp-lhs (get-structured-python t)))
                        targets)))]
    

    [_ (begin
         (display pyjson)
         (display "\n")
         (error 'parse "Haven't handled a case yet"))]))

)

;; gsp-lhs

(define (gsp-lhs lhs)
   (cond
     [(PySubscript? lhs)
      (local ([define obj-val (PySubscript-value lhs)]
              [define sub (PySubscript-params lhs)]
              [define sub-val (cond
                                [(indexParams? sub) (indexParams-value sub)]
                                [else (error 'gsp "can't be converted to PyExpr")]
                              )])
        (cond
          [(PyId? obj-val) (DotLHS obj-val sub-val)]
          [else (error 'gsp "can't be converted to LHS")]))]

     [(PyId? lhs) (IdLHS (PyId-x lhs))]
     [(PyGetField? lhs) (DotLHS (PyGetField-obj lhs) (PyGetField-field lhs))]
     [(PyList? lhs) (ListLHS (map gsp-lhs (PyList-values lhs)))]
     [else (error 'get-structured-python "Assignment to invalid destination")]))


