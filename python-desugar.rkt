#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

;; desugar : PyExpr -> CExp
;; desugars the given visible language expression to the core language
(define (desugar expr)
  (local ([define global-vars (get-vars #t #f #t expr)]
          ; get local, and nonlocal in inner scopes
          [define global-local-vars (get-vars #f #f #t expr)]
          [define global-all-vars (append global-vars global-local-vars)])
    (get-vars-declare-then-desugar empty global-all-vars expr)))

;; desugar-helper : PyExpr -> CExp
;; same as desugar, but used for non-top level expressions
(define (desugar-helper expr)
  (begin
    ;(display expr)
    ;(display "\n\n")
  (type-case PyExpr expr
    [PySeq (es) (if (empty? es)
                    (CPass)
                    (foldl (lambda (e1 e2)
                             (CSeq e2
                                   (desugar-helper e1)))
                           (desugar-helper (first es))
                           (rest es)))]
    
    ; objects/functions
    [PyClass (bases body) (CClass bases (get-vars-then-desugar empty body))]
    [PyFunc (args body) (CFunc #f args empty (get-vars-then-desugar args body))]
    [PyApp (fun args) (CApp (desugar-helper fun)
                            (CList #f (map desugar-helper args)))]
    [PyReturn (value) (CReturn (desugar-helper value))]
    
    ; data types
    [PyDict (has-values htable) (local ([define new-hash (make-hash empty)])
                       (begin
                         (map (lambda (key)
                                (hash-set! new-hash
                                           (desugar-helper key)
                                           (desugar-helper (some-v (hash-ref htable key)))))
                              (hash-keys htable))
                         (CDict has-values new-hash)))]
    [PyList (mutable fields) (CList mutable (map desugar-helper fields))]
    [PyInt (n) (CInt n)]
    [PyFloat (n) (CFloat n)]
    [PyStr (s) (CStr s)]
    [PyTrue () (CTrue)]
    [PyFalse () (CFalse)]
    [PyNone () (CNone)]
    
    ; var manipulation
    [PyId (x) (CGet (CIdLHS x))]
    [PyGetField (obj field) (CGet (CDotLHS (desugar-helper obj) (desugar-helper field)))]
    [PyDelete (lhs) (CDel (desugar-lhs lhs))]
    [PyAssign (lhs value) (CSet (desugar-lhs lhs) (desugar-helper value))]
    [PyPrimAssign (op lhs value)
                  (local ([define d-lhs (desugar-lhs lhs)])
                    (CSet d-lhs (CPrim2 op (CGet d-lhs) (desugar-helper value))))]
    [PyPrim (op args)
            (if (= 1 (length args))
                (CPrim1 op (desugar-helper (first args)))
                (foldr (lambda (a res)
                         (desugar-prim2-mapping op res (desugar-helper a)))
                       (desugar-helper (first args))
                       (rest args)))]
    
    ; scope
    [PyNonlocal (vars) (CPass)]
    [PyGlobal (vars) (CPass)]
    
    ; control
    [PyIf (cond then els) (CIf (desugar-helper cond)
                                (desugar-helper then)
                                (desugar-helper els))]
    [PyCompare (ops left args) (desugar-compare ops left args)]

    ; error control
    [PyTry (body orelse excepts)
           (CTry (desugar-helper body)
                 (desugar-helper orelse)
                 (map desugar-helper excepts))]
    [PyTryFinally (body final)
                  (CTryFinally (desugar-helper body)
                               (desugar-helper final))]
    [PyExcept (type body) (CExcept (desugar-helper type) (desugar-helper body))]
    [PyNamedExcept (name type body) (CNamedExcept name (desugar-helper type) (desugar-helper body))]

    [PyRaise (exc cause) (CError (desugar-helper exc))]
    [PyReraise () (CError (CApp (CGet (CIdLHS 'RuntimeError)) (CList #f empty)))]
    
    ; loops
    #;[PyWhile (test body) ...]
    #;[PyForElse (id seq body else-exp) ...]
    
    [PyIterator (id iter) (CIterator id (desugar-helper iter))]
    [PyGenerator (value-gen iters) (CGenerator (desugar-helper value-gen) (map desugar-helper iters))]
    ;[PyYield (value) (CYield (desugar-helper value))]
    
    [PyPass () (CPass)]

    [else (begin
            (display expr)
            (error 'desugar "Haven't handled a case yet"))]))
)

(define (desugar-lhs [lhs : LHS]) : CLHS
  (type-case LHS lhs
    [IdLHS (id) (CIdLHS id)]
    [DotLHS (obj field) (CDotLHS (desugar-helper obj)
                                 (desugar-helper field))]
    [ListLHS (li) (CListLHS (map desugar-lhs li))]))

#|
(define (desugar-class [bases : (listof string)] [body : PyExpr]) : CExp
   (local ([define fields (make-hash empty)])
     (begin
       ; add field vars
       (type-case PyExpr body
         [PySeq (es)
           (begin
             ; place all definitions into fields
             (map (lambda (exp)
                    ; separate out class definition
                    (type-case PyExpr exp
                      [PyAssign (lhs value)
                                ; put definition into field of class obj
                                (type-case LHS lhs
                                  [IdLHS (id)
                                         (hash-set! fields (CStr (symbol->string id)) (desugar-helper value))]
                                  [else (void)])]
                      [else (void)]))
                  es)
             (CSeq
              ; non-field expressions go here
              (foldr (lambda (expr result)
                       (type-case PyExpr expr
                         [PyAssign (lhs value) result]
                         [else (CSeq (desugar-helper expr)
                                     result)]))
                     (CPass)
                     es)
              (CClass bases fields)))]
         ; class body should be a pyseq
         [else (error 'desugar "Class body isn't seq")]))))
|#

;; get-vars-then-desugar : ExprP -> ExprC
;; calls helper function which sets vars to undefined, then desugars expression
(define (get-vars-then-desugar [declared : (listof symbol)] [exprP : PyExpr]) : CExp
  ; don't re-declare global and non-local vars
  (local ([define declared-vars (append declared (get-vars #t #t #f exprP))])
    (get-vars-declare-then-desugar declared-vars (get-vars #f #f #t exprP) exprP)))

;; get-vars-declare-then-desugar : (listof symbol) ExprP -> ExprC
;; defines the vars of an expression to be undefined at the start, then desugars the expression
(define (get-vars-declare-then-desugar [declared : (listof symbol)] [vars : (listof symbol)] [body : PyExpr]) : CExp
(begin
  (foldr (lambda (var expr)
           ; add var declaration if not yet declared
           (if (member var declared)
               expr
               (CLet var (CUndefined) expr)))
         (desugar-helper body)
         vars))
)

;; get-vars : PyExpr -> (listof symbol)
;; finds all Var declarations in an expression
;; global = return PyGlobal vars only
;; nonlocal = return Nonlocal vars only
;; extend = extend into further scopes. If global, then extends all the way. If nonlocal, then extends one
(define (get-vars [global : boolean] [nonlocal : boolean] [extend : boolean] [exprP : PyExpr]) : (listof symbol)
(local ([define gv-lambda (lambda (expr) (get-vars global nonlocal extend expr))])
  (type-case PyExpr exprP
    ; lifted to top of FuncP, and not above it, but look for nonlocals if currently looking for locals
    ; extend = global, if looking for nonlocals, then global = false so it only goes to the next
    [PyClass (supers body) (if extend (get-vars global (not global) global body) empty)]
    [PyFunc (args body) (if extend (get-vars global (not global) global body) empty)]

    [PyApp (func args) empty]
    [PyId (name) empty]
    
    [PyWhile (test body) (gv-lambda body)]
    
    [PyForElse (id seq body orelse) (cons id
                                    (append (gv-lambda seq)
                                            (gv-lambda body)))]
    
    [PyPrim (op args) empty]
    [PyPrimAssign (op lhs value) (gv-lambda value)]

    ; IF NONLOCAL, add var from Nonlocal
    [PyNonlocal (vars) (if nonlocal vars empty)]
    [PyGlobal (vars) (if global vars empty)]
    
    ; IF LOCAL, then add var from PyAssign
    [PyAssign (lhs value) (if (and (not global) (not nonlocal))
                              (append (get-vars-lhs lhs) (gv-lambda value))
                              (gv-lambda value))]
    
    [PySeq (es) (cond
                 [(= 0 (length es)) empty]
                 [else (foldl (lambda (expr seq) (append (gv-lambda expr) seq))
                              empty
                              es)])]
    [PyIf (cond then els) (append (gv-lambda cond)
                                 (append (gv-lambda then)
                                         (gv-lambda els)))]
    
    [PyTry (body els excepts)
           (append (append (gv-lambda body) (gv-lambda els))
                   (foldl (lambda (exc vars) (append (gv-lambda exc) vars))
                          empty
                          excepts))]
    [PyExcept (type body) (gv-lambda body)]
    [PyNamedExcept (name type body) (gv-lambda body)]
    [PyTryFinally (body final) (append (gv-lambda body) (gv-lambda final))]
    
    [PyInt (n) empty]
    [PyStr (s) empty]
    [PyTrue () empty]
    [PyFalse () empty]
    
    [else empty]))
)

(define (get-vars-lhs [lhs : LHS]) : (listof symbol)
  (type-case LHS lhs
    [IdLHS (id) (list id)]
    [ListLHS (li) (foldl (lambda (lhs vars)
                           (append (get-vars-lhs lhs) vars))
                         empty li)]
    [else empty]))

;; desugar-compare : (listof symbol) PyExpr (listof PyExpr) -> CExp
;; desugars a string of comparisons
(define (desugar-compare ops left args)
  (desugar-compare-helper ops (desugar-helper left) (map desugar-helper args)))

;; desugar-compare-helper (listof symbol) PyExpr (listof PyExpr) -> CExp
;; desugars the comparisons themselves, ANDing the comparisons together
(define (desugar-compare-helper ops left args)
  (local ([define farg (first args)]
          [define fop (first ops)])
    (if (= 1 (length args))
        (desugar-prim2-mapping fop left farg)
        (CPrim2 'And
                (desugar-prim2-mapping fop left farg)
                (desugar-compare-helper (rest ops) farg (rest args))))))

(define (desugar-prim2-mapping [op : symbol] [left : CExp] [right : CExp]) : CExp
  (case op
    ['NotIn (CPrim1 'Not (CPrim2 'In left right))]
    [else (CPrim2 op left right)]))


