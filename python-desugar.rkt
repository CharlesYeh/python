#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

;; desugar : PyExpr -> CExp
;; desugars the given visible language expression to the core language
(define (desugar expr)
  (get-vars-then-desugar empty expr))

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
    
    [PyId (x) (CId x)]
    
    ;############# default params
    [PyClass (bases body)
             (local ([define fields (make-hash empty)])
               (begin
                 (desugar-class body fields)
                 (CClass bases fields)))]
    [PyFunc (args body) (CFunc #f args empty (get-vars-then-desugar args body))]
    [PyApp (fun args) (CApp (desugar-helper fun)
                            (map desugar-helper args))]
    
    ; control
    [PyIf (cond then els) (CIf (desugar-helper cond)
                                (desugar-helper then)
                                (desugar-helper els))]
    
    
    [PyInt (n) (CInt n)]
    [PyFloat (n) (CFloat n)]
    [PyStr (s) (CStr s)]
    [PyList (mutable fields) (CList mutable (map desugar-helper fields))]
    [PyDict (htable) (local ([define new-hash (make-hash empty)])
                       (begin
                         (map (lambda (key)
                                (hash-set! new-hash
                                           (desugar-helper key)
                                           (desugar-helper (some-v (hash-ref htable key)))))
                              (hash-keys htable))
                         (CDict new-hash)))]
    [PyGetField (obj field) (CGetField (desugar-helper obj) (desugar-helper field))]
    
    [PyTrue () (CTrue)]
    [PyFalse () (CFalse)]
    [PyNone () (CNone)]
    
    [PyCompare (ops left args) (desugar-compare ops left args)]
    [PyPrim (op args)
            (if (= 1 (length args))
                (CPrim1 op (desugar-helper (first args)))
                (foldr (lambda (a res)
                         (CPrim2 op res
                                    (desugar-helper a)))
                       (desugar-helper (first args))
                       (rest args)))]
    
    ; error control
    [PyTry (body orelse excepts)
           (CTry (desugar-helper body)
                 (desugar-helper orelse)
                 (map desugar-helper excepts))]
    [PyTryFinally (body final)
                  (CTryFinally (desugar-helper body)
                               (desugar-helper final))]
    [PyRaise (exc cause) (CError (desugar-helper exc))]
    [PyReraise () (CReraise)]
    [PyExcept (type body) (CExcept (desugar-helper type) (desugar-helper body))]
    
    ; loops
    #;[PyWhile (test body) ...]
    #;[PyForElse (id seq body else-exp) ...]
    
    [PyAssign (lhs value)
              (type-case LHS lhs
                [IdLHS (id) (CSet id (desugar value))]
                [else (error 'desugar "Handle other assignments")])]
    ;[BracketLHS (obj field) (SetFieldC (desugar obj) (desugar field) (desugar value))]
    ;[DotLHS (obj field) (SetFieldC (desugar obj) (StrC (symbol->string field)) (desugar value))])]
    
    [PyPrimAssign (op lhs value)
                  (type-case LHS lhs
                    [IdLHS (id) (CSet id (CPrim2 op (CId id) (desugar value)))]
                    [else (error 'desugar "Handle other assignments")])]
    
    [PyPass () (CPass)]
    [PyReturn (value) (CReturn (desugar-helper value))]
    #;[PyBreak () ...]
    #;[PyContinue () ...]
    
    [else (begin
            (display expr)
            (error 'desugar "Haven't handled a case yet"))]))
)

(define (desugar-class (body : PyExpr) (fields : (hashof CExp CExp))) : void
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
             (void))]
    [else (void)]))

;; get-vars-then-desugar-helper : (listof symbol) ExprP -> ExprC
;; defines the vars of an expression to be undefined at the start, then desugars the expression
(define (get-vars-then-desugar-helper [declared : (listof symbol)] [vars : (listof symbol)] [body : PyExpr]) : CExp
  (if (empty? vars)
      (desugar-helper body)
      ; continue defining vars
      (local ([define vname (first vars)])
        (if (member vname declared)
            (get-vars-then-desugar-helper declared (rest vars) body)
            (CLet vname
                  (CUndefined)
                  (get-vars-then-desugar-helper (cons vname declared) (rest vars) body))))))

;; get-vars-then-desugar : ExprP -> ExprC
;; calls helper function which sets vars to undefined, then desugars expression
(define (get-vars-then-desugar [declared : (listof symbol)] [exprP : PyExpr]) : CExp
  (get-vars-then-desugar-helper declared (get-vars exprP) exprP))

;; get-vars : PyExpr -> (listof symbol)
;; finds all Var declarations in an expression
(define (get-vars [exprP : PyExpr]) : (listof symbol)
  (type-case PyExpr exprP
    [PyObject (fields) empty]
    ;[DotP (obj field) empty]
    ;[BracketP (obj field) empty]
    ;[DotMethodP (obj field args) empty]
    ;[BrackMethodP (obj field args) empty]
    
    ; lifted to top of FuncP, and not above it
    [PyFunc (args body) empty]
    [PyApp (func args) empty]
    ; don't lift past Defvar and Deffun
    ;[DefvarP (id bind body) empty]
    ;[DeffunP (name ids funbody body) empty]
    [PyId (name) empty]
    
    [PyWhile (test body) (get-vars body)]
    
    [PyForElse (id seq body orelse) (cons id
                                    (append (get-vars seq)
                                            (get-vars body)))]
    
    [PyAssign (lhs value)
             (type-case LHS lhs
               [IdLHS (id) (cons id (get-vars value))]
               [else (get-vars value)])]
    
    [PySeq (es) (cond
                 [(= 0 (length es)) empty]
                 [(= 1 (length es)) (get-vars (first es))]
                 [else (get-vars-seq es)])]
    [PyIf (cond then els) (append (get-vars cond)
                                 (append (get-vars then)
                                         (get-vars els)))]
    
    [PyInt (n) empty]
    [PyStr (s) empty]
    [PyTrue () empty]
    [PyFalse () empty]
    
    #;[TryCatchP (body param catch)
               (append (get-vars body)
                       (get-vars catch))]
    ;[PyRaise (exn) empty]
    
    [PyPrim (op args) empty]
    [PyPrimAssign (op lhs value) (get-vars value)]
    
    [else empty]))

;; get-vars-seq : (listof ExprP) -> (listof symbol)
;; gets the variable declarations within a sequence
(define (get-vars-seq [es : (listof PyExpr)]) : (listof symbol)
  (if (empty? es)
      empty
      (append (get-vars (first es))
              (get-vars-seq (rest es)))))


;; desugar-compare : (listof symbol) PyExpr (listof PyExpr) -> CExp
;; desugars a string of comparisons
(define (desugar-compare ops left args)
  (desugar-compare-helper ops (desugar-helper left) (map desugar-helper args)))

;; desugar-compare-helper (listof symbol) PyExpr (listof PyExpr) -> CExp
;; desugars the comparisons themselves, ANDing the comparisons together
(define (desugar-compare-helper ops left args)
  (begin
    ;(display args)
    (local ([define farg (first args)]
            [define fop (first ops)])
      (if (= 1 (length args))
          (CPrim2 fop left farg)
          (CPrim2 'And
                  (CPrim2 fop left farg)
                  (desugar-compare-helper (rest ops) farg (rest args))))))
  )

