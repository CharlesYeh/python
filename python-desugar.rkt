#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar expr)
  (get-vars-then-desugar expr))

(define (desugar-helper expr)
  (type-case PyExpr expr
    [PySeq (es) (if (empty? es)
                    (CPass)
                    (foldl (lambda (e1 e2)
                             (CSeq e2
                                   (desugar-helper e1)))
                           (desugar-helper (first es))
                           (rest es)))]
    
    [PyId (x) (CId x)]
    
    [PyFunc (args body) (CFunc args (desugar-helper body))]
    [PyApp (fun args) (CApp (desugar-helper fun)
                            (map desugar-helper args))]
    
    ; control
    [PyIf (cond then els) (CIf (desugar-helper cond)
                                (desugar-helper then)
                                (desugar-helper els))]
    
    
    [PyInt (n) (CInt n)]
    [PyFloat (n) (CFloat n)]
    #;[PyTuple (values) ...]
    #;[PyList (values) ...]
    [PyStr (s) (CStr s)]
    
    [PyTrue () (CTrue)]
    [PyFalse () (CFalse)]
    
    [PyCompare (ops left args) (desugar-compare ops left args)]
    [PyPrim (op args)
            (if (= 1 (length args))
                (CPrim1 op (desugar-helper (first args)))
                (foldr (lambda (a res)
                         (CPrim2 op res
                                    (desugar-helper a)))
                       (desugar-helper (first args))
                       (rest args)))]
    
    ; EVAN: Do these exist in python?
    #;[PyPreInc (id) ...]
    #;[PyPostInc (id) ...]
    #;[PyPreDec (id) ...]
    #;[PyPostDec (id) ...]
    
    ; error control
    [PyTryExcept (body excepts) (CTryExcept (desugar-helper body)
                                            (map desugar-helper excepts))]
    #;[PyTryElseExcept (body else excepts) ...]
    [PyRaise (type inst tback) (CError (desugar-helper inst))]
    #;[PyReRaise () ...]
    [PyExcept (type body) (CExcept (desugar-helper type) (desugar-helper body))]
    #;[PyNamedExcept (type id body) ...]
    
    ; loops
    #;[PyWhile (test body) ...]
    #;[PyFor (id seq body) ...]
    #;[PyForElse (id seq body else-exp) ...]
    
    [PyPass () (CPass)]
    #;[PyBreak () ...]
    #;[PyContinue () ...]
    
    [else (begin
            (display expr)
            (desugar-error "Haven't handled a case yet"))]))


;; get-vars-then-desugar-helper : (listof symbol) ExprP -> ExprC
;; defines the vars of an expression to be undefined at the start, then desugars the expression
(define (get-vars-then-desugar-helper [vars : (listof symbol)] [body : PyExpr]) : CExp
  (if (empty? vars)
      (desugar-helper body)
      ; continue defining vars
      (CLet (first vars) (CUndefined) (get-vars-then-desugar-helper (rest vars) body))))

;; get-vars-then-desugar : ExprP -> ExprC
;; calls helper function which sets vars to undefined, then desugars expression
(define (get-vars-then-desugar [exprP : PyExpr]) : CExp
  (get-vars-then-desugar-helper (get-vars exprP) exprP))

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
    
    [PyFor (id seq body) (cons id
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
    
    ;[PreIncP (lhs) (list lhs)]
    ;[PostIncP (lhs) (list lhs)]
    ;[PreDecP (lhs) (list lhs)]
    ;[PostDecP (lhs) (list lhs)]
    [else empty]))

;; get-vars-seq : (listof ExprP) -> (listof symbol)
;; gets the variable declarations within a sequence
(define (get-vars-seq [es : (listof PyExpr)]) : (listof symbol)
  (if (empty? es)
      empty
      (append (get-vars (first es))
              (get-vars-seq (rest es)))))



(define (desugar-compare ops left args)
  (desugar-compare-helper ops (desugar-helper left) (map desugar-helper args)))

(define (desugar-compare-helper ops left args)
  (begin
    (display args)
    (local ([define farg (first args)]
            [define fop (first ops)])
      (if (= 1 (length args))
          (CPrim2 fop left farg)
          (CPrim2 'And
                  (CPrim2 fop left farg)
                  (desugar-compare-helper (rest ops) farg (rest args))))))
  )
