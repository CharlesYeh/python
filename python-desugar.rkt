#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar expr)
  (type-case PyExpr expr
    [PySeq (es) (if (empty? es)
                    (CPass)
                    (foldl (lambda (e1 e2)
                             (CSeq e2
                                   (desugar e1)))
                           (desugar (first es))
                           (rest es)))]
    
    [PyId (x) (CId x)]
    
    #;[PyFunc (args body) ...]
    [PyApp (fun args) (CApp (desugar fun)
                            (map desugar args))]
    
    ; control
    [PyIf (cond then els) (CIf (desugar cond)
                                (desugar then)
                                (desugar els))]
    
    
    [PyInt (n) (CNum n)]
    #;[PyFloat (n) ...]
    #;[PyTuple (values) ...]
    #;[PyList (values) ...]
    [PyStr (s) (CStr s)]
    #;[PyTrue () ...]
    #;[PyFalse () ...]
    
    [PyCompare (ops left args) (desugar-compare ops left args)]
    [PyPrim (op args)
            (if (= 1 (length args))
                (CPrim1 op (desugar (first args)))
                (foldr (lambda (a res)
                         (CPrim2 op res
                                    (desugar a)))
                       (desugar (first args))
                       (rest args)))]
    
    ; EVAN: Do these exist in python?
    #;[PyPreInc (id) ...]
    #;[PyPostInc (id) ...]
    #;[PyPreDec (id) ...]
    #;[PyPostDec (id) ...]
    
    ; error control
    [PyTryExcept (body excepts) (CTryExcept (desugar body)
                                            (map desugar excepts))]
    #;[PyTryElseExcept (body else excepts) ...]
    [PyRaise (exc cause) (CError (desugar exc))]
    #;[PyReRaise () ...]
    [PyExcept (type body) (CExcept (desugar type) (desugar body))]
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
            (error 'desugar "Haven't handled a case yet"))]))

(define (desugar-compare ops left args)
  (desugar-compare-helper ops (desugar left) (map desugar args)))


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
