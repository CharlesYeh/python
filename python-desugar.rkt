#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar expr)
  (type-case PyExpr expr
    [PySeq (es) (if (empty? es)
                    (CPass)
                    (foldr (lambda (e1 e2)
                             (CSeq e2
                                   (desugar e1)))
                           (desugar (first es))
                           (rest es)))]
    [PyApp (fun args) (CApp (desugar fun)
                            (map desugar args))]
    
    ; loops
    
    ; control
    [PyIf (cond then els) (CIf (desugar cond)
                                (desugar then)
                                (desugar els))]
    
    ; error control
    [PyTryExcept (body excepts) (CTryExcept (desugar body)
                                            (map desugar excepts))]
    [PyRaise (exc cause) (CError (desugar exc))]
    [PyExcept (type body) (CExcept (desugar type) (desugar body))]
    
    [PyPass () (CPass)]
    
    [PyInt (n) (CNum n)]
    [PyStr (s) (CStr s)]
    [PyId (x) (CId x)]
    
    [PyPrim (op args)
            (if (= 1 (length args))
                (CPrim1 op (desugar (first args)))
                (foldr (lambda (a res)
                         (CPrim2 op res
                                    (desugar a)))
                       (desugar (first args))
                       (rest args)))]
    [PyCompare (ops left args) (desugar-compare ops left args)]
    
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
