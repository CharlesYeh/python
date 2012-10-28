#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

;; new-loc : -> Location
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n(add1 (unbox n)))
        (unbox n)))))

(define (lookup-defined (name : symbol) (env : Env) (store : Store)) : Location
    (if (empty? env)
        -1
        (local ([define node (first env)])
          (if (equal? name (bind-name))
              (type-case (optionof CVal) (hash-ref store (bind-value node))
                [some (val)
                      (type-case CVal val
                        
                        )])))))

;; interp-env : CExp Env Store -> CVal
(define (interp-env expr env store)
  (type-case CExp expr
    [CNum (n) (VNum n)]
    [CStr (s) (VStr s)]
    
    [CTrue () (VTrue)]
    [CFalse () (VFalse)]
    
    [CError (e) (error 'interp (to-string (interp-env e env)))]
    
    [CIf (i t e)
         (local ([define condition (type-case CVal (interp-env i env)
                                     [VTrue () #t]
                                     [VNum (n) (not (= 0 n))]
                                     [VStr (s) (not (string=? s ''))]
                                     [else (interp-env e env)])])
           (if condition
               (interp-env t env store)
               (interp-env e env store)))]
    
    [CId (x) (local ([define loc (lookup-defined x env store)])
               (if (= -1 loc)
                   (error 'interp (string-append "Unbound identifier: "
                                                 (symbol->string x)))
                   (type-case (optionof CVal) (hash-ref store loc)
                     [some (v) v]
                     [none () (error 'interp "Unbound identifier")])))]
    
    [CLet (x bind body)
          (interp-env body (hash-set env x (interp-env bind env)))]
    
    [CSeq (e1 e2)
          (begin (interp-env e1 env) (interp-env e2 env))]
    
    [CApp (fun arges)
          (type-case CVal (interp-env fun env)
            [VClosure (cenv argxs body)
                      (local [(define argvs (map (lambda (e) (interp-env e env)) arges))]
                        (interp-env body (bind-args argxs argvs cenv)))]
            [else (error 'interp "Not a closure")])]
    
    [CFunc (args body) (VClosure env args body)]
    
    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env))]
    [else (VTrue)]))

(define (bind-args args vals env)
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (hash-set (bind-args (rest args) (rest vals) env)
                   (first args) (first vals))]))

;; interp : CExp -> CVal
(define (interp expr)
  (interp-env expr (list) (hash (list))))

