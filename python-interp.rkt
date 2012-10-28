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
        (if (equal? name (binding-name node))
            (type-case (optionof CVal) (hash-ref store (binding-value node))
              [some (val)
                    (type-case CVal val
                      [VUndefined () (lookup-defined name (rest env) store)]
                      [else (binding-value node)])]
              [none () -1])
            (lookup-defined name (rest env) store)))))
                      

;; interp-env : CExp Env Store -> CVal
(define (interp-env (expr : CExp) (env : Env) (store : Store)) : AnswerC
  (type-case CExp expr
    [CNum (n) (ValueA (VInt n) store)]
    [CStr (s) (ValueA (VStr s) store)]

    [CTrue () (ValueA (VTrue) store)]
    [CFalse () (ValueA (VFalse) store)]

    [CError (e) (error 'interp (to-string (interp-env e env store)))]

    [CIf (i t e)
      (local ([define cond (type-case CVal (interp-env i env store)
                             [VTrue () #t]
                             [VInt (n) (not (= 0 n))]
                             [VStr (s) (not (equal? s ""))]
                             [else #f])])
        (if cond
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
          (local ([define loc (new-loc)])
            (begin
              (hash-set! store loc (interp-env bind env store))
              (interp-env body
                          (cons (binding x loc) env)
                          store)))]

    [CSeq (e1 e2)
      (begin (interp-env e1 env store) (interp-env e2 env store))]

    [CApp (fun arges) (interp-app fun arges env store)]

    [CFunc (args body) (VClosure env args body)] 

    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env store))]
    [else (VTrue)]))

;; interp-app : ExprC (listof ExprC) Env Store -> AnswerC
;; interprets function applications, checking for exceptions
(define (interp-app [func : CExp] [args : (listof CExp)] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env func env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ValueA (value store)
         (type-case CVal value
           [VClosure (a b e)
                         (interp-app-helper value a args e env store)]
           
           [else (interp-error (string-append "Applied a non-function: " (pretty value)) store)])]))

;; interp-app-helper : ValueC (listof symbol) (listof ExprC) Env Store -> AnswerC
;; interprets a function application, by first recursively binding the arguments
(define (interp-app-helper [closure : CVal] [params : (listof symbol)] [args : (listof CExp)]
                           [closureEnv : Env] [appEnv : Env] [store : Store]) : AnswerC
  (if (empty? args)
      (if (empty? params)
          (interp-env (VClosure-body closure) closureEnv store) ; apply!!
          (interp-error "Application failed with arity mismatch" store))
      ; add arg/symbol and check if there are more args
      (if (empty? params)
          (interp-error "Application failed with arity mismatch" store)
          (local ([define newloc (new-loc)]
                  [define var-name (first params)])
            ; evaluate argument value
            (type-case AnswerC (interp-env (first args) appEnv store)
              [ExceptionA (exn-val store) (ExceptionA exn-val store)]
              [ValueA (value store)
                      (begin
                        (hash-set! store newloc value)
                        (interp-app-helper closure (rest params) (rest args)
                                           (cons (binding var-name newloc) closureEnv)
                                           appEnv
                                           store))])))))

;; interp : CExp -> CVal
(define (interp expr)
  (interp-env expr (list) (hash (list))))

