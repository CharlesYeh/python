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

;; lookup : symbol Env -> Location
;; finds the location of a symbol in the environment
(define (lookup [name : symbol] [env : Env]) : Location
  (if (empty? env)
      ; if undefined, look for previous value
      -1
      (let ([node (first env)])
        (if (equal? name (binding-name node))
            (binding-value node)
            (lookup name (rest env))))))

;; lookup-defined : symbol Env Store -> Location
;; finds the location of a symbol in the environment, searching until there's a defined value
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

    [CError (e) (interp-error (to-string (interp-env e env store)) store)]

    [CIf (i t e)
         (type-case AnswerC (interp-env i env store)
           [ExceptionA (exn-val store) (ExceptionA exn-val store)]
           [ValueA (value store)
                   (if (type-case CVal value
                         [VTrue () #t]
                         [VInt (n) (not (= 0 n))]
                         [VStr (s) (not (equal? s ""))]
                         [else #f])
                       
                       (interp-env t env store)
                       (interp-env e env store))])]

    [CId (x) (local ([define loc (lookup-defined x env store)])
               (if (= -1 loc)
                   (interp-error (string-append "Unbound identifier: "
                                                (symbol->string x)) store)
                   (type-case (optionof CVal) (hash-ref store loc)
                     [some (v) (ValueA v store)]
                     [none () (interp-error "Unbound identifier" store)])))]
    
    [CLet (x bind body)
          ; get value of binding
          (type-case AnswerC (interp-env bind env store)
            [ExceptionA (exn-val store) (ExceptionA exn-val store)]
            [ValueA (value store)
                    ; insert binding
                      (local ([define use-loc (new-loc)]
                              [define use-env (cons (binding x use-loc) env)])
                        (begin
                          (hash-set! store use-loc value)
                          (interp-env body use-env store)))])]

    [CSeq (e1 e2)
      (begin (interp-env e1 env store) (interp-env e2 env store))]

    [CApp (fun arges) (interp-app fun arges env store)]

    [CFunc (args body) (ValueA (VClosure args body env) store)]

    [CPrim1 (prim arg)
            (interp-prim1 prim arg env store)]

    [else (ValueA (VTrue) store)]))

;; interp-prim1 : symbol ExprC Env Store -> Result
;; interprets a single argument primitive operation
(define (interp-prim1 [op : symbol] [arg : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    ; evaluate
    [ValueA (value store)
            (ValueA (case op
                      ['print (begin
                                (display (pretty value))
                                value)]
                      ['tagof (type-case CVal value
                                [VStr (s) (VStr "string")]
                                [VInt (n) (VStr "number")]
                                [VObject (fields) (VStr "object")]
                                [VClosure (args body env) (VStr "function")]
                                [VTrue () (VStr "boolean")]
                                [VFalse () (VStr "boolean")]
                                [VUndefined () (VStr "undefined")])])
                    store)]))

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

