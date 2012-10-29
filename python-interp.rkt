#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

(require (typed-in racket (string-length : (string -> number))))

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

(define (get-truth-value value)
  (type-case CVal value
    [VTrue () #t]
    [VInt (n) (not (= 0 n))]
    [VStr (s) (not (equal? s ""))]
    [else #f]))

;; ObjectFields
;; used to allow the object field interpret to return an exception
(define-type ObjectFields
  [ObjFields (fields : (listof FieldV)) (store : Store)]
  [ObjException (exn-val : CVal) (store : Store)])

;; interp-obj-fields : (listof FieldC) Env Store -> (listof FieldV)
;; interprets an object's fields to values
(define (interp-obj-fields [fs : (listof FieldC)] [env : Env] [store : Store]) : ObjectFields
  (if (empty? fs)
      (ObjFields empty store)
      ; interp first, and recurse on rest
      (type-case FieldC (first fs)
        [fieldC (name value)
                ; check value to set to
                (type-case AnswerC (interp-env value env store)
                  [ExceptionA (exn-val store) (ObjException exn-val store)]
                  [ValueA (value store)
                          ; eval rest of fields
                          (type-case ObjectFields (interp-obj-fields (rest fs) env store)
                            [ObjException (exn-val store) (ObjException exn-val store)]
                            [ObjFields (fields store) (ObjFields (cons (fieldV name value) fields) store)])])])))


;; interp-env : CExp Env Store -> CVal
(define (interp-env (expr : CExp) (env : Env) (store : Store)) : AnswerC
  (begin
    ;(display expr)
    ;(display "\n\n")
    
    (type-case CExp expr
    [CInt (n) (ValueA (VInt n) store)]
    [CFloat (n) (ValueA (VInt n) store)]
    [CStr (s) (ValueA (VStr s) store)]

    [CTrue () (ValueA (VTrue) store)]
    [CFalse () (ValueA (VFalse) store)]
    ; is not actually used as a value
    [CUndefined () (ValueA (VUndefined) store)]
      
    [CPass () (ValueA (VUndefined) store)]

    [CError (e) (interp-error (to-string (interp-env e env store)) store)]

    [CIf (i t e)
         (type-case AnswerC (interp-env i env store)
           [ExceptionA (exn-val store) (ExceptionA exn-val store)]
           [ValueA (value store)
                   (if (get-truth-value value)
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
          (type-case AnswerC (interp-env e1 env store)
            [ExceptionA (exn-val store) (ExceptionA exn-val store)]
            [ValueA (value store) (interp-env e2 env store)])]

    [CApp (fun arges) (interp-app fun arges env store)]

    [CFunc (args body) (ValueA (VClosure args body env) store)]

    [CPrim1 (prim arg)
            (interp-prim1 prim arg env store)]
    
    [CPrim2 (prim left right)
            (interp-prim2 prim left right env store)]
    
    [CTryExcept (body excepts)
                (type-case AnswerC (interp-env body env store)
                  [ValueA (value store) (ValueA value store)]
                  [ExceptionA (exn-val store)
                              ; catch exception
                              ;## USE THE CORRECT EXCEPT
                              (interp-env (first excepts)
                                          env
                                          store)])]
    [CExcept (type body)
             (interp-env body env store)]
    [CSet (id value)
          (local ([define loc (lookup id env)])
            (if (= -1 loc)
                (interp-error (string-append "Unbound identifier: " (symbol->string id)) store)
                (type-case AnswerC (interp-env value env store)
                  [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                  [ValueA (val store)
                          (begin
                            (hash-set! store loc val)
                            (ValueA val store))])))]
    
    [else (begin
            (display expr)
            (display "WHAAAT\n\n")
            (ValueA (VFalse) store))]))
  
  )

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
#|
                      ['tagof (type-case CVal value
                                [VStr (s) (VStr "string")]
                                [VInt (n) (VStr "number")]
                                [VObject (fields) (VStr "object")]
                                [VClosure (args body env) (VStr "function")]
                                [VTrue () (VStr "boolean")]
                                [VFalse () (VStr "boolean")]
                                [VUndefined () (VStr "undefined")])]
                      ['len (type-case CVal value
                              [VStr (s) (VInt (string-length s))]
                              [else (VUndefined)])]
|#
                      ; numbers
                      ['USub (type-case CVal value
                               [VInt (n) (VInt (- 0 n))]
                               [else (VUndefined)])]
                      ['Not (type-case CVal value
                              [VTrue () (VFalse)]
                              [VFalse () (VTrue)]
                              [else (VUndefined)])]
                      [else (begin
                              (display op)
                              value)])
                    store)]))

;; interp-prim2 : symbol ExprC ExprC Env Store -> Result
;; interprets a two argument primitive operation
(define (interp-prim2 (op : symbol) (arg1 : CExp) (arg2 : CExp) (env : Env) (store : Store)) : AnswerC
  ; check arg 1
  (type-case AnswerC (interp-env arg1 env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ValueA (value store)
            (local ([define val1 value])
              (type-case AnswerC (interp-env arg2 env store)
                [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                [ValueA (value store)
                        (local ([define val2 value]) 
                          (interp-prim2-helper op val1 val2 env store))]))]))

;; interp-prim2-helper : symbol ValueC ValueC Env Store -> AnswerC
;; interprets prim2 after exception checking is done in the main prim2c interpret function
(define (interp-prim2-helper [op : symbol] [val1 : CVal] [val2 : CVal] [env : Env] [store : Store]) : AnswerC
  (case op
    ['Or
     (ValueA (if
              (or (get-truth-value val1)
                  (get-truth-value val2))
              (VTrue)
              (VFalse))
             store)]
    ['And
     (ValueA (if
              (and (get-truth-value val1)
                  (get-truth-value val2))
              (VTrue)
              (VFalse))
             store)]
    
    ['FloorDiv
     (type-case CVal val1
       [VInt (n) (let ([n1 n])
                   (type-case CVal val2
                     [VInt (n) (let ([n2 n])
                                 (if (= 0 n2)
                                     (interp-error "Division by zero" store)
                                     (ValueA (VInt (/ n1 n2)) store)))]
                     [else (interp-error "Bad arguments for /" store)]))]
       [else (interp-error "Bad arguments for /" store)])]
    ['Mod
     (type-case CVal val1
       [VInt (n) (let ([n1 n])
                   (type-case CVal val2
                     [VInt (n) (let ([n2 n])
                                 (if (= 0 n2)
                                     (interp-error "Division by zero" store)
                                     (ValueA (VInt (modulo n1 n2)) store)))]
                     [else (interp-error "Bad arguments for %" store)]))]
       [else (interp-error "Bad arguments for %" store)])]
    ['Div
     (type-case CVal val1
       [VInt (n) (let ([n1 n])
                   (type-case CVal val2
                     [VInt (n) (let ([n2 n])
                                 (if (= 0 n2)
                                     (interp-error "Division by zero" store)
                                     (ValueA (VInt (/ n1 n2)) store)))]
                     [else (interp-error "Bad arguments for /" store)]))]
       [else (interp-error "Bad arguments for /" store)])]
    ['Eq
     (ValueA (if (equal? val1 val2)
                         (VTrue)
                         (VFalse)) store)]
    ['NotEq
     (ValueA (if (equal? val1 val2)
                 (VFalse)
                 (VTrue)) store)]
    ['string+
     (ValueA (VStr (string-append
                    (VStr-s val1)
                    (VStr-s val2)))
             store)]
    ['Add
     (ValueA (VInt (+
                    (VInt-n val1)
                    (VInt-n val2)))
             store)]
    ['Sub
     (ValueA (VInt (-
                    (VInt-n val1)
                    (VInt-n val2)))
             store)]
    ['== (ValueA (if (equal? val1 val2) (VTrue) (VFalse)) store)]
    ['Gt (type-case CVal val1
          [VInt (n) (let ([n1 n])
                      (type-case CVal val2
                        [VInt (n) (ValueA
                                   (if (> (VInt-n val1) (VInt-n val2))
                                       (VTrue)
                                       (VFalse))
                                   store)]
                        [else (interp-error (string-append "Bad arguments for >:\n"
                                                           (string-append (pretty val1)
                                                                          (string-append "\n"
                                                                                         (pretty val2))))
                                            store)]))]
          [else (type-case CVal val2
                  [VInt (n) (interp-error (string-append "Bad arguments for >:\n" (string-append (pretty val1) (string-append "\n" (pretty val2))))
                                          store)]
                  [else (interp-error (string-append "Bad arguments for >:\n" (string-append (pretty val1) (string-append "\n" (pretty val2))))
                                      store)])])]
    
    ['Lt (type-case CVal val1
          [VInt (n) (let ([n1 n])
                      (type-case CVal val2
                        [VInt (n) (ValueA (if (< (VInt-n val1) (VInt-n val2))
                                              (VTrue)
                                              (VFalse))
                                          store)]
                        [else (interp-error
                               (string-append "Bad arguments for <:\n"
                                              (string-append (pretty val1)
                                                             (string-append "\n" (pretty val2))))
                               store)]))]
          [else (type-case CVal val2
                  [VInt (n) (interp-error (string-append "Bad arguments for <:\n" (string-append (pretty val1) (string-append "\n" (pretty val2))))
                                          store)]
                  [else (interp-error (string-append "Bad arguments for <:\n" (string-append (pretty val1) (string-append "\n" (pretty val2))))
                                      store)])])]))

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
  (interp-env expr empty (make-hash empty)))





