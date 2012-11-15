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

;; get-truth-value : CVal -> boolean
;; the truth value definitions of different types when used as a boolean
(define (get-truth-value value)
  (type-case CVal value
    [VTrue () #t]
    [VInt (n) (not (= 0 n))]
    [VFloat (n) (not (= 0 n))]
    [VStr (s) (not (equal? s ""))]
    [VList (mutable fields) (< 0 (length fields))]
    [VDict (htable) (< 0 (length (hash-keys htable)))]
    [else #f]))

;; ObjectFields
;; used to allow the object field interpret to return an exception
(define-type ObjectFields
  [ObjFields (fields : (listof FieldV)) (store : Store)]
  [ObjException (exn-val : CVal) (store : Store)])

;; interp : CExp -> CVal
(define (interp expr)
  ; catch exception at top level
  (type-case AnswerC (interp-env expr empty (make-hash empty))
    [ExceptionA (exn-val store) (print-error exn-val store)]
    [ReturnA (value store) (print-error value store)]
    [ValueA (val store) val]))

;; interp-env : CExp Env Store -> CVal
;; interprets the given expression, with environment and store
(define (interp-env (expr : CExp) (env : Env) (store : Store)) : AnswerC
  (begin
    ;(display expr)
    ;(display "\n\n")
    
    (type-case CExp expr
      [CInt (n) (ValueA (VInt n) store)]
      [CFloat (n) (ValueA (VInt n) store)]
      [CStr (s) (ValueA (VStr s) store)]
      [CList (mutable fields)
             (type-case ListFields (interp-list-exp fields env store)
               [LException (exn-val store) (ExceptionA exn-val store)]
               [LFields (fields store) (ValueA (VList mutable fields)
                                               store)])]
      [CDict (htable)
             (local ([define new-hash (make-hash empty)])
               (interp-dict-exp htable
                                new-hash
                                (hash-keys htable)
                                env store))]
      [CClass (bases fields)
              (local ([define new-fields (make-hash empty)])
                (type-case AnswerC (interp-dict-exp fields new-fields
                                                    (hash-keys fields)
                                                    env store)
                  [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                  [ReturnA (value store) (ExceptionA value store)]
                  [ValueA (value store) (ValueA (VClass bases new-fields) store)]))]
      [CGetField (obj field) (interp-getfield obj field env store)]
      
      [CTrue () (ValueA (VTrue) store)]
      [CFalse () (ValueA (VFalse) store)]
      [CNone () (ValueA (VNone) store)]
      
      ; is not actually used as a value
      [CUndefined () (ValueA (VUndefined) store)]
      
      [CPass () (ValueA (VUndefined) store)]
      [CReturn (value)
               (type-case AnswerC (interp-env value env store)
                 [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                 [ReturnA (value store) (ExceptionA value store)]
                 [ValueA (value store) (ReturnA value store)])]
      
      [CError (e) (interp-error (to-string (interp-env e env store)) store)]
      
      [CIf (i t e)
           (type-case AnswerC (interp-env i env store)
             [ExceptionA (exn-val store) (ExceptionA exn-val store)]
             [ReturnA (value store) (ReturnA value store)]
             [ValueA (value store)
                     (if (get-truth-value value)
                         (interp-env t env store)
                         (interp-env e env store))])]
      
      [CId (x) (local ([define loc (lookup x env)])
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
              [ReturnA (value store) (ReturnA value store)]
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
              [ReturnA (value store) (ReturnA value store)]
              [ValueA (value store) (interp-env e2 env store)])]
      
      [CApp (fun arges)
            ; catch ReturnA, and change to value
            (type-case AnswerC (interp-app fun arges env store)
              [ExceptionA (exn-val store) (ExceptionA exn-val store)]
              [ReturnA (value store) (ValueA value store)]
              ; functions return None by default
              [ValueA (value store) (ValueA (VNone) store)])]
      [CFunc (args defaults body) (ValueA (VClosure args defaults body env) store)]
      
      [CPrim1 (prim arg) (interp-prim1 prim arg env store)]
      [CPrim2 (prim left right) (interp-prim2 prim left right env store)]
      
      [CTry (body orelse excepts)
            (type-case AnswerC (interp-env body env store)
              [ValueA (value store)
                      (interp-env orelse env store)]
              [ReturnA (value store) (ReturnA value store)]
              [ExceptionA (exn-val store)
                          ; catch exception
                          ;## USE THE CORRECT EXCEPT
                          (interp-env (first excepts) env store)])]
      [CTryFinally (body final)
                   (type-case AnswerC (interp-env body env store)
                     [ExceptionA (exn-val store) (interp-env final env store)]
                     [ReturnA (value store) (interp-env final env store)]
                     [ValueA (value store) (interp-env final env store)])]
      [CExcept (type body)
               (interp-env body env store)]
      [CSet (id value)
            (local ([define loc (lookup id env)])
              (if (= -1 loc)
                  (interp-error (string-append "Unbound identifier: " (symbol->string id)) store)
                  (type-case AnswerC (interp-env value env store)
                    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                    [ReturnA (value store) (ReturnA value store)]
                    [ValueA (val store)
                            (begin
                              (hash-set! store loc val)
                              (ValueA val store))])))]
      
      [else (begin
              (display expr)
              (display "WHAAAT\n\n")
              (ValueA (VFalse) store))]))
  
  )

;; interp-obj-fields : (listof FieldC) Env Store -> (listof FieldV)
;; interprets an object's fields to values
(define (interp-obj-fields [fs : (listof FieldC)] [env : Env] [store : Store]) : ObjectFields
  (if (empty? fs)
      (ObjFields empty store)
      ; interp first, and recurse on rest
      (type-case FieldC (first fs)
        [fieldC (name value)
                (local ([define (obj-lambda value store)
                          (type-case ObjectFields (interp-obj-fields (rest fs) env store)
                            [ObjException (exn-val store) (ObjException exn-val store)]
                            [ObjFields (fields store) (ObjFields (cons (fieldV name value) fields) store)])])
                  ; check value to set to
                  (type-case AnswerC (interp-env value env store)
                    [ExceptionA (exn-val store) (ObjException exn-val store)]
                    ; eval rest of fields
                    [ReturnA (value store) (obj-lambda value store)]
                    [ValueA (value store) (obj-lambda value store)]))])))

;; ListFields
;; used to allow the object field interpret to return an exception
(define-type ListFields
  [LFields (fields : (listof CVal)) (store : Store)]
  [LException (exn-val : CVal) (store : Store)])

(define (interp-list-exp (exprs : (listof CExp)) (env : Env) (store : Store)) : ListFields
  (cond
    [(= 0 (length exprs))
     (LFields empty store)]
    [(= 1 (length exprs))
     (type-case AnswerC (interp-env (first exprs) env store)
       [ExceptionA (exn-val store)
                   (LException exn-val store)]
       [ReturnA (value store) (LFields (list value) store)]
       [ValueA (value store) (LFields (list value) store)])]
    [else
     (local ([define (list-lambda value store)
               (type-case ListFields (interp-list-exp (rest exprs) env store)
                 [LException (exn-val store)
                             (LException exn-val store)]
                 [LFields (fields store)
                          (LFields (cons value fields) store)])])
       (type-case AnswerC (interp-env (first exprs) env store)
         [ExceptionA (exn-val store)
                     (LException exn-val store)]
         [ReturnA (value store) (list-lambda value store)]
         [ValueA (value store) (list-lambda value store)]))]))

(define (interp-dict-exp (old-hash : (hashof CExp CExp))
                         (new-hash : (hashof CVal CVal))
                         (exprs : (listof CExp)) (env : Env) (store : Store)) : AnswerC
  (cond
    [(= 0 (length exprs)) (ValueA (VDict new-hash) store)]
    [else
     ; add current (key -> value) pair
     (local ([define key (first exprs)])
       ; interp key
       (type-case AnswerC (interp-env key env store)
         [ExceptionA (exn-val store) (ExceptionA exn-val store)]
         [ReturnA (value store) (ReturnA value store)]
         [ValueA (value store)
                 (local ([define key-val value])
                   ; interp value
                   (type-case AnswerC (interp-env (some-v (hash-ref old-hash key)) env store)
                     [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                     [ReturnA (value store) (ReturnA value store)]
                     [ValueA (value store)
                             ; recurse to rest of pairs
                             (begin
                               (hash-set! new-hash key-val value)
                               (interp-dict-exp old-hash
                                                new-hash
                                                (rest exprs) env store))]))]))]))


;; interp-prim1 : symbol ExprC Env Store -> Result
;; interprets a single argument primitive operation
(define (interp-prim1 [op : symbol] [arg : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ReturnA (value store) (ReturnA value store)]
    ; evaluate
    [ValueA (value store)
            (ValueA
             (case op
               ['to-print (begin
                            (display (pretty value))
                            value)]
               
               ['to-string (VStr (pretty value))]
               ['to-list (type-case CVal value
                           [VStr (s) (VList #t (map (lambda (s) (VStr (list->string (list s))))
                                                    (string->list s)))]
                           [VList (mutable fields) (VList #t fields)]
                           [else (VUndefined)])]
               ['to-tuple (type-case CVal value
                            [VStr (s) (VList #f (map (lambda (s) (VStr (list->string (list s))))
                                                     (string->list s)))]
                            [VList (mutable fields) (VList #f fields)]
                            [else (VUndefined)])]
               
               ['tagof (type-case CVal value
                         [VStr (s) (VStr "string")]
                         [VInt (n) (VStr "number")]
                         [VFloat (n) (VStr "number")]
                         [VObject (fields) (VStr "object")]
                         [VClosure (args body defaults env) (VStr "function")]
                         [VTrue () (VStr "boolean")]
                         [VFalse () (VStr "boolean")]
                         [VUndefined () (VStr "undefined")]
                         [VNone () (VStr "none")]
                         [VList (mutable fields) (VStr "list")]
                         [VDict (htable) (VStr "hash")]
                         [VClass (bases fields) (VStr "class")])]
               ['len (type-case CVal value
                       [VStr (s) (VInt (string-length s))]
                       [VObject (fields) (VInt (length fields))]
                       [VList (mutable fields) (VInt (length fields))]
                       [VDict (htable) (VInt (length (hash-keys htable)))]
                       [else (VUndefined)])]
               ; numbers
               ['USub (type-case CVal value
                        [VInt (n) (VInt (- 0 n))]
                        [else (VUndefined)])]
               
               ; logical
               ['Not (if (get-truth-value value)
                         (VFalse)
                         (VTrue))]
               [else (begin
                       (display "HANDLE PRIM1: \n")
                       (display op)
                       value)])
             store)]))

;; interp-prim2 : symbol ExprC ExprC Env Store -> Result
;; interprets a two argument primitive operation
(define (interp-prim2 (op : symbol) (arg1 : CExp) (arg2 : CExp) (env : Env) (store : Store)) : AnswerC
  ; check arg 1
  (type-case AnswerC (interp-env arg1 env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ReturnA (value store) (ReturnA value store)]
    [ValueA (value store)
            (local ([define val1 value])
              (type-case AnswerC (interp-env arg2 env store)
                [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                [ReturnA (value store) (ReturnA value store)]
                [ValueA (value store)
                        (local ([define val2 value])
                          (case op
                            ['Is (interp-prim-is arg1 arg2 val1 val2 env store)]
                            ['IsNot (type-case AnswerC (interp-prim-is arg1 arg2 val1 val2 env store)
                                      [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                                      [ReturnA (value store) (ReturnA value store)]
                                      [ValueA (value store)
                                              (ValueA (if (VTrue? value) (VFalse) (VTrue))
                                                      store)])]
                            [else
                             (interp-prim2-helper op val1 val2 env store)]))]))]))

(define (interp-prim-is (arg1 : CExp) (arg2 : CExp)
                        (val1 : CVal) (val2 : CVal)
                        (env : Env) (store : Store)) : AnswerC
  (ValueA
   (if (type-case CVal val1
         ; strings and numbers are checked by value, everything else by address
         ; check addr of lists? #########
         [VStr (s) (equal? val1 val2)]
         [VInt (n) (equal? val1 val2)]
         [VFloat (n) (equal? val1 val2)]
         [VTrue () (equal? val1 val2)]
         [VFalse () (equal? val1 val2)]
         [VList (mutable fields) (and (not mutable) (and (< 0 (length fields)) (equal? val1 val2)))]
         [else #f]
         #;[else
            ; check addr of CExp
            (type-case CExp arg1
              [CId (x1)
                   (type-case CExp arg2
                     [CId (x2) 
                          (if (equal? (lookup x1 env) (lookup x2 env))
                              #t
                              #f)]
                     [else #f])]
              [else #f])])
       (VTrue)
       (VFalse))
   store))

;; interp-prim2-helper : symbol ValueC ValueC Env Store -> AnswerC
;; interprets prim2 after exception checking is done in the main prim2c interpret function
(define (interp-prim2-helper [op : symbol] [val1 : CVal] [val2 : CVal] [env : Env] [store : Store]) : AnswerC
  (case op
    ; BOOLEAN PRIM
    ;['builtin-filter
    ; check filter function
    ;(type-case CVal]
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
    
    ; NUMBER PRIM
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
    ['Mult
     (type-case CVal val1
       [VInt (n) (let ([n1 n])
                   (type-case CVal val2
                     [VInt (n) (let ([n2 n])
                                 (ValueA (VInt (* n1 n2)) store))]
                     [else (interp-error "Bad arguments for *" store)]))]
       [else (interp-error "Bad arguments for *" store)])]
    ['Add
     ; int int -> int, otherwise yield float
     (ValueA 
      (type-case CVal val1
        [VFloat (n1)
                (type-case CVal val2
                  [VFloat (n2) (VFloat (+ n1 n2))]
                  [VInt (n2) (VFloat (+ n1 n2))]
                  [else (VUndefined)])]
        [VInt (n1) 
              (type-case CVal val2
                [VFloat (n2) (VFloat (+ n1 n2))]
                [VInt (n2) (VInt (+ n1 n2))]
                [else (VUndefined)])]
        [else (VUndefined)])
      store)]
    ['Sub
     (ValueA (VInt (-
                    (VInt-n val1)
                    (VInt-n val2)))
             store)]
    
    ; LOGICAL PRIM
    ['Eq
     (ValueA (if (equal? val1 val2)
                 (VTrue)
                 (VFalse))
             store)]
    ['NotEq
     (ValueA (if (equal? val1 val2)
                 (VFalse)
                 (VTrue))
             store)]
    ['In
     (type-case CVal val2
       [VList (mutable fields)
              (ValueA
               (if (member val1 fields)
                   (VTrue)
                   (VFalse))
               store)]
       [VDict (htable)
              (ValueA 
               (if (member val1 (hash-keys htable))
                   (VTrue)
                   (VFalse))
               store)]
       [else (interp-error
              (string-append (pretty val2)
                             " not iterable")
              store)])]
    ['NotIn
     (type-case CVal val2
       [VList (mutable fields)
              (ValueA
               (if (member val1 fields)
                   (VFalse)
                   (VTrue))
               store)]
       [VDict (htable)
              (ValueA 
               (if (member val1 (hash-keys htable))
                   (VFalse)
                   (VTrue))
               store)]
       [else (interp-error
              (string-append (pretty val2)
                             " not iterable")
              store)])]
    
    ; COMPARISON PRIM
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
    ['GtE (type-case CVal val1
            [VInt (n) (let ([n1 n])
                        (type-case CVal val2
                          [VInt (n) (ValueA
                                     (if (>= (VInt-n val1) (VInt-n val2))
                                         (VTrue)
                                         (VFalse))
                                     store)]
                          [else (interp-error (string-append "Bad arguments for >=:\n"
                                                             (string-append (pretty val1)
                                                                            (string-append "\n"
                                                                                           (pretty val2))))
                                              store)]))]
            [else (type-case CVal val2
                    [VInt (n) (interp-error (string-append "Bad arguments for >=:\n" (string-append (pretty val1) (string-append "\n" (pretty val2))))
                                            store)]
                    [else (interp-error (string-append "Bad arguments for >=:\n" (string-append (pretty val1) (string-append "\n" (pretty val2))))
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
                                       store)])])]
    ['LtE (type-case CVal val1
            [VInt (n) (let ([n1 n])
                        (type-case CVal val2
                          [VInt (n) (ValueA (if (<= (VInt-n val1) (VInt-n val2))
                                                (VTrue)
                                                (VFalse))
                                            store)]
                          [else (interp-error
                                 (string-append "Bad arguments for <=:\n"
                                                (string-append (pretty val1)
                                                               (string-append "\n" (pretty val2))))
                                 store)]))]
            [else (type-case CVal val2
                    [VInt (n) (interp-error (string-append "Bad arguments for <=:\n" (string-append (pretty val1) (string-append "\n" (pretty val2))))
                                            store)]
                    [else (interp-error (string-append "Bad arguments for <=:\n" (string-append (pretty val1) (string-append "\n" (pretty val2))))
                                        store)])])]
    [else
     (begin
       (display "HANDLE PRIM2: \n")
       (display op)
       (ExceptionA (VUndefined) store))]))

;; interp-getfield : CExp CExp Env Store -> AnswerC
;; gets field values from objects
(define (interp-getfield (obj : CExp) (field : CExp) (env : Env) (store : Store)) : AnswerC
  (type-case AnswerC (interp-env obj env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ReturnA (value store) (ReturnA value store)]
    [ValueA (obj-val store)
            (type-case AnswerC (interp-env field env store)
              [ExceptionA (exn-val store) (ExceptionA exn-val store)]
              [ReturnA (value store) (ReturnA value store)]
              [ValueA (field-val store)
                      ; get the field
                      (ValueA
                       (type-case CVal obj-val
                         [VDict (htable)
                                (cond
                                  ;[(equal? (VStr "update") field-val) (dict-update-lambda)]
                                  [else (VUndefined)])]
                         [VClass (bases fields)
                                 (type-case (optionof CVal) (hash-ref fields field-val)
                                   [none () (VUndefined)]
                                   [some (exp) exp])]
                         [else (VUndefined)])
                       store)])]))

;; interp-app : ExprC (listof ExprC) Env Store -> AnswerC
;; interprets function applications, checking for exceptions
(define (interp-app [func : CExp] [args : (listof CExp)] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env func env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ReturnA (value store) (ReturnA value store)]
    [ValueA (value store)
            (type-case CVal value
              [VClosure (a d b e)
                        (interp-app-helper value a args d
                                           e env store)]
              [else (interp-error (string-append "Applied a non-function: " (pretty value)) store)])]))

;; interp-app-helper : ValueC (listof symbol) (listof ExprC) Env Store -> AnswerC
;; interprets a function application, by first recursively binding the arguments
(define (interp-app-helper [closure : CVal] [params : (listof symbol)] [args : (listof CExp)] [defaults : (listof CExp)]
                           [closureEnv : Env] [appEnv : Env] [store : Store]) : AnswerC
  (if (empty? args)
      (if (empty? params)
          ; apply!!
          (interp-env (VClosure-body closure) closureEnv store)
          ; out of app exprs, use default
          (if (empty? defaults)
              (interp-error "Application failed with arity mismatch" store)
              (local ([define newloc (new-loc)]
                      [define var-name (first params)]
                      [define rest-def (if (empty? defaults) empty (rest defaults))])
                (type-case AnswerC (interp-env (first defaults) appEnv store)
                  [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                  [ReturnA (value store) (ReturnA value store)]
                  [ValueA (value store)
                          (begin
                            (hash-set! store newloc value)
                            (interp-app-helper closure (rest params) empty rest-def
                                               (cons (binding var-name newloc) closureEnv)
                                               appEnv
                                               store))]))))
      ; add arg/symbol and check if there are more args
      (if (empty? params)
          (interp-error "Application failed with arity mismatch" store)
          (local ([define newloc (new-loc)]
                  [define var-name (first params)]
                  [define rest-def (if (empty? defaults) empty (rest defaults))])
            ; evaluate argument value
            (type-case AnswerC (interp-env (first args) appEnv store)
              [ExceptionA (exn-val store) (ExceptionA exn-val store)]
              [ReturnA (value store) (ReturnA value store)]
              [ValueA (value store)
                      (begin
                        (hash-set! store newloc value)
                        (interp-app-helper closure (rest params) (rest args) rest-def
                                           (cons (binding var-name newloc) closureEnv)
                                           appEnv
                                           store))])))))


;; print-object-exn : CVal (listof FieldV)
;; checks for a message field, and outputs the message if exists, value of the object if not
(define (print-object-exn (val : CVal) (fields : (listof FieldV)))
  (if (empty? fields)
      (error 'interp (pretty val))
      (type-case FieldV (first fields)
        [fieldV (name value)
                (if (equal? name "message")
                    (error 'interp (pretty value))
                    (print-object-exn val (rest fields)))])))

;; print-error : CVal Store
;; prints the exception output given the exn-val
(define (print-error (exn-val : CVal) (store : Store))
  (type-case CVal exn-val
    [VObject (fields) (print-object-exn exn-val fields)]
    [else (error 'interp (pretty exn-val))]))

