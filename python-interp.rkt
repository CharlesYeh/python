#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-builtin.rkt")

(require (typed-in racket (string-length : (string -> number))))

;; new-loc : -> Location
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
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
    [VClass (bases fields) #t]
    [VInstance (bases fields) #t]
    [VClosure (varargs arg defaults body env) #t]
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
      [CSetField (obj field value) (interp-setfield obj field value env store)]
      
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
      
      [CError (exn)
              (type-case AnswerC (interp-env exn env store)
                [ExceptionA (value store) (ExceptionA value store)]
                [ReturnA (value store) (ExceptionA value store)]
                [ValueA (value store) (ExceptionA value store)])]
      
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
      
      [CApp (func arges)
            ; catch ReturnA, and change to value
            (type-case AnswerC (interp-env func env store)
              [ExceptionA (exn-val store) (ExceptionA exn-val store)]
              [ReturnA (value store) (ReturnA value store)]
              [ValueA (func-value store)
                (type-case AnswerC (interp-app func-value arges env store)
                  [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                  [ReturnA (value store) (ValueA value store)]
                  ; functions return None by default
                  [ValueA (value store) (ValueA (VNone) store)])])]
      [CFunc (varargs args defaults body) (ValueA (VClosure varargs args defaults body env) store)]
      
      [CPrim1 (prim arg) (interp-prim1 prim arg env store)]
      [CPrim2 (prim left right) (interp-prim2 prim left right env store)]
      
      [CTry (body orelse excepts)
            (type-case AnswerC (interp-env body env store)
              [ValueA (value store) (interp-env orelse env store)]
              [ReturnA (value store) (ReturnA value store)]
              ; catch exception
              [ExceptionA (exn-val store) (interp-excepts excepts exn-val env store)])]
      [CTryFinally (body final)
                   (type-case AnswerC (interp-env body env store)
                     ; handle re-raises
                     [ExceptionA (exn-val store) (interp-try-clause final exn-val env store)]
                     [ReturnA (value store) (ReturnA value store)]
                     [ValueA (value store) (interp-env final env store)])]
      [CExcept (type body) (interp-env body env store)]
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

;; interp-try-clause : CExp CVal Env Store -> AnswerC
;; interprets an except or finally clause, handling re-raised exceptions
(define (interp-try-clause [body : CExp] [exn-val : CVal] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env body env store)
    [ExceptionA (val store)
     ; if is reraise, replace exception
     (type-case CVal val
       [VInstance (bases fields)
                  (if (equal? (VStr "No active exception")
                              (type-case (optionof CVal) (hash-ref fields (VStr "message"))
                                [some (v) v]
                                [none () (VStr "")]))
                      ; replace with last error
                      (ExceptionA exn-val store)
                      ; leave the re-raised error
                      (ExceptionA val store))]
       [else (ExceptionA val store)])]
    [ReturnA (value store) (ReturnA value store)]
    [ValueA (value store) (ValueA value store)]))

;; interp-except : (listof CExp) Env Store -> AnswerC
;; handles except cases after a try by matching the correct one
(define (interp-excepts [excepts : (listof CExp)] [exn-val : CVal] [env : Env] [store : Store]) : AnswerC
  (if (empty? excepts)
      ; no except caught it, so just return the exception
      (ExceptionA exn-val store)
      (local ([define exc (first excepts)]
              [define exc-type : CVal
                (type-case AnswerC
                  (interp-env (type-case CExp exc
                                [CExcept (type body) type]
                                [CNamedExcept (name type body) type]
                                [else (error 'interp "Except not actually except statement")])
                              env store)
                  [ExceptionA (value store) (VUndefined)]
                  [ReturnA (value store) (VUndefined)]
                  [ValueA (value store) value])])
        (cond
          [(or
            ; except clause for any exception
            (equal? exc-type (VNone))
            ; specific exception matched
            (and (VInstance? exn-val)
                 (VClass? exc-type)
                 (member (first (VInstance-bases exn-val))
                         (VClass-bases exc-type))))
           ; matched except!
           (type-case CExp exc
             [CExcept (t body) (interp-try-clause body exn-val env store)]
             [CNamedExcept (name t body)
                           ; bind exception var
                           (local ([define use-loc (new-loc)]
                                   [define use-env (cons (binding name use-loc) env)])
                             (begin
                               (hash-set! store use-loc exn-val)
                               (interp-try-clause body exn-val use-env store)))]
             [else (error 'interp "Except not actually except statement")])]
          ; no match, check next one
          [else (interp-excepts (rest excepts) exn-val env store)]))))

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

;; interp-dict-exp : (hashof CExp CExp) (hashof CVal CVal) (listof CExp) env store -> AnswerC
;; interprets a dictionary
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
            (case op
               ['to-list (type-case CVal value
                           [VStr (s) (ValueA (VList #t (map (lambda (s) (VStr (list->string (list s))))
                                                            (string->list s)))
                                             store)]
                           [VList (mutable fields) (ValueA (VList #t fields) store)]
                           [else (interp-error "to-list bad argument" store)])]
               [else
            (ValueA
             (case op
               ['builtin-dict-clear
                (type-case CVal value
                  [VDict (htable)
                         (begin
                           (map (lambda (key) (hash-remove! htable key))
                                (hash-keys htable))
                           (VNone))]
                  [else (error 'builtin "builtin dict function not on dict")])]
               ['builtin-all (type-case CVal value
                               [VList (mutable fields)
                                 (if (foldl (lambda (val bool)
                                              (and (get-truth-value val) bool))
                                            #f
                                            fields)
                                     (VTrue)
                                     (VFalse))]
                               [else (error 'interp "NOT ITERABLE")])]
               ['builtin-any (type-case CVal value
                               [VList (mutable fields)
                                 (if (foldl (lambda (val bool)
                                              (or (get-truth-value val) bool))
                                            #f
                                            fields)
                                     (VTrue)
                                     (VFalse))]
                               [else (error 'interp "NOT ITERABLE")])]

               ['to-print (begin
                            (display (pretty value))
                            value)]
               
               ['to-string
                (type-case CVal value
                  [VInstance (bases fields)
                             ; call the instance's __str__ function
                             (if (member (VStr "__str__") (hash-keys fields))
                                 (type-case AnswerC (interp-app (some-v (hash-ref fields (VStr "__str__")))
                                                                (CList #f empty)
                                                                env store)
                                   [ExceptionA (value store) value]
                                   [ReturnA (value store) (VStr (pretty value))]
                                   ; TODO: __str__ must return a string
                                   [ValueA (value store) value])
                                 (VStr (pretty value)))]
                  [else (VStr (pretty value))])]
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
                         [VClosure (varargs args body defaults env) (VStr "function")]
                         [VMethod (inst varargs args body defaults env) (VStr "function")]
                         [VTrue () (VStr "boolean")]
                         [VFalse () (VStr "boolean")]
                         [VUndefined () (VStr "undefined")]
                         [VNone () (VStr "none")]
                         [VList (mutable fields) (VStr "list")]
                         [VDict (htable) (VStr "hash")]
                         [VClass (bases fields) (VStr "class")]
                         [VInstance (bases fields) (VStr "instance")])]
               ;########### MOVE TO PRIM 2, and use lambda
               ['range (type-case CVal value
                         [VInt (n) (if (> 0 n)
                                       (VList #t empty)
                                       (VList #t (map (lambda (x) (VInt x))
                                                      (build-list n (lambda (x) x)))))]
                         [else (VUndefined)])]
               ; min
               ['min (type-case CVal value
                       [VStr (s)
                             (VStr (local ([define sl (string-to-chars s)])
                                     (foldl (lambda (c m)
                                              (if (< (atoi c) (atoi m)) c m))
                                            (first sl)
                                            sl)))]
                       [else (VUndefined)])]
               ['max (type-case CVal value
                       [VStr (s)
                             (VStr (local ([define sl (string-to-chars s)])
                                     (foldl (lambda (c m)
                                              (if (> (atoi c) (atoi m)) c m))
                                            (first sl)
                                            sl)))]
                       [else (VUndefined)])]
               ['len (type-case CVal value
                       [VStr (s) (VInt (string-length s))]
                       [VObject (fields) (VInt (length fields))]
                       [VList (mutable fields) (VInt (length fields))]
                       [VDict (htable) (VInt (length (hash-keys htable)))]
                       [else (VUndefined)])]
               ; numbers
               ['UAdd (type-case CVal value
                        [VInt (n) (VInt n)]
                        [VTrue () (VInt 1)]
                        [VFalse () (VInt 0)]
                        [else (VUndefined)])]
               ['USub (type-case CVal value
                        [VInt (n) (VInt (- 0 n))]
                        [VTrue () (VInt -1)]
                        [VFalse () (VInt 0)]
                        [else (VUndefined)])]
               ['Invert (type-case CVal value
                          [VInt (n) (VInt (- (- 0 n) 1))]
                          [VTrue () (VInt -2)]
                          [VFalse () (VInt -1)]
                          [else (VUndefined)])]
               
               ; logical
               ['Not (if (get-truth-value value)
                         (VFalse)
                         (VTrue))]
               [else (begin
                       (display "HANDLE PRIM1: \n")
                       (display op)
                       (display "\n-------------\n")
                       value)])
             store)])]))

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

;; interp-prim-is : CExp CExp Cval CVal env store -> AnswerC
;; interprets the built-in "is" operator
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
         [VNone () (equal? val1 val2)]
         [else #f]
         #;[else
            ; check addr of CExp
            (type-case CExp arg1
              [CId (x1)
                   (type-case CExp arg2
                     [CId (x2) (equal? (lookup x1 env) (lookup x2 env))]
                     [else #f])]
              [else #f])])
       (VTrue)
       (VFalse))
   store))

(define (interp-compare [op : (number number -> boolean)] [val1 : CVal] [val2 : CVal]
                        [env : Env] [store : Store]) : AnswerC
  (cond
    ; both values are numeric
    [(and (numeric? val1)
          (numeric? val2))
     (local ([define n1 (to-number val1)]
             [define n2 (to-number val2)])
       (ValueA (if (op n1 n2) (VTrue) (VFalse)) store))]
    ; both values are strings
    [(and (VStr? val1) (VStr? val2))
     (ValueA (if (op (compare-str (VStr-s val1) (VStr-s val2)) 0)
                 (VTrue)
                 (VFalse)) store)]
    [else (interp-throw-error 'TypeError
                              (list (CStr "unsupported operand type(s) for "))
                              env store)]))

(define (interp-throw-error [exc : symbol] [args : (listof CExp)] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env (CApp (CId exc) (CList #f args)) env store)
    [ReturnA (value store) (ExceptionA value store)]
    [ExceptionA (value store) (ExceptionA value store)]
    [ValueA (value store) (ExceptionA value store)]))

;; interp-prim2-helper : symbol ValueC ValueC Env Store -> AnswerC
;; interprets prim2 after exception checking is done in the main prim2c interpret function
(define (interp-prim2-helper [op : symbol] [val1 : CVal] [val2 : CVal] [env : Env] [store : Store]) : AnswerC
  (case op
    ; BOOLEAN PRIM
    ['Or
     (ValueA (if (or (get-truth-value val1)
                     (get-truth-value val2))
                 (VTrue)
                 (VFalse))
             store)]
    ['And
     (ValueA (if (and (get-truth-value val1)
                      (get-truth-value val2))
                 (VTrue)
                 (VFalse))
             store)]
    
    ; NUMBER PRIM
    ['FloorDiv
     (local ([define n1 (to-number val1)]
             [define n2 (to-number val2)])
       (if (= n2 0)
           (interp-throw-error 'ZeroDivisionError empty env store)
           (ValueA (VInt (floor (/ n1 n2))) store)))]
    ['Mod
     (type-case CVal val1
       [VInt (n) (let ([n1 n])
                   (type-case CVal val2
                     [VInt (n) (let ([n2 n])
                                 (if (= 0 n2)
                                     (interp-throw-error 'ZeroDivisionError empty env store)
                                     (ValueA (VInt (modulo n1 n2)) store)))]
                     [else (interp-error "Bad arguments for %" store)]))]
       [else (interp-error "Bad arguments for %" store)])]
    ['Div
     (local ([define n1 (to-number val1)]
             [define n2 (to-number val2)])
       (if (= n2 0)
           (interp-throw-error 'ZeroDivisionError empty env store)
           (ValueA (VInt (/ n1 n2)) store)))]
    ['Mult
     (cond
       ; numeric
       [(and (numeric? val1) (numeric? val2))
        (local ([define n1 (to-number val1)]
                [define n2 (to-number val2)])
          (ValueA (VInt (* n1 n2)) store))]
       [(or (and (VInt? val1) (VStr? val2))
            (and (VInt? val2) (VStr? val1)))
        (local ([define n (if (VInt? val1) (to-number val1) (to-number val2))]
                [define s (if (VStr? val1) (VStr-s val1) (VStr-s val2))])
          (ValueA (VStr (foldl (lambda (piece str)
                                 (string-append str piece))
                               ""
                               (build-list n (lambda (x) s))))
                  store))])]
    ['Add
     (cond
       ; numeric vals
       [(and (numeric? val1) (numeric? val2))
        (local ([define n1 (to-number val1)]
                [define n2 (to-number val2)])
          (ValueA (VInt (+ n1 n2)) store))]
       ; string concat
       [(and (VStr? val1) (VStr? val2))
        (ValueA (VStr (string-append (VStr-s val1) (VStr-s val2))) store)]
       [else (interp-error "TypeError" store)])]
    ['Sub
     (local ([define n1 (to-number val1)]
             [define n2 (to-number val2)])
       (ValueA (VInt (- n1 n2)) store))]
    
    ; LOGICAL PRIM
    ['Eq
     (local ([define (int-to-float val)
               (type-case CVal val
                 [VInt (n) (VFloat (+ 0.0 n))]
                 [else val])]
             [define eq-val1 (int-to-float val1)]
             [define eq-val2 (int-to-float val2)])
       (ValueA (if (equal? eq-val1 eq-val2) (VTrue) (VFalse)) store))]
    ['NotEq
     (local ([define (int-to-float val)
               (type-case CVal val
                 [VInt (n) (VFloat (+ 0.0 n))]
                 [else val])]
             [define eq-val1 (int-to-float val1)]
             [define eq-val2 (int-to-float val2)])
       (ValueA (if (equal? eq-val1 eq-val2) (VFalse) (VTrue)) store))]
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
       [VStr (s2)
             (type-case CVal val1
               [VStr (s1) (ValueA (if (string-in s1 s2) (VTrue) (VFalse)) store)]
               [else (interp-error "Must test string in string" store)])]
       [else (interp-error
              (string-append (pretty val2)
                             " not iterable")
              store)])]
    
    ; COMPARISON PRIM
    ['Gt (interp-compare > val1 val2 env store)]
    ['GtE (interp-compare >= val1 val2 env store)]
    ['Lt (interp-compare < val1 val2 env store)]
    ['LtE (interp-compare <= val1 val2 env store)]
    
    ['builtin-dict-get
     (type-case CVal val1
       [VDict (htable)
              (type-case (optionof CVal) (hash-ref htable val2)
                ; if not in hash table, return None
                [none () (ValueA (VNone) store)]
                [some (n) (ValueA n store)])]
       [else (interp-error "builtin dict function not on dict" store)])]

    ['builtin-filter
     (type-case CVal val1
       [VClosure (varargs args defaults body env)
                 (type-case CVal val2
                   [VList (mutable fields) (interp-builtin-filter val1 fields empty store)]
                   [else (interp-error "Filter iterable error" store)])]
       ; no filter function: return input
       [VNone () (type-case CVal val2
                   [VList (mutable fields) (interp-builtin-filter val1 fields empty store)]
                   [else (interp-error "Filter iterable error" store)])]
       [else (interp-error "Filter type error" store)])]
    
    ['isinstance
     (type-case CVal val1
       [VInstance (i-bases i-fields)
                  (type-case CVal val2
                    ; check if given class is in instance's supers
                    [VClass (bases fields) (ValueA (if (member (first bases) i-bases) (VTrue) (VFalse)) store)]
                    [else (interp-error "isinstance on nonclass" store)])]
       [else (ValueA (VFalse) store)])]
    
    [else
     (begin
       (display "HANDLE PRIM2: \n")
       (display op)
       (ExceptionA (VUndefined) store))]))

;; interp-builtin-filter : CVal (listof CVal) (listof CVal) store -> AnswerC
;; recursively filters each item
(define (interp-builtin-filter (closure : CVal) (iter : (listof CVal)) (result : (listof CVal)) (store : Store)) : AnswerC
  (if (empty? iter)
      (ValueA (VList #t (reverse result)) store)
      (type-case CVal closure
        [VClosure (v args d body env)
                  ; check function
                  (cond
                    ;##### CHANGE FILTER OBJECT TO GENERATOR..? AND PUT THIS BACK
                    ;[(not (= 1 (length args))) (interp-error "Filter function invalid" store)]
                    [(not (= 1 (length args))) (ValueA (VUndefined) store)]
                    [else 
                     ; bind var first
                     (local ([define newloc (new-loc)]
                             [define arg (first args)]
                             [define item (first iter)])
                       (begin
                         (hash-set! store newloc item)
                         ; check result for whether to filter out or not
                         (type-case AnswerC (interp-env body (cons (binding arg newloc) env) store)
                           [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                           [ValueA (value store) (interp-builtin-filter closure (rest iter) result store)]
                           [ReturnA (value store) (if (get-truth-value value)
                                                      (interp-builtin-filter closure (rest iter) (cons item result) store)
                                                      (interp-builtin-filter closure (rest iter) result store))
                                    ])))])]
        [VNone () (if (get-truth-value (first iter))
                      (interp-builtin-filter closure (rest iter) (cons (first iter) result) store)
                      (interp-builtin-filter closure (rest iter) result store))]
        [else (interp-error "Tried to apply non function" store)])))

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
                                  [(equal? (VStr "get") field-val) (dict-get-lambda obj-val env)]
                                  [(equal? (VStr "clear") field-val) (dict-clear-lambda obj-val env)]
                                  [else (VUndefined)])]
                         [VClass (bases fields)
                                 (type-case (optionof CVal) (hash-ref fields field-val)
                                   [none () (VUndefined)]
                                   [some (exp) exp])]
                         [VInstance (bases fields)
                                    (type-case (optionof CVal) (hash-ref fields field-val)
                                      [none () (VUndefined)]
                                      [some (exp) exp])]
                         [else (VUndefined)])
                       store)])]))

;; interp-setfield : CExp CExp CExp Env Store -> AnswerC
;; sets field values in objects
(define (interp-setfield (obj : CExp) (field : CExp) (value : CExp) (env : Env) (store : Store)) : AnswerC
  ; get the object
  (type-case AnswerC (interp-env obj env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ReturnA (value store) (ReturnA value store)]
    [ValueA (obj-val store)
            ; get the field
            (type-case AnswerC (interp-env field env store)
              [ExceptionA (exn-val store) (ExceptionA exn-val store)]
              [ReturnA (value store) (ReturnA value store)]
              [ValueA (field-val store)
                      ; get the value
                      (type-case AnswerC (interp-env value env store)
                        [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                        [ReturnA (value store) (ReturnA value store)]
                        [ValueA (value store)
                          (begin
                           (type-case CVal obj-val
                             [VClass (bases fields) (hash-set! fields field-val value)]
                             [VInstance (bases fields) (hash-set! fields field-val value)]
                             [else (void)])
                           (ValueA value store))])])]))

;; interp-class-to-instance : (listof string) (hashof CVal CVal) CExp Env Store -> AnswerC
;; creates an instance given a class, and calls the constructor if appropriate
(define (interp-class-to-instance [bases : (listof string)] [fields : (hashof CVal CVal)]
                                  [args : CExp] [env : Env] [store : Store]) : CVal
  (local ([define new-fields (make-hash empty)]
          [define new-instance (VInstance bases new-fields)])
    (begin
      (map (lambda (key)
             (local ([define value (some-v (hash-ref fields key))])
               (type-case CVal value
                 ; change VClosure to VMethod
                 [VClosure (varargs args defaults body env)
                           (hash-set! new-fields key (VMethod new-instance varargs args defaults body env))]
                 ; leave all other fields as-is
                 [else (hash-set! new-fields key value)])))
           (hash-keys fields))
      ; call constructor
      (if (member (VStr "__init__") (hash-keys new-fields))
          (interp-app (some-v (hash-ref new-fields (VStr "__init__"))) args env store)
          (ExceptionA (VUndefined) store))
      ; return instance
      new-instance)))

;; interp-app : CVal CExp Env Store -> AnswerC
;; interprets function applications, checking for exceptions
(define (interp-app [func : CVal] [args : CExp] [env : Env] [store : Store]) : AnswerC
  ; evaluate arguments
  (type-case AnswerC (interp-env args env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ReturnA (value store) (ReturnA value store)]
    [ValueA (value store)
            (type-case CVal value
              [VList (mutable fields)
                     (type-case CVal func
                       [VClosure (v a d b e)
                                 (interp-app-helper v func a fields d
                                                    e env store)]
                       ; add the first argument
                       [VMethod (inst v a d b e)
                                (local ([define newloc (new-loc)]
                                        [define newenv (cons (binding (first a) newloc) e)])
                                  (begin
                                    (hash-set! store newloc inst)
                                    (interp-app-helper v func (rest a) fields d
                                                       newenv env store)))]
                       ; #### CHANGE THIS
                       [VClass (bases fields) (ReturnA (interp-class-to-instance bases fields args env store) store)]
                       [else (interp-error (string-append "Applied a non-function: " (pretty func)) store)])]
              [else (error 'interp "Application arguments not a list")])]))

;; interp-app-helper : ValueC (listof symbol) (listof CVal) Env Store -> AnswerC
;; interprets a function application, by first recursively binding the arguments
(define (interp-app-helper [varargs : boolean] [closure : CVal]
                           [params : (listof symbol)] [args : (listof CVal)] [defaults : (listof CExp)]
                           [closureEnv : Env] [appEnv : Env] [store : Store]) : AnswerC
(begin
  (cond
    ; apply!!
    [(and (empty? params) (empty? args))
     (type-case CVal closure
       [VClosure (v a d body e) (interp-env body closureEnv store)]
       [VMethod (inst v a d body e) (interp-env body closureEnv store)]
       [else (interp-error "Tried to apply non function" store)])]
    
    ; no app args, symbols still, use defaults
    [(and (not (empty? params)) (empty? args) (not (empty? defaults)))
     (local ([define newloc (new-loc)]
             [define var-name (first params)]
             [define rest-def (if (empty? defaults) empty (rest defaults))])
       (type-case AnswerC (interp-env (first defaults) appEnv store)
         [ExceptionA (exn-val store) (ExceptionA exn-val store)]
         [ReturnA (value store) (ReturnA value store)]
         [ValueA (value store)
                 (begin
                   (hash-set! store newloc value)
                   (interp-app-helper varargs closure
                                      (rest params) empty rest-def
                                      (cons (binding var-name newloc) closureEnv)
                                      appEnv
                                      store))]))]
    
    ; combine app args left into last symbol (var args)
    [(and varargs (= 1 (length params)))
     (local ([define newloc (new-loc)]
             [define ans args])
       (type-case CVal closure
         [VClosure (v a d body e)
                   (begin
                     (hash-set! store newloc (VList #f args))
                     (interp-env body
                                 (cons (binding (first params) newloc) closureEnv)
                                 store))]
         [else (interp-error "Tried to apply non function" store)]))]
    
    ; keep applying
    [(and (not (empty? params)) (not (empty? args)))
     (local ([define newloc (new-loc)]
             [define arg-val (first args)]
             [define var-name (first params)]
             
             [define rest-def (if (empty? defaults) empty (rest defaults))])
       ; evaluate argument value
       (begin
         (hash-set! store newloc arg-val)
         (interp-app-helper varargs closure
                            (rest params) (rest args) rest-def
                            (cons (binding var-name newloc) closureEnv)
                            appEnv
                            store)))]
    
    [else (interp-error "Application failed with arity mismatch" store)]))

)

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

