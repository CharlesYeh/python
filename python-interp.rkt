#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-native.rkt")

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
      -1
      (type-case (optionof Location) (hash-ref (first env) name)
        [some (v) v]
        [none () (lookup name (rest env))])))

;; interp : CExp -> CVal
(define (interp [expr : CExp]) : CVal
  ; catch exception at top level
  (type-case AnswerC (interp-env expr (new-env) (new-store))
    [ExceptionA (exn-val store) (print-error exn-val store)]
    [ReturnA (value store) (print-error value store)]
    [ValueA (value store) value]))

;; interp-env : CExp Env Store -> CVal
;; interprets the given expression, with environment and store
(define (interp-env [expr : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case CExp expr
    [CSeq (e1 e2)
          (type-case AnswerC (interp-env e1 env store)
            [ExceptionA (exn-val store) (ExceptionA exn-val store)]
            [ReturnA (value store) (ReturnA value store)]
            [ValueA (value store) (interp-env e2 env store)])]
    
    [CClass (bases body) (interp-class bases body env store)]
    [CFunc (static varargs args defaults body)
           (if static
               (ValueA (VMethod "" (VNone) varargs args defaults body env) store)
               (ValueA (VClosure "" varargs args defaults body env) store))]
    [CApp (func arges)
          (type-case AnswerC (interp-env func env store)
            [ExceptionA (exn-val store) (ExceptionA exn-val store)]
            [ReturnA (value store) (ReturnA value store)]
            [ValueA (func-value store)
                    (type-case AnswerC (interp-app func-value arges env store)
                      [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                      ; catch ReturnA, and change to value
                      [ReturnA (value store) (ValueA value store)]
                      ; functions return None by default
                      [ValueA (value store) (ValueA (VNone) store)])])]
    [CReturn (value)
             (type-case AnswerC (interp-env value env store)
               [ExceptionA (exn-val store) (ExceptionA exn-val store)]
               [ReturnA (value store) (ExceptionA value store)]
               [ValueA (value store) (ReturnA value store)])]
    
    ; data types
    [CDict (has-values htable)
           (local ([define new-hash (make-hash empty)])
             (interp-dict-exp has-values
                              htable new-hash
                              (hash-keys htable)
                              env store))]
    [CList (mutable fields)
           (type-case ListFields (interp-list-exp fields env store)
             [LException (exn-val store) (ExceptionA exn-val store)]
             [LFields (fields store) (ValueA (VList mutable fields) store)])]
    [CInt (n) (ValueA (VInt n) store)]
    [CFloat (n) (ValueA (VInt n) store)]
    [CStr (s) (ValueA (VStr s) store)]
    [CTrue () (ValueA (VTrue) store)]
    [CFalse () (ValueA (VFalse) store)]
    [CNone () (ValueA (VNone) store)]
    
    ; this is used "unbound"
    [CUndefined () (ValueA (VUndefined) store)]
    
    [CGet (lhs) (interp-get lhs env store)]
    [CSet (lhs value) (interp-set lhs value env store)]
    [CDel (lhs) (interp-del lhs env store)]
    [CPrim1 (prim arg) (interp-prim1 prim arg env store)]
    [CPrim2 (prim left right) (interp-prim2 prim left right env store)]
    
    [CIf (i t e)
         (type-case AnswerC (interp-env i env store)
           [ExceptionA (exn-val store) (ExceptionA exn-val store)]
           [ReturnA (value store) (ReturnA value store)]
           [ValueA (value store)
                   (interp-env (if (get-truth-value value) t e) env store)])]
    
    ; scope
    [CLet (x bind body)
          ; get value of binding
          (type-case AnswerC (interp-env bind env store)
            [ExceptionA (exn-val store) (ExceptionA exn-val store)]
            [ReturnA (value store) (ReturnA value store)]
            [ValueA (value store)
                    ; insert binding
                    (local ([define use-loc (new-loc)]
                            [define use-env (env-bind #t env x use-loc)])
                      (begin
                        (hash-set! store use-loc value)
                        (interp-env body use-env store)))])]
    
    [CError (exn)
            (type-case AnswerC (interp-env exn env store)
              [ExceptionA (value store) (ExceptionA value store)]
              [ReturnA (value store) (ExceptionA value store)]
              [ValueA (value store) (ExceptionA value store)])]
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
    [CExcept (type body) (error 'interp "CExcept should be reached through CTry")]
    [CNamedExcept (name type body) (error 'interp "CExcept should be reached through CTry")]
    
    ; TODO: this should be handled in CGenerator right now
    [CIterator (id iter) (ValueA (VNone) store)]
    [CGenerator (value-gen iters)
                (interp-generator value-gen iters env store)]
    ;[CGenerator (expr) (ValueA (VGenerator expr env) store)]
    #|[CYield (value)
              (type-case AnswerC (interp-env value env store)
                [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                [ReturnA (value store) (ExceptionA value store)]
                [YieldImm (value store) (|#
    
    [CPass () (ValueA (VUndefined) store)]))

;; interp-class : (listof string) CExp Env Store -> AnswerC
;; interprets a class by interpreting the body and pulling out the fields
(define (interp-class [bases : (listof string)] [body : CExp] [env : Env] [store : Store]) : AnswerC
  (local ([define new-env (env-extend env)]
          [define classdefs (make-hash empty)]
          [define fields (make-hash empty)]
          [define var-lambda (lambda (var) (VStr (symbol->string var)))]
          [define loc-lambda (lambda (var) (lookup var new-env))]
          [define val-lambda (lambda (loc)
                               (local ([define val (some-v (hash-ref store loc))])
                                 (type-case CVal val
                                   [VClosure (bs v a d b e) 
                                             ; remove the extra scope added when interpreting the class def
                                             (if (equal? (rest e) env)
                                                 (VClosure (first bases) v a d b (rest e))
                                                 (VClosure bs v a d b e))]
                                   [VMethod (bs i v a d b e) 
                                            (if (equal? (rest e) env)
                                                (VMethod (first bases) i v a d b (rest e))
                                                (VMethod bs i v a d b e))]
                                   [else val])))]
          ; mutate super class def into "classdefs", then recurse for super's super defs
          [define set-def
            (lambda (base)
              (local ([define def (val-lambda (loc-lambda (string->symbol base)))])
                (begin
                  ; get def
                  (hash-set! classdefs base def)
                  ; recurse into def for more defs
                  (type-case CVal def
                    [VClass (bases supers fields) (map set-def (rest bases))]
                    [else (error 'interp "Super isn't class")]))))])
    (begin
      ; create scope for class, then pull out assignments
      (interp-env body new-env store)
      (map (lambda (str)
             (hash-set! fields (var-lambda str)
                        (val-lambda (loc-lambda str))))
           (hash-keys (first new-env)))
      ; get class defs
      (map set-def (rest bases))
      ; set native var __dict__
      (hash-set! fields (VStr "__dict__") (VDict #t fields))
      ; get method resolution order
      (local ([define mro (cons (first bases)
                                (reverse (interp-mro (VClass bases classdefs fields) empty)))]
              [define class-val (VClass mro classdefs fields)])
        (begin
          ; replace inst in VMethods in f since they're class methods
          (map (lambda (key)
                 (type-case CVal (some-v (hash-ref fields key))
                   [VMethod (bs i v a d b e)
                            (hash-set! fields key (VMethod bs class-val v a d b e))]
                   [else (void)]))
               (hash-keys fields))
          
          ; add current class
          (hash-set! classdefs (first bases) class-val)
          (ValueA class-val store))))))

;; interp-mro : CVal (listof string) -> (listof string)
;; determines the method resolution order of a new class definition
(define (interp-mro [classdef : CVal] [mro : (listof string)]) : (listof string)
  (type-case CVal classdef
    [VClass (bases classdefs body)
            (foldr (lambda (b res)
                     (if (or (member b res) (member b mro))
                         res
                         (append
                          (append
                           res (interp-mro (some-v (hash-ref classdefs b)) res))
                          (list b))))
                   empty
                   (rest bases))]
    [else (error 'interp "No mro for non-classes")]))

;; interp-generator : CExp (listof CExp) Env Store -> AnswerC
;; turns a generator expression into a list
(define (interp-generator [value-gen : CExp] [iters : (listof CExp)]
                          [env : Env] [store : Store]) : AnswerC
  (local ([define iter-fields 
                  ; get list of (list of iter values)
                  (map (lambda (iter)
                         (type-case CExp iter
                           [CIterator (id expr)
                                      ; TODO: test for exceptions
                                      (VList-fields
                                       (ValueA-value
                                        (interp-to-list (ValueA-value (interp-env expr env store))
                                                        env store)))]
                           [else (error 'interp "Generator can't take non-iterator")]))
                       iters)]
          [define iter-vars
                  (map (lambda (iter)
                         (type-case CExp iter
                           ; TODO: test for exceptions
                           [CIterator (id expr) id]
                           [else (error 'interp "Generator can't take non-iterator")]))
                       iters)]
          [define new-env (env-extend env)])
    ; TODO: supposed to return a generator, not a list
    (ValueA (VList #f
                   ; get each new value in list (by interpreting value-gen with iter bindings)
                   (map (lambda (index)
                          (begin
                            ; map the iter value bindings in
                            (map2 (lambda (var fields)
                                    (local ([define nloc (new-loc)])
                                      (begin
                                        (env-bind #t env var nloc)
                                        (hash-set! store nloc (list-ref fields index)))))
                                  iter-vars iter-fields)
                            ; now get value at this index
                            (ValueA-value (interp-env value-gen new-env store))))
                        (build-list (length (first iter-fields)) (lambda (x) x))))
            store)))

;; interp-try-clause : CExp CVal Env Store -> AnswerC
;; interprets an except or finally clause, handling re-raised exceptions
(define (interp-try-clause [body : CExp] [exn-val : CVal] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env body env store)
    [ExceptionA (val store)
     ; if is reraise, replace exception
     (type-case CVal val
       [VInstance (bases classdefs fields)
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
                 (cond
                   [(VClass? exc-type)
                    (member (first (VInstance-bases exn-val))
                            (VClass-bases exc-type))]
                   ; match list of expressions
                   [(VList? exc-type)
                    (foldl (lambda (type res)
                             (or res (member (first (VInstance-bases exn-val))
                                             (VClass-bases type))))
                           #f
                           (VList-fields exc-type))])))
           ; matched except!
           (type-case CExp exc
             [CExcept (t body) (interp-try-clause body exn-val env store)]
             [CNamedExcept (name t body)
                           ; bind exception var
                           (local ([define use-loc (new-loc)]
                                   [define use-env (env-bind #t env name use-loc)])
                             (begin
                               (hash-set! store use-loc exn-val)
                               (interp-try-clause body exn-val use-env store)))]
             [else (error 'interp "Except not actually except statement")])]
          ; no match, check next one
          [else (interp-excepts (rest excepts) exn-val env store)]))))

;; ListFields
;; used to allow the object field interpret to return an exception
(define-type ListFields
  [LFields (fields : (listof CVal)) (store : Store)]
  [LException (exn-val : CVal) (store : Store)])

;; interp-list-exp : (listof CExp) Env Store -> ListFields
;; interprets a list, stopping if there's an error
(define (interp-list-exp [exprs : (listof CExp)] [env : Env] [store : Store]) : ListFields
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
         [ExceptionA (exn-val store) (LException exn-val store)]
         [ReturnA (value store) (list-lambda value store)]
         [ValueA (value store) (list-lambda value store)]))]))

;; interp-dict-exp : (hashof CExp CExp) (hashof CVal CVal) (listof CExp) env store -> AnswerC
;; interprets a dictionary, stopping if there's an error
(define (interp-dict-exp [has-values : boolean]
                         [old-hash : (hashof CExp CExp)] [new-hash : (hashof CVal CVal)]
                         [exprs : (listof CExp)] [env : Env] [store : Store]) : AnswerC
  (cond
    [(= 0 (length exprs)) (ValueA (VDict has-values new-hash) store)]
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
                               (interp-dict-exp has-values
                                                old-hash new-hash
                                                (rest exprs) env store))]))]))]))

;; interp-to-list : CVal Env Store -> AnswerC
;; converts a core value into a VList
(define (interp-to-list [value : CVal] [env : Env] [store : Store]) : AnswerC
   (type-case CVal value
     [VStr (s) (ValueA (VList #t (map (lambda (s) (VStr (list->string (list s))))
                                      (string->list s)))
                                 store)]
     [VList (mutable fields) (ValueA (VList #t fields) store)]
     [VDict (has-values htable) (ValueA (VList #t (hash-keys htable)) store)]
#|   [VGenerator (expr env)
                 (type-case ListFields (interp-generator-list expr env store)
                   [LFields (fields store)
                            (ValueA (VList #t (interp-generator-list expr env store)) store)]
                   [LException (exn-val store) (ExceptionA exn-val store)])]|#
     [else (interp-throw-error 'TypeError empty env store)]))

;; interp-to-set : CVal Env Store -> AnsewrC
;; converts a core value into a VDict #f (set)
(define (interp-to-set [value : CVal] [env : Env] [store : Store]) : AnswerC
   (type-case CVal value
     [VList (mutable fields)
            (local ([define keys (make-hash empty)])
              (begin
                (map (lambda (v) (hash-set! keys v (VNone)))
                     fields)
                (ValueA (VDict #f keys) store)))]
     [VDict (has-values htable)
            (local ([define keys (make-hash empty)])
              (begin
                (map (lambda (v) (hash-set! keys v (VNone)))
                     (hash-keys htable))
                (ValueA (VDict #f keys) store)))]
     [else (interp-throw-error 'TypeError empty env store)]))

;; interp-prim1 : symbol ExprC Env Store -> Result
;; interprets a single argument primitive operation
(define (interp-prim1 [op : symbol] [arg : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ExceptionA (exn-val store) (ExceptionA exn-val store)]
    [ReturnA (value store) (ReturnA value store)]
    ; evaluate
    [ValueA (value store)
            (case op
              ['to-list (interp-to-list value env store)]
              ['to-set (interp-to-set value env store)]
              ['builtin-all
               (type-case CVal value
                 [VList (mutable fields)
                        (ValueA (if (foldl (lambda (val bool) (and (get-truth-value val) bool))
                                           #t
                                           fields)
                                    (VTrue) (VFalse))
                                store)]
                 [else (interp-throw-error 'TypeError empty env store)])]
              ['builtin-any
               (type-case CVal value
                 [VList (mutable fields)
                        (ValueA (if (foldl (lambda (val bool) (or (get-truth-value val) bool))
                                           #f
                                           fields)
                                    (VTrue) (VFalse))
                                store)]
                 [else (interp-throw-error 'TypeError empty env store)])]
               ['builtin-dict-keys
                (type-case CVal value
                  [VDict (has-values htable)
                         (interp-to-set (VList #f (hash-keys htable)) env store)]
                  [else (error 'builtin "builtin dict function not on dict")])]
               ['builtin-dict-items
                (type-case CVal value
                  [VDict (has-values htable)
                         (interp-to-set
                           (VList #f (map (lambda (key)
                                            (VList #f (list key (some-v (hash-ref htable key)))))
                                          (hash-keys htable)))
                           env store)]
                  [else (error 'builtin "builtin dict function not on dict")])]
              [else
               (ValueA
                (case op
                  ['builtin-dict-values
                   (type-case CVal value
                     [VDict (has-values htable)
                            (VList #f (map (lambda (key) (some-v (hash-ref htable key)))
                                           (hash-keys htable)))]
                     [else (error 'builtin "builtin dict function not on dict")])]
                  ['builtin-dict-clear
                   (type-case CVal value
                     [VDict (has-values htable)
                            (begin
                              (map (lambda (key) (hash-remove! htable key))
                                   (hash-keys htable))
                              (VNone))]
                     [else (error 'builtin "builtin dict function not on dict")])]
                  ['builtin-locals
                   (local ([define local-vars (make-hash empty)])
                     (begin
                       (map (lambda (v)
                              (local ([define loc (lookup v env)]
                                      [define var (VStr (symbol->string v))]
                                      [define val (some-v (hash-ref store loc))])
                                (if (not (equal? (VUndefined) val))
                                    (hash-set! local-vars var val)
                                    (void))))
                            ; append lookuped vars
                            (hash-keys (first env)))
                       (VDict #t local-vars)))]
                  
                  ['to-print (begin
                               (display (string-append (pretty value) "\n"))
                               value)]
                  
                  ['to-string
                   (type-case CVal value
                     [VInstance (bases classdefs fields)
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
                            [VInt (n) (VStr "int")]
                            [VFloat (n) (VStr "float")]
                            [VClosure (base varargs args body defaults env) (VStr "function")]
                            [VMethod (base inst varargs args body defaults env) (VStr "function")]
                            [VTrue () (VStr "boolean")]
                            [VFalse () (VStr "boolean")]
                            [VUndefined () (VStr "undefined")]
                            [VNone () (VStr "none")]
                            [VList (mutable fields) (VStr "list")]
                            [VDict (has-values htable) (VStr "hash")]
                            [VClass (bases classdefs fields) (VStr "class")]
                            [VInstance (bases classdefs fields) (VStr "instance")]
                            [VGenerator (expr env) (VStr "generator")])]
                  
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
                          [VList (mutable fields) (VInt (length fields))]
                          [VDict (has-values htable) (VInt (length (hash-keys htable)))]
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
                  [else (error 'interp "Unhandled prim1 operator")])
                store)])]))

;; interp-prim2 : symbol CExp CExp Env Store -> Result
;; interprets a two argument primitive operation
(define (interp-prim2 [op : symbol] [arg1 : CExp] [arg2 : CExp] [env : Env] [store : Store]) : AnswerC
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
                             (interp-prim2-helper op arg1 arg2 val1 val2 env store)]))]))]))

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
         ;[VList (mutable fields) (and (not mutable) (and (< 0 (length fields)) (equal? val1 val2)))]
         [VNone () (equal? val1 val2)]
         [else
          ; check addr of CExp
          (type-case CExp arg1
            [CGet (lhs)
                  (type-case CLHS lhs
                    [CIdLHS (x1)
                            (type-case CExp arg2
                              [CGet (lhs)
                                    (type-case CLHS lhs
                                      [CIdLHS (x2) (equal? (lookup x1 env) (lookup x2 env))]
                                      [else #f])]
                              [else #f])]
                    [else #f])]
            [else #f])])
       (VTrue) (VFalse))
   store))

;; interp-compare : (number number -> boolean) CVal CVal Env Store -> AnswerC
;; interprets a comparison operation
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

;; interp-throw-error : symbol (listof CExp) Env Store -> AnswerC
;; gets an instance of the "exc" exception/error
(define (interp-throw-error [exc : symbol] [args : (listof CExp)] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env (CApp (CGet (CIdLHS exc)) (CList #f args)) env store)
    [ReturnA (value store) (ExceptionA value store)]
    [ExceptionA (value store) (ExceptionA value store)]
    [ValueA (value store) (ExceptionA value store)]))

;; interp-prim2-helper : symbol ValueC ValueC Env Store -> AnswerC
;; interprets prim2 after exception checking is done in the main prim2c interpret function
(define (interp-prim2-helper [op : symbol] [exp1 : CExp] [exp2 : CExp] [val1 : CVal] [val2 : CVal] [env : Env] [store : Store]) : AnswerC
  (case op
    ; BOOLEAN PRIM
    ['BitXor
     (cond
       [(and (VDict? val1) (not (VDict-has-values val1))
             (VDict? val2) (not (VDict-has-values val2)))
        (local ([define v1 (VDict-htable val1)]
                [define v2 (VDict-htable val2)]
                [define res-table (make-hash empty)])
          (begin
            (map (lambda (v) (hash-set! res-table v (VNone)))
                 (hash-keys v1))
            (map (lambda (v)
                   (type-case (optionof CVal) (hash-ref res-table v)
                     [some (n) (hash-remove! res-table v)]
                     [none () (hash-set! res-table v (VNone))]))
                 (hash-keys v2))
            (ValueA (VDict #f res-table) store)))]
       [else (interp-throw-error 'TypeError empty env store)])]
    ['BitOr
     (cond
       [(and (VDict? val1) (not (VDict-has-values val1))
             (VDict? val2) (not (VDict-has-values val2)))
        (local ([define v1 (VDict-htable val1)]
                [define v2 (VDict-htable val2)]
                [define res-table (make-hash empty)])
          (begin
            (map (lambda (v) (hash-set! res-table v (VNone)))
                 (hash-keys v1))
            (map (lambda (v) (hash-set! res-table v (VNone)))
                 (hash-keys v2))
            (ValueA (VDict #f res-table) store)))]
       [else (interp-throw-error 'TypeError empty env store)])]
    ['BitAnd
     (cond
       [(and (VDict? val1) (not (VDict-has-values val1))
             (VDict? val2) (not (VDict-has-values val2)))
        (local ([define v1 (VDict-htable val1)]
                [define v2 (VDict-htable val2)]
                [define res-table (make-hash empty)])
          (begin
            (map (lambda (v)
                   (type-case (optionof CVal) (hash-ref v2 v)
                     [some (n) (hash-set! res-table v (VNone))]
                     [none () (void)]))
                 (hash-keys v1))
            (ValueA (VDict #f res-table) store)))]
       [else (interp-throw-error 'TypeError empty env store)])]
    ['Or
     (ValueA (if (or (get-truth-value val1) (get-truth-value val2))
                 (VTrue) (VFalse))
             store)]
    ['And
     (ValueA (if (and (get-truth-value val1) (get-truth-value val2))
                 (VTrue) (VFalse))
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
       [VInt (n1)
             (type-case CVal val2
               [VInt (n2)
                     (if (= 0 n2)
                         (interp-throw-error 'ZeroDivisionError empty env store)
                         (ValueA (VInt (modulo n1 n2)) store))]
               [else (interp-throw-error 'TypeError empty env store)])]
       [else (interp-throw-error 'TypeError empty env store)])]
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
          (ValueA (VStr (foldl (lambda (piece str) (string-append str piece))
                               ""
                               (build-list n (lambda (x) s))))
                  store))]
       ; multiply a list
       [(or (and (VList? val2) (VInt? val1))
            (and (VList? val1) (VInt? val2)))
        (local ([define first-num (VInt? val1)]
                [define n (if first-num (to-number val1) (to-number val2))]
                [define li-val (if first-num val2 val1)]
                [define li-fields (VList-fields li-val)]
                [define li-mutable (VList-mutable li-val)])
          (ValueA (VList li-mutable
                         (foldl (lambda (piece res) (append piece res))
                                empty
                                (build-list n (lambda (x) li-fields))))
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
       [(and (VList? val1) (VList? val1))
        (cond
          [(equal? (VList-mutable val1) (VList-mutable val2))
           (ValueA (VList (VList-mutable val1)
                          (append (VList-fields val1) (VList-fields val2)))
                   store)]
          [else (interp-throw-error 'TypeError empty env store)])]
       [else (interp-throw-error 'TypeError empty env store)])]
    ['Sub
     (cond
       ; subtract 2 sets
       [(and (VDict? val1) (not (VDict-has-values val1))
             (VDict? val2) (not (VDict-has-values val2)))
        (local ([define v1 (VDict-htable val1)]
                [define v2 (VDict-htable val2)]
                [define res-table (make-hash empty)])
          (begin
            ; copy values over, then remove them
            (map (lambda (v) (hash-set! res-table v (VNone)))
                 (hash-keys v1))
            (map (lambda (v)
                   (type-case (optionof CVal) (hash-ref res-table v)
                     [some (n) (hash-remove! res-table v)]
                     [none () (void)]))
                 (hash-keys v2))
            (ValueA (VDict #f res-table) store)))]
       [(and (numeric? val1) (numeric? val2))
        (local ([define n1 (to-number val1)]
                [define n2 (to-number val2)])
          (ValueA (VInt (- n1 n2)) store))]
       [else (ValueA (VNone) store)])]
    
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
                   (VTrue) (VFalse))
               store)]
       [VDict (has-values htable)
              (ValueA 
               (if (member val1 (hash-keys htable))
                   (VTrue) (VFalse))
               store)]
       [VStr (s2)
             (type-case CVal val1
               [VStr (s1) (ValueA (if (string-in s1 s2) (VTrue) (VFalse)) store)]
               [else (interp-throw-error 'TypeError empty env store)])]
       ; not iterable
       [else (interp-throw-error 'TypeError empty env store)])]
    
    ; COMPARISON PRIM
    ['Gt (interp-compare > val1 val2 env store)]
    ['GtE (interp-compare >= val1 val2 env store)]
    ['Lt (interp-compare < val1 val2 env store)]
    ['LtE (interp-compare <= val1 val2 env store)]
    
    ; 
    ['builtin-super
     (type-case CVal val1
       [VStr (base)
             (type-case CVal val2
               [VInstance (i-bases i-classdefs i-fields)
                          (local ([define found (box #f)]
                                  [define rbases
                                    (rest (foldl (lambda (b res)
                                                   (begin
                                                     (if (equal? b base)
                                                         (set-box! found #t)
                                                         (void))
                                                     (if (unbox found) (append res (list b)) res)))
                                                 empty
                                                 i-bases))])
                              (ValueA (VInstance rbases i-classdefs i-fields) store))]
               [else (error 'interp "super() with non-instance")])]
       [else (error 'interp "super() with non-string")])]
    ['builtin-dict-update
     (type-case CVal val1
       [VDict (has-values1 htable1)
              (type-case CVal val2
                [VDict (has-values2 htable2)
                       (begin
                         (map (lambda (key) (hash-set! htable1 key (some-v (hash-ref htable2 key))))
                              (hash-keys htable2))
                         (ValueA val1 store))]
                [else (interp-throw-error 'TypeError empty env store)])]
       [else (error 'interp "builtin dict function not on dict")])]
    
    ['builtin-dict-get
     (type-case CVal val1
       [VDict (has-values htable)
              (type-case (optionof CVal) (hash-ref htable val2)
                ; if not in hash table, return None
                [none () (ValueA (VNone) store)]
                [some (n) (ValueA n store)])]
       [else (error 'interp "builtin dict function not on dict")])]

    ['builtin-filter
     (type-case CVal val1
       [VClosure (base varargs args defaults body env)
                 (type-case CVal val2
                   [VList (mutable fields) (interp-builtin-filter val1 fields empty env store)]
                   [else (interp-throw-error 'TypeError empty env store)])]
       ; no filter function: return input
       [VNone () (type-case CVal val2
                   [VList (mutable fields) (interp-builtin-filter val1 fields empty env store)]
                   [else (interp-throw-error 'TypeError empty env store)])]
       [else (interp-throw-error 'TypeError empty env store)])]
    
    ['isinstance
     (local ([define check-lambda
                     (lambda (i-bases bases)
                       (ValueA (if (member (first bases) i-bases) (VTrue) (VFalse))
                               store))]
             [define check-error (interp-throw-error 'TypeError empty env store)])
       (type-case CVal val2
         [VClass (bases classdefs fields)
                 (type-case CVal val1
                   [VInstance (i-bases i-classdefs i-fields)
                              ; check if given class is in instance's supers
                              (check-lambda i-bases bases)]
                   
                   ; TODO: unfortunately special casing these
                   [VTrue () (check-lambda (list "bool" "int") bases)]
                   [VFalse () (check-lambda (list "bool" "int") bases)]
                   [VInt (n) (check-lambda (list "int") bases)]
                   [else (ValueA (VFalse) store)])]
         [else check-error]))]
    
    [else (error 'interp "Unhandled prim2 operator")]))

;; interp-builtin-filter : CVal (listof CVal) (listof CVal) Env Store -> AnswerC
;; recursively filters each item
(define (interp-builtin-filter [closure : CVal] [iter : (listof CVal)]
                               [result : (listof CVal)] [env : Env] [store : Store]) : AnswerC
  (if (empty? iter)
      (ValueA (VList #t (reverse result)) store)
      (type-case CVal closure
        [VClosure (base v args d body env)
                  ; check function
                  (cond
                    [(not (= 1 (length args))) (ValueA (VUndefined) store)]
                    [else 
                     ; bind var first
                     (local ([define newloc (new-loc)]
                             [define arg (first args)]
                             [define item (first iter)])
                       (begin
                         (hash-set! store newloc item)
                         ; TODO: this should use interp-app
                         ; check result for whether to filter out or not
                         (type-case AnswerC (interp-env body (env-bind #t env arg newloc) store)
                           [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                           [ValueA (value store) (interp-builtin-filter closure (rest iter) result env store)]
                           [ReturnA (value store) (if (get-truth-value value)
                                                      (interp-builtin-filter closure (rest iter) (cons item result) env store)
                                                      (interp-builtin-filter closure (rest iter) result env store))
                                    ])))])]
        [VNone () (if (get-truth-value (first iter))
                      (interp-builtin-filter closure (rest iter) (cons (first iter) result) env store)
                      (interp-builtin-filter closure (rest iter) result env store))]
        ; tried to apply non-function
        [else (interp-throw-error 'TypeError empty env store)])))

;; interp-del : CLHS Env Store -> AnswerC
;; interprets the delete operator
(define (interp-del [lhs : CLHS] [env : Env] [store : Store]) : AnswerC
(begin
  (type-case CLHS lhs
    [CDotLHS (obj field)
             (type-case AnswerC (interp-env obj env store)
               [ExceptionA (exn-val store) (ExceptionA exn-val store)]
               [ReturnA (value store) (ReturnA value store)]
               [ValueA (obj-val store)
                       (type-case AnswerC (interp-env field env store)
                         [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                         [ReturnA (value store) (ReturnA value store)]
                         [ValueA (field-val store)
                                 ; get the field
                                 (type-case CVal obj-val
                                   [VDict (has-values htable)
                                          (begin (hash-remove! htable field-val)
                                                 (ValueA (VNone) store))]
                                   [else (error 'interp "HANDLE THIS DELETE CASE")])])])]
    [CListLHS (li) (interp-lhs-list li interp-del env store)]
    [CIdLHS (x) (interp-set-val lhs (VUndefined) env store)])))
               
;; interp-get : CLHS Env Store -> AnswerC
;; gets the LHS expression from the env and store
(define (interp-get [lhs : CLHS] [env : Env] [store : Store]) : AnswerC
  (begin
  (type-case CLHS lhs
    [CIdLHS (x)
            (local ([define loc (lookup x env)])
              (if (= -1 loc)
                  (interp-throw-error 'UnboundLocalError empty env store)
                  (type-case (optionof CVal) (hash-ref store loc)
                    [some (v) (type-case CVal v
                                [VUndefined ()
                                            (type-case (optionof Location) (hash-ref (first env) x)
                                              ; if UNDEFINED in current scope, throw UnboundLocalError
                                              [some (n) (interp-throw-error 'UnboundLocalError empty env store)]
                                              ; if in parent scope, throw NameError
                                              [none () (interp-throw-error 'NameError empty env store)])]
                                [else (ValueA v store)])]
                    [none () (interp-throw-error 'UnboundLocalError empty env store)])))]
    [CDotLHS (obj field)
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
                                    [VDict (has-values htable)
                                           (cond
                                             ; TODO: make dict it's own class
                                             [(equal? (VStr "update") field-val) (dict-update-lambda obj-val env)]
                                             [(equal? (VStr "get") field-val) (dict-get-lambda obj-val env)]
                                             [(equal? (VStr "keys") field-val) (dict-keys-lambda obj-val env)]
                                             [(equal? (VStr "clear") field-val) (dict-clear-lambda obj-val env)]
                                             [(equal? (VStr "values") field-val) (dict-values-lambda obj-val env)]
                                             [(equal? (VStr "items") field-val) (dict-items-lambda obj-val env)]
                                             [else (VUndefined)])]
                                    [VClass (bases classdefs fields)
                                            (type-case (optionof CVal) (hash-ref fields field-val)
                                              ; look in a super class
                                              [none () (interp-instance-method obj-val bases field-val store)]
                                              [some (exp) exp])]
                                    [VInstance (bases classdefs fields)
                                               (cond
                                                 [(equal? (VStr "__class__") field-val) (some-v (hash-ref classdefs (first bases)))]
                                                 [else
                                                  (type-case (optionof CVal) (hash-ref fields field-val)
                                                    ; if method, then get from class def instead
                                                    [some (n) (if (or (VClosure? n) (VMethod? n))
                                                                  (interp-instance-method obj-val bases field-val store)
                                                                  n)]
                                                    [none () (interp-instance-method obj-val bases field-val store)])])]
                                    [else (VUndefined)])
                                  store)])])]
    ; convert to list
    [CListLHS (li) (interp-lhs-list li interp-get env store)]))
  )

(define (interp-instance-method [inst : CVal] [bases : (listof string)] [field-val : CVal] [store : Store]) : CVal
  (if (empty? bases)
      ; TODO: throw attribute error
      (error 'interp "EMPTY")
      (type-case CVal inst
        [VInstance (i-bases classdefs fields)
                   ; get desired class def, isn't necessarily sub class
                   (local ([define c-def (some-v (hash-ref classdefs (first bases)))]
                           [define c-fields (VClass-fields c-def)])
                     (type-case (optionof CVal) (hash-ref c-fields field-val)
                       [some (n) (type-case CVal n
                                   [VClosure (bs v a d b e) (VMethod bs inst v a d b e)]
                                   [else n])]
                       [none () (interp-instance-method inst (rest bases) field-val store)]))]
        [VClass (i-bases classdefs fields)
                   ; get desired class def, isn't necessarily sub class
                   (local ([define c-def (some-v (hash-ref classdefs (first bases)))]
                           [define c-fields (VClass-fields c-def)])
                     (type-case (optionof CVal) (hash-ref c-fields field-val)
                       [some (n) (type-case CVal n
                                   [VClosure (bs v a d b e) (VClosure bs v a d b e)]
                                   [else n])]
                       [none () (interp-instance-method inst (rest bases) field-val store)]))]
        [else (error 'interp "inst isn't instance")])))

;; interp-lhs-list : (listof CLHS) (CLHS Env Store -> AnswerC) Env Store -> AnswerC
;; interprets a list of CLHS expressions
(define (interp-lhs-list [li : (listof CLHS)] [interp-func : (CLHS Env Store -> AnswerC)]
                        [env : Env] [store : Store]) : AnswerC
  (foldl (lambda (item result)
           ; if already an exception, just keep it
           (if (ExceptionA? result)
               result
               ; else combine into list, and convert back to AnswerC
               (local ([define res-list (VList-fields (ValueA-value result))]
                       [define new-item (ValueA-value item)])
                 (ValueA (VList #f (cons new-item res-list)) store))))
         (ValueA (VList #f empty) store)
         (map (lambda (lhs) (interp-func lhs env store)) li)))

;; interp-set : CLHS CExp Env Store -> AnswerC
;; sets field values in objects
(define (interp-set [lhs : CLHS] [value : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env value env store)
    [ExceptionA (value store) (ExceptionA value store)]
    [ReturnA (value store) (ReturnA value store)]
    [ValueA (value store) (interp-set-val lhs value env store)]))

(define (interp-set-val [lhs : CLHS] [i-val : CVal] [env : Env] [store : Store]) : AnswerC
  (type-case CLHS lhs
    [CIdLHS (id)
            (local ([define loc (lookup id env)])
              (if (= -1 loc)
                  (interp-throw-error 'NameError empty env store)
                  (local ([define use-loc (new-loc)])
                    (begin
                      (env-bind #f env id use-loc)
                      (hash-set! store use-loc i-val)
                      (ValueA i-val store)))))]
#|                            ; if setting equal to another id, copy addr instead
                            (cond
                              [(and (or (and (CGet? value) (CIdLHS? (CGet-lhs value)))
                                        (and (CSet? value) (CIdLHS? (CSet-lhs value))))
                                    (reference-var? i-val))
                               (begin
                                 (env-bind #f env id
                                           (lookup
                                            (type-case CExp value
                                              [CGet (lhs) (CIdLHS-id lhs)]
                                              [CSet (lhs value) (CIdLHS-id lhs)]
                                              [else (error 'interp "Invalid variable")])
                                            env))
                                 (ValueA i-val store))] |#
    [CDotLHS (obj field)
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
                                 ; set the value
                                 (begin
                                   (type-case CVal obj-val
                                     [VClass (bases classdefs fields) (hash-set! fields field-val i-val)]
                                     [VInstance (bases classdefs fields) (hash-set! fields field-val i-val)]
                                     [else (void)])
                                   (ValueA i-val store))])])]
    [CListLHS (li)
              ; split a list, convert to list first
              (type-case AnswerC (interp-to-list i-val env store)
                [ExceptionA (exn-val store) (ExceptionA exn-val store)]
                [ReturnA (value store) (ReturnA value store)]
                [ValueA (value store)
                        (type-case CVal value
                          [VList (mutable fields)
                                 ; map lhs -> fields
                                 (begin
                                   (map2 (lambda (lhs-item val)
                                           (interp-set-val lhs-item val env store))
                                         li
                                         fields)
                                   ; then return list
                                   (ValueA i-val store))]
                          [else (error 'interp "Not converted to list")])])]))

;; interp-class-to-instance : (listof string) (hashof string CVal) (hashof CVal CVal) CExp Env Store -> AnswerC
;; creates an instance given a class, and calls the constructor if appropriate
(define (interp-class-to-instance [bases : (listof string)]
                                  [classdefs : (hashof string CVal)]
                                  [fields : (hashof CVal CVal)]
                                  [args : CExp] [args-list : (listof CVal)]
                                  [env : Env] [store : Store]) : CVal
  (cond
    [(equal? (first bases) "bool")
     (cond
       [(= 0 (length args-list)) (VFalse)]
       [(= 1 (length args-list)) (if (get-truth-value (first args-list)) (VTrue) (VFalse))]
       [else (VFalse)])]
    [(equal? (first bases) "int")
     (cond
       [(= 0 (length args-list)) (VInt 0)]
       [(= 1 (length args-list)) (if (numeric? (first args-list))
                                     (VInt (floor (to-number (first args-list))))
                                     (if (get-truth-value (first args-list)) (VInt 1) (VInt 0)))]
       [else (VFalse)])]
    
    [else
     (local ([define new-fields (make-hash empty)]
             [define new-instance (VInstance bases classdefs new-fields)]
             [define class-env (env-extend env)])
       (begin
         ; copy fields to instance
         (map (lambda (key)
                (local ([define value (some-v (hash-ref fields key))])
                  (type-case CVal value
                    ; change VClosure to VMethod
                    [VClosure (base varargs args defaults body env)
                              (local ([define m-val (VMethod base new-instance varargs args defaults
                                                             body class-env)])
                                (hash-set! new-fields key m-val))]
                    ; leave all other fields as-is
                    [else (hash-set! new-fields key value)])))
              (hash-keys fields))
         ; call constructor
         (type-case (optionof CVal) (hash-ref new-fields (VStr "__init__"))
           [some (v) (interp-app v args env store)]
           [none () (ExceptionA (VUndefined) store)])
         ; return instance
         new-instance))]))

;; define locals-func-body for comparison
(define locals-func-body (CPrim1 'builtin-locals (CNone)))

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
                       [VClosure (bs v a d b e)
                                 ; TODO: don't special case locals()?
                                 (cond
                                   [(equal? b locals-func-body)
                                    (interp-env (CReturn locals-func-body) env store)]
                                   [else
                                    (local ([define extended-env (env-extend e)]
                                            [define super-loc (new-loc)])
                                      (begin
                                        (if (empty? a)
                                            (void)
                                            (begin
                                              (hash-set! (first extended-env) 'super super-loc)
                                              (hash-set! store super-loc (super-lambda (first a) bs extended-env))))
                                        (interp-app-helper v func a fields d
                                                       extended-env env store)))])]
                       ; add the first argument
                       [VMethod (bs inst v a d b e)
                                (local ([define super-loc (new-loc)]
                                        [define newloc (new-loc)]
                                        [define newenv (env-bind #t (env-extend e) (first a) newloc)])
                                  (begin
                                    ; bind super
                                    (if (empty? a)
                                        (void)
                                        (begin
                                          (hash-set! (first newenv) 'super super-loc)
                                          (hash-set! store super-loc (super-lambda (first a) bs newenv))))
                                    
                                    ; if inst = VNone, then replace with classdef for class method
                                    (hash-set! store newloc inst)
                                    (interp-app-helper v func (rest a) fields d
                                                       newenv env store)))]
                       [VClass (bases class-defs class-fields)
                               (ReturnA (interp-class-to-instance bases
                                                                  class-defs
                                                                  class-fields
                                                                  args fields env store)
                                        store)]
                       [VInstance (bases classdefs class-fields)
                                  (type-case (optionof CVal) (hash-ref class-fields (VStr "__call__"))
                                    [some (v) (interp-app v args env store)]
                                    [none () (interp-error "Applied instance without __call__" store)])]
                       [else (interp-error (string-append "Applied a non-function: " (pretty func)) store)])]
              [else (error 'interp "Application arguments not a list")])]))

;; interp-app-helper : ValueC (listof symbol) (listof CVal) Env Store -> AnswerC
;; interprets a function application, by first recursively binding the arguments
(define (interp-app-helper [varargs : boolean] [closure : CVal]
                           [params : (listof symbol)] [args : (listof CVal)] [defaults : (listof CExp)]
                           [closureEnv : Env] [appEnv : Env] [store : Store]) : AnswerC
  (cond
    ; apply!!
    [(and (empty? params) (empty? args))
     (type-case CVal closure
       [VClosure (bs v a d body e) (interp-env body closureEnv store)]
       [VMethod (bs inst v a d body e) (interp-env body closureEnv store)]
       [else (interp-throw-error 'TypeError empty appEnv store)])]
    
    ; no app args, symbols still, use defaults
    [(and (not (empty? params)) (empty? args) (not (empty? defaults)))
     (local ([define newloc (new-loc)]
             [define var-name (first params)]
             [define rest-def (if (empty? defaults) empty (rest defaults))])
       (if (equal? (first defaults) (CUndefined))
           (interp-throw-error 'TypeError empty closureEnv store)
           (type-case AnswerC (interp-env (first defaults) appEnv store)
             [ExceptionA (exn-val store) (ExceptionA exn-val store)]
             [ReturnA (value store) (ReturnA value store)]
             [ValueA (value store)
                     (begin
                       (hash-set! store newloc value)
                       (interp-app-helper varargs closure
                                          (rest params) empty rest-def
                                          (env-bind #t closureEnv var-name newloc)
                                          appEnv
                                          store))])))]
    
    ; combine app args left into last symbol (var args)
    [(and varargs (= 1 (length params)))
     (local ([define newloc (new-loc)]
             [define ans args])
       (type-case CVal closure
         [VClosure (bs v a d body e)
                   (begin
                     (hash-set! store newloc (VList #f args))
                     (interp-env body
                                 (env-bind #t closureEnv (first params) newloc)
                                 store))]
         [else (interp-throw-error 'TypeError empty appEnv store)]))]
    
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
                            (env-bind #t closureEnv var-name newloc)
                            appEnv
                            store)))]
    
    ; arity mismatch
    [else (interp-throw-error 'TypeError empty appEnv store)]))

;; print-error : CVal Store
;; prints the exception output given the exn-val
(define (print-error (exn-val : CVal) (store : Store))
  (type-case CVal exn-val
    [else (error 'interp (pretty exn-val))]))

