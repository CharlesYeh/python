#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type FieldC
  [fieldC (name : string) (value : CExp)])

; the core expression type variants
(define-type CExp
  [CInt (n : number)]
  [CFloat (n : number)]
  [CStr (s : string)]
  [CList (mutable : boolean) (fields : (listof CExp))]
  ; MERGE SET INTO THIS ##
  [CDict (htable : (hashof CExp CExp))]
  [CGetField (obj : CExp) (field : CExp)]
  [CSetField (obj : CExp) (field : CExp) (value : CExp)]
  [CClass (bases : (listof string)) (fields : (hashof CExp CExp))]
  
  [CGenerator (genexp : CExp)]
  
  [CTrue]
  [CFalse]
  [CNone]
  [CUndefined]
  
  [CSeq (e1 : CExp) (e2 : CExp)]

  ; control
  [CIf (test : CExp) (then : CExp) (else : CExp)]

  ; error control
  [CError (e1 : CExp)]
  [CTry (body : CExp) (orelse : CExp) (excepts : (listof CExp))]
  [CTryFinally (body : CExp) (final : CExp)]
  [CExcept (type : CExp) (body : CExp)]
  [CNamedExcept (name : symbol) (type : CExp) (body : CExp)]

  [CId (x : symbol)]
  [CLet (x : symbol) (bind : CExp) (body : CExp)]
  
  ; args is to be a CList so an empty list of parameters can be passed around
  [CApp (fun : CExp) (args : CExp)]
  [CFunc (varargs : boolean) (args : (listof symbol)) (defaults : (listof CExp)) (body : CExp)]

  [CPass]
  [CReturn (value : CExp)]
  
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (prim : symbol) (left : CExp) (right : CExp)]
  
  [CSet (id : symbol) (value : CExp)])
  
(define-type FieldV
  [fieldV (name : string) (value : CVal)])

; the value data type
(define-type CVal
  [VInt (n : number)]
  [VFloat (n : number)]
  [VStr (s : string)]
  
  [VTrue]
  [VFalse]
  [VNone]
  ; to handle python scope
  [VUndefined]
  
  [VClosure (varargs : boolean) (args : (listof symbol)) (defaults : (listof CExp)) (body : CExp) (env : Env)]
  ; closure from an instance
  [VMethod (inst : CVal) (varargs : boolean) (args : (listof symbol)) (defaults : (listof CExp)) (body : CExp) (env : Env)]
  
  [VGenerator (expr : CExp) (env : Env)]
  [VList (mutable : boolean) (fields : (listof CVal))]
  [VDict (htable : (hashof CVal CVal))]
  [VClass (bases : (listof string)) (fields : (hashof CVal CVal))]
  [VInstance (bases : (listof string)) (fields : (hashof CVal CVal))]
)

; the combination value with store, also used to keep track of exceptions
(define-type AnswerC
  [ValueA (value : CVal) (store : Store)]
  [ReturnA (value : CVal) (store : Store)]
  [ExceptionA (exn : CVal) (store : Store)])


; Env keeps track of id to location,
; and Store keeps track of location to value
(define-type-alias Location number)
(define-type Binding
  [binding (name : symbol) (value : Location)])

(define-type-alias Scope (hashof symbol Location))
(define-type-alias Env (listof Scope))
(define-type-alias Store (hashof Location CVal))

; convenience method for interpretation errors
(define (interp-error str store)
  (error 'interp (string-append "THIS IS AN ERROR" str)))

; convenience method for desugaring errors
(define (desugar-error str)
  (error 'desugar (string-append "THIS IS AN ERROR" str)))


