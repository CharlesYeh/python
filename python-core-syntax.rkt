#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CLHS
  [CIdLHS (id : symbol)]
  [CDotLHS (obj : CExp) (field : CExp)]
  [CListLHS (li : (listof CLHS))])

; the core expression type variants
(define-type CExp
  [CSeq (e1 : CExp) (e2 : CExp)]

  [CClass (bases : (listof string)) (body : CExp)]
  [CFunc (varargs : boolean) (args : (listof symbol)) (defaults : (listof CExp)) (body : CExp)]
  ; args is to be a CList so an empty list of parameters can be passed around
  [CApp (fun : CExp) (args : CExp)]
  [CReturn (value : CExp)]
  
  [CDict (has-values : boolean) (htable : (hashof CExp CExp))]
  [CList (mutable : boolean) (fields : (listof CExp))]
  [CInt (n : number)]
  [CFloat (n : number)]
  [CStr (s : string)]
  [CTrue]
  [CFalse]
  [CNone]
  [CUndefined]

  ; var manipulation
  [CGet (lhs : CLHS)]
  [CSet (lhs : CLHS) (value : CExp)]
  [CDel (lhs : CLHS)]
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (prim : symbol) (left : CExp) (right : CExp)] 

  ; control
  [CIf (test : CExp) (then : CExp) (else : CExp)]

  ; scope
  [CLet (x : symbol) (bind : CExp) (body : CExp)]

  ; error control
  [CError (e1 : CExp)]
  [CTry (body : CExp) (orelse : CExp) (excepts : (listof CExp))]
  [CTryFinally (body : CExp) (final : CExp)]
  [CExcept (type : CExp) (body : CExp)]
  [CNamedExcept (name : symbol) (type : CExp) (body : CExp)]

  ; generator expr
  [CIterator (id : symbol) (iter : CExp)]
  [CGenerator (value-gen : CExp) (iters : (listof CExp))]
  ;[CYield (value : CExp)]
  
  [CPass]

)
  
; the value data type
(define-type CVal
  [VClass (bases : (listof string)) (classdefs : (hashof string CVal)) (fields : (hashof CVal CVal))]
  [VInstance (bases : (listof string)) (classdefs : (hashof string CVal)) (fields : (hashof CVal CVal))]
  [VClosure (varargs : boolean) (args : (listof symbol)) (defaults : (listof CExp)) (body : CExp) (env : Env)]
  ; closure from an instance
  [VMethod (inst : CVal) (varargs : boolean) (args : (listof symbol)) (defaults : (listof CExp)) (body : CExp) (env : Env)]

  [VInt (n : number)]
  [VFloat (n : number)]
  [VStr (s : string)]
  
  [VDict (has-values : boolean) (htable : (hashof CVal CVal))]
  [VList (mutable : boolean) (fields : (listof CVal))]
  [VTrue]
  [VFalse]
  [VNone]

  ; to handle python scope
  [VUndefined]
  
  [VGenerator (expr : CExp) (env : Env)]
)

; the combination value with store, also used to keep track of exceptions
(define-type AnswerC
  [ValueA (value : CVal) (store : Store)]
  [ReturnA (value : CVal) (store : Store)]
  [ExceptionA (exn : CVal) (store : Store)])
  
  ; generator yield, (yield x) gives YieldImm, which the statement "above" will
  ; turn into a YieldA so that the return point is saved
  ;[YieldImm (value : CVal) (env : Env) (store : Store)]
  ;[YieldA (value : CVal) (next : CExp) (env : Env) (store : Store)])


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


