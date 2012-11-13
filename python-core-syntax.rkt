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
  [CList (fields : (listof CExp))]
  [CDict (htable : (hashof CExp CExp))]
  
  [CTrue]
  [CFalse]
  [CNone]
  [CUndefined]
  
  [CSeq (e1 : CExp) (e2 : CExp)]

  ; control
  [CIf (test : CExp) (then : CExp) (else : CExp)]

  ; error control
  [CError (e1 : CExp)]
  [CTryExcept (body : CExp) (excepts : (listof CExp))]
  [CExcept (type : CExp) (body : CExp)]

  [CId (x : symbol)]
  [CLet (x : symbol) (bind : CExp) (body : CExp)]
  
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (body : CExp)]

  [CPass]
  [CReturn (value : CExp)]
  
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (prim : symbol) (left : CExp) (right : CExp)]
          
  [CObject (fields : (listof FieldC))]
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
  
  [VClosure (args : (listof symbol)) (body : CExp) (env : Env)]
  
  [VObject (fields : (listof FieldV))]
  [VList (fields : (listof CVal))]
  [VDict (htable : (hashof CVal CVal))]
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

(define-type-alias Env (listof Binding))
(define-type-alias Store (hashof Location CVal))

; convenience method for interpretation errors
(define (interp-error str store)
  (ExceptionA (VObject (list (fieldV "message" (VStr str))
                             (fieldV "type" (VStr "Python")))) store))

; convenience method for desugaring errors
(define (desugar-error str)
  (CError (CObject (list (fieldC "message" (CStr str))
                         (fieldC "type" (CStr "Python"))))))


