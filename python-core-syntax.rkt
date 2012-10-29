#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type FieldC
  [fieldC (name : string) (value : CExp)])

(define-type CExp
  [CInt (n : number)]
  [CFloat (n : number)]
  [CStr (s : string)]
  
  [CTrue]
  [CFalse]
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
  
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (prim : symbol) (left : CExp) (right : CExp)]
          
  [CObject (fields : (listof FieldC))]
  [CSet (id : symbol) (value : CExp)])

(define-type FieldV
  [fieldV (name : string) (value : CVal)])

(define-type CVal
  [VInt (n : number)]
  [VStr (s : string)]
  
  [VTrue]
  [VFalse]
  ; to handle python scope
  [VUndefined]
  
  [VClosure (args : (listof symbol)) (body : CExp) (env : Env)]
  
  [VObject (fields : (listof FieldV))])


(define-type AnswerC
  [ValueA (value : CVal) (store : Store)]
  [ExceptionA (exn : CVal) (store : Store)])


(define-type-alias Location number)
(define-type Binding
  [binding (name : symbol) (value : Location)])

(define-type-alias Env (listof Binding))
(define-type-alias Store (hashof Location CVal))


(define (interp-error str store)
  (ExceptionA (VObject (list (fieldV "message" (VStr str))
                             (fieldV "type" (VStr "Python")))) store))
(define (desugar-error str)
  (CError (CObject (list (fieldC "message" (CStr str))
                         (fieldC "type" (CStr "Python"))))))



