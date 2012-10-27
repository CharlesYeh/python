#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum (n : number)]
  [CStr (s : string)]
  
  [CTrue]
  
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
  [CPrim2 (prim : symbol) (left : CExp) (right : CExp)])

(define-type CVal
  [VNum (n : number)]
  [VStr (s : string)]
  
  [VTrue]
  [VFalse]
  
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp)])

(define-type-alias Location number)
(define-type-alias Binding
  [bind (name : symbol) (value : Location)])

;(define-type-alias Env (hashof symbol Location))

(define-type-alias Env (listof Binding))
(define-type-alias Store (hashof Location CVal))


