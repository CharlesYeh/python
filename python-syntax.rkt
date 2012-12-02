#lang plai-typed

(define-type LHS
  [IdLHS (id : symbol)]
  [BracketLHS (obj : PyExpr) (field : string)]
  [DotLHS (obj : PyExpr) (field : PyExpr)])

;(define-type SliceParams
;  [sliceParams (hasStart : boolean) (hasEnd : boolean) (hasStep : boolean)
;               (start : number) (end : number) (step : number)])

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  
  [PyId (x : symbol)]
  
  [PyClass (supers : (listof string)) (body : PyExpr)]
  [PyFunc (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  
  ; data types
  [PyInt (n : number)]
  [PyFloat (n : number)]
  [PyComplex (n : number)]
  [PyStr (s : string)]
  [PyList (mutable : boolean) (values : (listof PyExpr))]
  [PyDict (htable : (hashof PyExpr PyExpr))]
  [PyGetField (obj : PyExpr) (field : PyExpr)]

  [PyGlobal (vars : (listof symbol))]
  [PyNonlocal (vars : (listof symbol))]
  
  [PyTrue]
  [PyFalse]
  
  [PyAssign (lhs : LHS) (value : PyExpr)]
  [PyPrimAssign (op : symbol) (lhs : LHS) (value : PyExpr)]
  
  [PyCompare (ops : (listof symbol)) (left : PyExpr) (args : (listof PyExpr))]
  [PyPrim (op : symbol) (args : (listof PyExpr))]
  ;[PySlice (value : PyExpr) (params : SliceParams)]
  
  ; error control
  [PyTry (body : PyExpr) (else : PyExpr) (excepts : (listof PyExpr))]
  [PyExcept (type : PyExpr) (body : PyExpr)]
  [PyNamedExcept (name : symbol) (type : PyExpr) (body : PyExpr)]
  [PyTryFinally (body : PyExpr) (final : PyExpr)]

  [PyRaise (exc : PyExpr) (cause : PyExpr)]
  [PyReraise]
  
  ; loops
  [PyWhile (test : PyExpr) (body : PyExpr)]
  [PyForElse (id : symbol) (seq : PyExpr) (body : PyExpr) (else : PyExpr)]
  [PyIterator (id : symbol) (iter : PyExpr)]
  [PyGenerator (value-gen : (listof PyExpr)) (iters : (listof PyExpr))]

  [PyNone]
  [PyPass]
  [PyReturn (value : PyExpr)]
  ;[PyYield (value : PyExpr)]
  
  ; TODO:
  [PyBreak]
  [PyContinue])

