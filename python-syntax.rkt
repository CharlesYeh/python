#lang plai-typed

; TODO: only use LHS in the core, get rid of it in the outer syntax
(define-type LHS
  [IdLHS (id : symbol)]
  [DotLHS (obj : PyExpr) (field : PyExpr)]
  [ListLHS (li : (listof LHS))])

; TODO: perhaps merge into LHS
(define-type SliceParams
  [sliceParams (hasStart : boolean) (hasEnd : boolean) (hasStep : boolean)
               (start : number) (end : number) (step : number)]
  [indexParams (value : PyExpr)])

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  
  [PyClass (supers : (listof string)) (body : PyExpr)]
  [PyFunc (static : boolean) (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  [PyReturn (value : PyExpr)]
  
  ; data types
  [PyDict (has-values : boolean) (htable : (hashof PyExpr PyExpr))]
  [PyList (mutable : boolean) (values : (listof PyExpr))]
  [PyInt (n : number)]
  [PyFloat (n : number)]
  [PyStr (s : string)]
  [PyTrue]
  [PyFalse]
  [PyNone]
  
  ; var manipulation
  [PyId (x : symbol)]
  [PyGetField (obj : PyExpr) (field : PyExpr)]
  [PyDelete (lhs : LHS)]
  [PyAssign (lhs : LHS) (value : PyExpr)]
  [PyPrimAssign (op : symbol) (lhs : LHS) (value : PyExpr)]
  [PyPrim (op : symbol) (args : (listof PyExpr))]

  ; scope
  [PyGlobal (vars : (listof symbol))]
  [PyNonlocal (vars : (listof symbol))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyCompare (ops : (listof symbol)) (left : PyExpr) (args : (listof PyExpr))]
  [PySubscript (value : PyExpr) (params : SliceParams)]
  
  ; error control
  [PyTry (body : PyExpr) (else : PyExpr) (excepts : (listof PyExpr))]
  [PyTryFinally (body : PyExpr) (final : PyExpr)]
  [PyExcept (type : PyExpr) (body : PyExpr)]
  [PyNamedExcept (name : symbol) (type : PyExpr) (body : PyExpr)]

  [PyRaise (exc : PyExpr) (cause : PyExpr)]
  [PyReraise]
  
  ; loops
  [PyWhile (test : PyExpr) (body : PyExpr)]
  [PyForElse (id : symbol) (seq : PyExpr) (body : PyExpr) (else : PyExpr)]

  [PyIterator (id : symbol) (iter : PyExpr)]
  [PyGenerator (value-gen : PyExpr) (iters : (listof PyExpr))]
  ;[PyYield (value : PyExpr)]

  [PyPass]
  
  ; TODO:
  [PyBreak]
  [PyContinue])

