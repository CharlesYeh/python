#lang plai-typed

(define-type LHS
  [IdLHS (id : symbol)]
  [BracketLHS (obj : PyExpr) (field : string)]
  [DotLHS (obj : PyExpr) (field : symbol)])

(define-type TryExcept
  [Except (body : PyExpr)]
  [ExceptError (error : PyExpr) (body : PyExpr)]
  [ExceptIdError (error : PyExpr) (id : symbol) (body : PyExpr)]
  [ExceptElse (body : PyExpr)])

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  
  [PyId (x : symbol)]
  
  [PyFunc (name : symbol) (args : (listof symbol)) (func : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  
  ; data types
  [PyInt (n : number)]
  [PyFloat (n : number)]
  [PyTuple (values : (listof PyExpr))]
  [PyList (values : (listof PyExpr))]
  [PyStr (s : string)]
  [PyTrue]
  [PyFalse]
  
  [PyAssign (lhs : LHS) (value : PyExpr)]
  [PyPrimAssign (op : symbol) (lhs : LHS) (value : PyExpr)]
  
  [PyCompare (ops : (listof symbol)) (left : PyExpr) (args : (listof PyExpr))]
  [PyPrim (op : symbol) (args : (listof PyExpr))]
  
  [PyPreInc (id : symbol)]
  [PyPostInc (id : symbol)]
  [PyPreDec (id : symbol)]
  [PyPostDec (id : symbol)]
  
  ; error control
  [PyTryExcept (body : PyExpr) (excepts : (listof PyExpr))]
  [PyTryElseExcept (body : PyExpr) (else : PyExpr) (excepts : (listof PyExpr))]
  [PyRaise (exc : PyExpr) (cause : PyExpr)]
  [PyReRaise]
  [PyExcept (type : PyExpr) (body : PyExpr)]
  [PyNamedExcept (type : PyExpr) (id : symbol) (body : PyExpr)]
  
  ; loops
  [PyWhile (test : PyExpr) (body : PyExpr)]
  [PyFor (id : symbol) (seq : PyExpr) (body : PyExpr)]
  [PyForElse (id : symbol) (seq : PyExpr) (body : PyExpr) (else : PyExpr)]

  [PyPass]
  [PyBreak]
  [PyContinue])

