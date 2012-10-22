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
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  
  [PyNum (n : number)]
  [PyString (s : string)]
  [PyTrue]
  [PyFalse]
  
  [PyAssign (lhs : LHS) (value : PyExpr)]
  [PyPrimAssign (op : symbol) (lhs : LHS) (value : PyExpr)]
  
  [PyPrim (op : symbol) (args : (listof PyExpr))]
  
  [PyPreInc (id : symbol)]
  [PyPostInc (id : symbol)]
  [PyPreDec (id : symbol)]
  [PyPostDec (id : symbol)]
  
  [PyTryExcept (body : PyExpr) (excepts : (listof TryExcept))]
  [PyRaise (error : PyExpr)]
  [PyReRaise]
  
  [PyFunc (name : symbol) (args : (listof symbol)) (func : PyExpr)]
  
  [PyWhile (test : PyExpr) (body : PyExpr)]
  [PyFor (id : symbol) (seq : PyExpr) (body : PyExpr)]
  [PyBreak]
  [PyContinue]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)])

