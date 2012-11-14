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

(define-type FieldP
  [fieldP (name : string) (value : PyExpr)])

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  
  [PyId (x : symbol)]
  
  [PyClass (body : PyExpr)]
  [PyFunc (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  
  [PyIf (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  
  ; data types
  [PyInt (n : number)]
  [PyStr (s : string)]
  [PyFloat (n : number)]
  [PyComplex (n : number)]
  [PyList (mutable : boolean) (values : (listof PyExpr))]
  [PyDict (htable : (hashof PyExpr PyExpr))]
  [PyObject (fields : (listof FieldP))]
  [PyGetField (obj : PyExpr) (field : PyExpr)]

  [PyGlobal (vars : (listof symbol))]
  [PyNonlocal (vars : (listof symbol))]
  
  [PyTrue]
  [PyFalse]
  
  [PyAssign (lhs : LHS) (value : PyExpr)]
  [PyPrimAssign (op : symbol) (lhs : LHS) (value : PyExpr)]
  
  [PyCompare (ops : (listof symbol)) (left : PyExpr) (args : (listof PyExpr))]
  [PyPrim (op : symbol) (args : (listof PyExpr))]
  
  ; error control
  [PyTry (body : PyExpr) (else : PyExpr) (excepts : (listof PyExpr))]
  [PyTryFinally (body : PyExpr) (final : PyExpr)]
  [PyRaise (exc : PyExpr) (cause : PyExpr)]
  [PyReRaise]
  [PyExcept (type : PyExpr) (body : PyExpr)]
  [PyNamedExcept (type : PyExpr) (id : symbol) (body : PyExpr)]
  
  ; loops
  [PyWhile (test : PyExpr) (body : PyExpr)]
  [PyFor (id : symbol) (seq : PyExpr) (body : PyExpr)]
  [PyForElse (id : symbol) (seq : PyExpr) (body : PyExpr) (else : PyExpr)]

  [PyNone]
  [PyPass]
  [PyReturn (value : PyExpr)]
  [PyBreak]
  [PyContinue])

