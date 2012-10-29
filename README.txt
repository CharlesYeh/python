CHOSEN TESTS
	types/test_booleans.py
	types/test_div_zero.py
	types/test_floats.py
	
	scope/lambda1.py
	scope/lambda2.py
	scope/lambda4.py


STRUCTURE
	The code is structured mostly as the stencil had it laid out.
"get-structured-python" converts the parsed code into a PyExpr, which is then desugared by
"python-desugar", which also handles most of the scope features. The desugared expression
is given to "python-lib", which handles built-in python library functions, then "python-interp"
is used to interpret the function.
	Exceptions were done in a similar manner to ParselExtend, where a single data type
separates exceptions from values. Throughout the interpreter, an ExceptionA is automatically
percolated back up without continuing interpretation.

	Errors just shown with (error ...). parse-python does this, so we thought it would be
appropriate to use it elsewhere as well.



DESIGN
	CORE REPRESENTATION
	We decided to have a variant for each primitive type. This means we plan on
having a VInt for integers, a VFloat for floats, a VStr for strings, and so on. We
chose to do this because, either way, we thought we would have to do type checking
for operations anyways. Being able to separate each easily makes things easier to handle.
	
	SCOPE
		Scope is done mainly through desugaring. The desugar process finds
variable assignments and brings them to the top level, or the top of a function (or
lambda) definition, whichever is first. They are defined as VUndefined, and are replaced
by the assignments. If something tries to access a variable before it has been assigned,
it will receive "VUndefined", and will know it is unbound and an error should
be thrown.
		To show pythonic scope, assignments replace the "closest"
(as close to the current scope) variable declaration, but accesses search up the scopes
until it finds a defined variable. This allows scope closing over variable accesses, but
not assignments.
		The environment is a linked list, and the store is a hash set for ease
in simulating python's scope. If a new function scope is created, simply adding to
the environment means old variables are untouched. After the new scope ends, the outer
scope is also able to continue using its variables as if the new scope did not exist.
However, if the new scope tries to access a variable, then it is able to search through
outer scopes for the last defined. 
