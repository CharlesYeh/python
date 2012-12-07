NOTE: I did the first part of super/test-super.py, but not the whole
thing.

Unfortunately, some things I did were due to the time constraint.
Things I hope to fix are labeled with a "; TODO" in a code.


	GENERATORS: (x for x in [1,2,3]) is immediately converted to
a list upon interpretation. I tried to create actual generators but
then realized cpsing most of the language was necessary. This also
applies to range()

	EXCEPTIONS: my exceptions are all defined in python-lib.rkt.
These class definitions are used to match the instance of an exception
with the definition. It's done in a manner very similar to isinstance.

	IS: I had some problems with this, and ultimately decided
not to do it for this deadline. There's a some-what working version,
but it's not perfect. I plan on introducing address fields into core
values themselves which represent a pointer to itself in the store.

	CLASSES: I decided to keep classes as expressions until actual
interpretation, whereas I was desugaring them into fields before. This
allows for accurate interpretation of other statements within the class.
To interpret the statements within the class, I create another local
scope and interpret the statements on top of it. I then pop off the
local scope and use it to create the fields of the class definition.
I also go through functions in the fields and remove the local scope
off the closure environment.

	SUPER: To have super() automatically add the correct the arguments,
I add it an extra binding at every function application so that I know
what the first argument name is and can use that within super as a
closure. This is pretty messy, but I found it to be the easiest to
do. python-native.rkt holds the function definition of super(), and
it takes the variable name of "self", which it uses directly as a variable
in its closure. This allows both instance methods and class methods to
call super appropriately.
	I modified instances to hold only non-method/closure fields.
super() actually returns the same instance, but with (rest bases), where
bases is a list of strings representing __mro__, or the order of method
resolution. (first bases) is the class type an instance is casted as.
Method and function lookups then go through a process where it searches
through bases from left to right, getting the class definition of each
base from classdefs, and looking in classdefs for the function.
	The arguments it takes right now are actually not similar to
python's. This is just to make it work as a 2-var operator. I need
to change this.


FOLLOWING NATIVE FUNCTIONS: I don't allow follow the function model
for a lot of things. For instance, I plan on fixing
(CGet (CDotLHS expr expr)) to actually use getattr instead of directly
fetching the attribute. Another example is super using __getattribute__.


python-native.rkt:
This includes a number of functions for primitive types. Unfortunately,
because my hashmaps are primitive types, its functions are special cased
out. The same goes for the bool and int classes. I plan on changing all
of these types to actually work as classes and not fully native primitive
types.

------------------------SECOND README------------------------


This submission is continued from the previous one, and is not from the 5 designs posted.
The tests this submission passes are specified in the file "passed" which runs them all.

	The tests which pass:
/exceptions/test-try-except.py
/exceptions/test-try-except-no-exception.py
/exceptions/test-try-except-else.py
/exceptions/try-except-else-no-exception.py
/exceptions/nested-else.py
/exceptions/nested.py

/dict/dict-bool.py
/dict/dict-contains.py
/dict/dict-get.py
/dict/dict-clear.py

/lists/test_list_identity.py
/lists/test_list_truth.py
/lists/test_list_simple.py

/tuple/tuple-constructors.py
/tuple/tuple-add.py
/tuple/tuple-truth.py
/tuple/tuple-length.py

/bool/bool-callable.py
/bool/bool-str.py
/bool/bool-int.py
/bool/bool-compare.py

/types/test_booleans.py
/types/test_div_zero.py
/types/test_floats.py
/types/types_truthy1.py
/types/types_truthy2.py
/types/test_simple_strings.py

/scope/lambda1.py
/scope/lambda2.py
/scope/lambda4.py
/scope/simple-nesting.py
/scope/nearest-enclosing-scope.py
/scope/simple-and-rebinding.py

/builtin/len.py
/builtin/callable.py
/builtin/isinstance.py




	Tuples vs Lists
I decided to use the same structure, and simply have a boolean denote whether it's mutable or not.
This way, functionalities which are the same across both types are easily implemented without a need
to "combine" two types.


	Classes and Instances
Class definitions are denoted by _Class, and their instances are denoted by _Instance. For example,
the values of a class definition and an instance are VClass and VInstance. Separating the two allow
for some easy separation of functionality, such as calling CApp on a VClass, which creates an instance,
whereas calling CApp on VInstance results in an error.

When an instance is created, the definitions are copied, so modifying the fields on the resulting
instance doesn't affect the class definition and future instances created.

Also, function definitions within a class are created as VMethod rather than VClosure. The sole difference
is that VMethod keeps track of its own instance, and automatically adds a reference to app calls. This
allows for a clear and easy separation of the two different functions.

It does not yet handle inheritance properly.


	Functions, and default/variable arguments
___assertRaises can accept different numbers of arguments, so variable arguments was necessary. At
first, this was done through default arguments, where each function would keep track of default arguments,
applying them if app arguments ran out. Setting a default argument to VUndefined means there is no default.
This method made it easy to apply default arguments in the correct order by matching the default argument
order to the function arguments, and copying over the left overs for the unfilled function arguments.

Variable arguments are specified by a boolean flag on VClosure, and the only difference is that when there
is only one function argument symbol left, the remaining app arguments are combined into a list and given
to the function as such. This contained variable argument functionality into just the application case, so
things could be handled easily elsewhere.


	Control flow
Return statements are currently handled in a similar manner to Exceptions, except that they are stopped
by CApp instead of CTry. I plan on doing Continue and Break in the same way.


------------------------FIRST README------------------------

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
