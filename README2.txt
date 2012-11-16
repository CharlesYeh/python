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



