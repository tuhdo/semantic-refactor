<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Context-sensitive menu:</a></li>
<li><a href="#sec-2">2. Generate class and function implementation</a></li>
<li><a href="#sec-3">3. Generate class getters and setters</a></li>
<li><a href="#sec-4">4. Generate function prototype</a></li>
<li><a href="#sec-5">5. Convert function to function pointer</a></li>
<li><a href="#sec-6">6. Convert function to function parameter</a></li>
<li><a href="#sec-7">7. Move semantic units</a></li>
<li><a href="#sec-8">8. Extract function</a></li>
<li><a href="#sec-9">9. Current limitation</a></li>
</ul>
</div>
</div>

# Context-sensitive menu:<a id="sec-1" name="sec-1"></a>

When user runs the command, a menu appears and offer refactoring
choices based on current scope of semantic tag. For example, if the
cursor is inside a class, the menu lists choices such as generate
function implementations for the class, generate class
getters/setters&#x2026; Each menu item also includes its own set of
options, such as perform a refactoring option in current file or other
file.

# Generate class and function implementation<a id="sec-2" name="sec-2"></a>

From the header file, all function prototypes of a class can be
generated into corresponding empty function implementation in a source
file. The generated function implementations also include all of their
(nested) parents as prefix in the names, if any. If the class is a
template, then the generated functions also includes all templates
declarations and in the parent prefix properly.

Since all function implementations can be generated a class, this
feature should be present.

Demonstration 1 (with nested classes):

[![img](class-gen-func-impl.gif)](class-gen-func-impl.gif)

Demonstration 2 (with nested classes and templates):

[![img](class-template-gen-func-impl.gif)](class-template-gen-func-impl.gif)

Note that in the demos, you see some regions highlighted with red
colors. Those regions are spaces inserted by Semantic Refactor, so
when you move the cursor inside a generated function body, you can
start typing code immediately without indenting.

# Generate class getters and setters<a id="sec-3" name="sec-3"></a>

All getters and setters of all variables in a class can be
automatically generated with appropriate type information. Obviously,
generating individual getter and setter for each variable works as
well.

Demonstration:

[![img](class-gen-getters-setters.gif)](class-gen-getters-setters.gif)

# Generate function prototype<a id="sec-4" name="sec-4"></a>

When the cursor is in a function implementation, a function prototype
can be generated and placed in a selected file. When the prototype is
moved into, its prefix is stripped.

Demonstration:

[![img](func-impl-to-prototypep.gif)](func-impl-to-prototypep.gif)

# Convert function to function pointer<a id="sec-5" name="sec-5"></a>

Any function can be converted to a function pointer with typedef. 

Demonstration:

[![img](function-pointer-gen.gif)](function-pointer-gen.gif)

# Convert function to function parameter<a id="sec-6" name="sec-6"></a>

The converted function pointer can also be placed as a parameter of a
function. In this case, all the parameter names of the function
pointer is stripped.

Demonstration:

[![img](function-pointer-as-parameter-gen.gif)](function-pointer-as-parameter-gen.gif)

# Move semantic units<a id="sec-7" name="sec-7"></a>

Any meaningful tags recognized by Semantic (class, function, variable,
namespace&#x2026;) can be moved relative to other tags in current file or
any other file.

Demonstration:

[![img](class-move.gif)](class-move.gif)

# Extract function<a id="sec-8" name="sec-8"></a>

Select a region and turn it into a function, with relevant variables
turned into function parameters and preserve full type information.
Notice that after the region is replaced with a function call, in the
minibuffer (at the bottom), Semantic shows the interface of newly
function immediately if `global-semantic-idle-summary-mode` is enabled.

Demonstration (C mode):

[![img](extract-function.gif)](extract-function.gif)

Demonstration (C++ mode):

[![img](extract-function-cpp.gif)](extract-function-cpp.gif)

# Current limitation<a id="sec-9" name="sec-9"></a>

In the C++ demo, the extracted function does not include namespace
prefix for its parameters. This is because currently Semantic Refactor
can only operate with Semantic tags in current buffer. Things starts
getting much more complicated outside of current file. For example,
the namespace information of `map` is not in `map` header file that we
include, but in `bits/stl_map.h`. To search for such information
requires Semantic to perform exhaustive search for all the included
files, which would take a long time and block Emacs.

One solution to this problem is to use a fast external indexer like
GNU Global along with Semantic. GNU Global can generate tag database
fast, but lack necessary information for smart refactoring; Semantic
is smart but is slow. We can combine them to make the best of both:
Global collects tag positions and Semantic decides which tag position
is valid and how to refactor with valid tags. 

This is just a plan.
