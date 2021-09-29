Interpreter for Lisp
====================

You will write the evaluator for a simple lisp language. The data types
for the abstract syntax tree are provided, as is a parser and the
code that drives the read-eval-print loop (REPL):

* `ast.sml`: data types for the abstract syntax tree
* `parse.sml`: the parser
* `lisp.sml`: the tokenizer and main read-eval-print loop
* `eval.sml`: you should write the evaluator here

The grammar for the language is provided here:

```
input           ->  expression | fundef
fundef          ->  ( define function arglist expression )
arglist         ->  ( variable* )
expression      ->  value
                 |  variable
                 |  ( if expression expression expression )
                 |  ( while expression expression )
                 |  ( set variable expression )
                 |  ( begin expression+ )
                 |  ( optr expression* )
optr            ->  function | value-op
value           ->  integer | quoted-const
value-op        ->  + | - | * | / | = | < | > | cons | car | cdr
                 |  number? | symbol? | list? | null? | print
quoted-const    ->  'S-expression
S-expression    ->  integer | symbol | ( S-expression* )
symbol          ->  name
function        ->  name
variable        ->  name
integer         ->  sequence of digits, possibly preceded by minus sign
name            ->  any sequence of characters not an integer, and not
                    containing a blank or any of the following characters:
                    ( ) ;.
```

A function cannot be one of the “keywords” define, if, while, begin,
or set, or any of the value-op's. Aside from this, names can use any
characters on the keyboard. Comments are introduced by the
character ';' and continue to the end of the line; this is why ';'
cannot occur within a name. A session is terminated by entering
“quit”; thus it is highly inadvisable to use this name for a
variable.


The details
-----------

You must write the `eval` function that evaluates an expression in
lisp. It's type signature is:

    fn: (string * sxp) list * expression -> (string * sxp) list * sxp

The `eval` function accepts a pair containing an environment, i.e.,
the local variables (called *rho*), and the expression to evaluate
(an instance of the `expression` type). It returns the updated
environment (with any changes made during evaluation) and the result
value (an s-expression).

Expression evaluation depends on the specific kind of expression.
See `ast.sml` for the type definitions:


*   `ValExp`: if the input is a value, simply return the value (the
    s-expression stored inside the `ValExp` instance).

*   `VarExp`: if the input is a variable, look it up in the local
    environment and return the value found. If it is not present in
    the local environment, try the global environment. If it is not
    present there, signal an error by raising a RuntimeError with a
    suitable message. Use the functions defined at the top of
    `eval.sml` to look up values in *rho* and in the global
    environment.

*   `IfExp`: evaluate the condition. If the result is a true
    value (anything other than `NilSxp`), evaluate and return the
    “then” branch. Otherwise, evaluate and return the “else” branch.

*   `WhileExp`: evaluate the condition. If the result is false
    (`NilSxp`) then return `NilSxp`, otherwise evaluate the body and
    repeat.

*   `SetExp`: evaluate the value. If the variable name is present in
    the local environment, then store the value there. If not, store
    it in the global environment. Return the value.

*   `BeginExp`: evaluate all of the expressions in order and return
    the result of evaluating the last one.

*   `BinaryBuiltinExp` and `UnaryBuiltinExp` primitive operations:

    *   `+`, `-`, `*`, `/`:
        , `<`, and `>`: evaluate the arguments and
        make sure both results are `NumSxp` instances. Apply the
        appropriate operation on the numbers, wrap the result in a
        `NumSxp`, and return it.

    *   `<`, and `>`: evaluate the arguments and make sure both
        results are `NumSxp` instances. Apply the appropriate
        operation on the numbers, return `SymSxp "T"` for true or
        `NilSxp` for false.

    *   `=`: evaluate both arguments and compare the results. The
        results are equal if: (1) they are both numbers and they are
        the same number, (2) they are both symbols and they are the same
        symbol, or (3) they are both nil values. All other cases should
        return false (the `NilSxp` value). Return `SymSxp "T"` for true.

    *   `cons`: evaluate both arguments. If the second argument is
        anything other than a list or a nil value, signal an error.
        Otherwise, build a list value from them and return it.

    *   `car`, `cdr`: evaluate the arguments and insist that the
        result is a list.  Return the appropriate part.

    *   `number?`, `symbol?`, `list?`, and `null?`: evaluate the
        argument and return either true (the symbol `T`) or false
        (the `NilSxp` value) based on the type of the result.

    *   `print`: , evaluate the argument, convert the result to a
        string using the `sxpToString` function in `ast.sml`, and
        print it.  Return the value.

*   User-defined functions: get the named function from the
    global list of functions (it is an error if the function
    does not exist). Make sure the number of arguments passed in
    matches the number of formal parameters in the function
    definition.

    Create a new environment where the keys are the formal
    parameter names and the values are the evaluated arguments.
    Use this new environment when you evaluate the body of the
    function. Return the result.

In each of these cases, `eval` is given a local environment called
*rho* as one of the inputs, and it must return an updated version of
*rho* as one of the return values. *rho* is just a list of pairs,
where each pair contains the name of the variable and the value of
the variable. Evaluating an expression may have a *side effect*
where one or more variables change, so it is important to keep track
of those changes. Since lists are immutable, the only way to make a
change to *rho* is to return the modified list.

This is more complicated with more complex expressions. For example,
consider an if expression: it has expressions for the condition, the
"then" part, and the "else" part. The following steps must be taken
to correctly evaluate it:

*   Evaluate the condition part (using a recursive call the `eval`)
    using *rho* and capture both the return value and the updated
    environment, which we will call *rho'*.

    *   If the condition returned a true value (anything other than
        `NilSxp` then evaluate the "then" part with a recursive call
        to `eval`. Use *rho'* in the recursive call, since it
        contains any changes that might have been made to the
        environment while the condition was being evaluated. Return
        both the return value and the updated environment returned
        by the recursive `eval` call when it processed the "then"
        part. Think of this final version of the environment as
        *rho''*.

    *   If the condition return a false value (`NilSxp`) then
        evaluate the "else" part with a recursive call to `eval`.
        The recursive call should use *rho'* and returns *rho''* as
        well as the result value. Return that result value and
        *rho''*.

The process is similar for all expression types that involve
recursive calls to `eval`. For each new recursive call, the latest
version of *rho* needs to be passed in, and it returns an updated
version that should be used (or returned) from that point forward.

The only exception to this is in function applications. The same
chaining of *rho* environments is used while evaluating the argument
list, but then the formal parameters are bound to the argument
values and this forms a completely new environment that is used when
evaluating the function body. Call the last version of *rho* that
resulted from evaluating the argument list *rho'* and the new
environment created when binding the formal parameters to the
argument values *newrho*. Here's how they interact:

*   When `eval` is called to evaluate the expression with the
    function call, it gets *rho* as its input. It evaluates the
    argument list (a list of expressions) by calling `eval`
    recursively, once for each argument, and gather the resulting
    values. Each time it makes a recursive `eval` call it trades the
    latest version of *rho* for a new *rho'*, which is the version
    used going forward.

*   The new environment *newrho* is created from scratch, with one
    entry for each formal parameter. The name of the variable is the
    formal parameter, and the value is the result handed back by
    each recursive call to `eval`. So if there were five parameters
    (and thus five argumentss), there will be five recursive calls
    to `eval` (one for each argument) and a total of six version of
    *rho*:

    *   eval is called with *rho*
    *   argument 1: call `eval` giving it *rho* and getting back
        *rho1*
    *   argument 2: call `eval` giving it *rho1* and getting back
        *rho2*
    *   argument 3: call `eval` giving it *rho1* and getting back
        *rho3*
    *   argument 4: call `eval` giving it *rho1* and getting back
        *rho4*
    *   argument 5: call `eval` giving it *rho1* and getting back
        *rho5*
    *   call the final version *rho'* as a generic label, but it is
        the same as *rho5* in this exaple

*   Evaluate the body expression of the function:

    *   The recursive call to `eval` gets *newrho* and the body
        expression as input

    *   It returns the result value and *newrho'*

*   The original call to `eval` is finally ready to return. It
    returns the result value (from evaluating the body expression)
    and *rho'*. Note that it does NOT return *newrho'*. That value
    is discarded at this point.
