## Implementation of lexical symbols in Maxima language

### Outline

 * A lexical symbol is a symbol defined in a lexical extent.

 * A lexical extent is one of the following operators and its arguments:
  * `block`
  * `lambda`
  * named function
  * array function (both `f[x]` and `f[x](y)`)
  * `for` loop
  * macro (i.e., defined by `::=`) (*not implemented yet*)
  * `buildq` (*not implemented yet*)
  * `makelist` (*not implemented yet*)
  * `create_list` (*not implemented yet*)

 * Lexical symbols in different lexical extents are distinct, even if they have the same name.

 * Any symbol which is not a lexical symbol is a global symbol.
  * There is a unique global symbol for any given name.
  * Dynamic binding is applied to global symbols.

 * Function calls (named and unnamed) are evaluated in the lexical environment in which they were defined.

 * All functions (named and unnamed) defined in a lexical extent share the same lexical environment.
  * *unresolved question*: same environment, or similar, in the sense that the symbols are the same,
    but the values might be different?

 * A lexical environment is a list of pairs of symbols and values.
  * *unresolved question*: can a symbol appear in more than one environment?
    I suspect we want the answer to be yes, because then a closure can capture a specific value.
    But if so, then how to we get functions to share lexical variables?

 * An expression containing lexical symbols returned from a lexical extent
   is wrapped in an object, called a closure, comprising the lexical environment and the expression.

 * A closure is evaluated by evaluating its contained expression with the symbols
   in its contained environment bound to their current values.

 * A closure is simplified according to some rules specifically for closures.
  * closure([], expr) --> expr
  * closure([env], expr) --> expr when expr is free of all of the symbols in env
  * closure([env1], closure([env2], expr)) --> closure([env1, env2], expr)

 * Also any simplifications applicable to expr are applied.
