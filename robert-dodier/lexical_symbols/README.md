## Implementation of lexical symbols in Maxima language

### Status of this proposal

This package, maxima-packages/robert-dodier/lexical\_symbols,
is an unofficial, optional package to reimplement local variables as lexical symbols.

I hope that the policies outlined here will be someday incorporated into Maxima,
perhaps in the same way as they are implemented here, perhaps implemented in a way
that differs in a small or large way.

Everything here is subject to change, although some items seem more fixed than others.
At this point, it seems unlikely that the large-scale policies
(local variables are lexical symbols and functions and expressions are tied to their lexical environment)
will change, but it's too soon to say so for sure.

### How to use this package

```
(%i1) push ("path/to/maxima-packages/robert-dodier/lexical_symbols/###.lisp", file_search_lisp) $
(%i2) load ("with-lexical-environment.lisp");
```

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

 * Lexical symbols defined in different lexical extents are distinct, even if they have the same name.

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
   in its contained environment bound to their current values. (*not implemented yet*)

 * A closure is simplified according to some rules specifically for closures.
  * closure([], expr) --> expr
  * closure([env], expr) --> expr when expr is free of all of the symbols in env
  * closure([env1], closure([env2], expr)) --> closure([env1, env2], expr)

 * Also any simplifications applicable to expr are applied.
