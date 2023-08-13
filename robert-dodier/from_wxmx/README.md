Package from\_wxmx for Maxima
Robert Dodier

from\_wxmx is a package to read .wxmx files (created by wxMaxima).

Functions
---------

parse\_xml -- parse XML input into a parse tree representation

    parse\_xml(f) where f is an XML string (containing XML, not a filename), input stream, or Lisp pathname

    Returns an XML parse tree (specific to the XML parser implementation, which is XMLS [1] at the moment.)

expressions\_from\_xml -- convert an XML parse tree into a Maxima expression

    expressions\_from\_xml(e) where e is a text node or nesting node in an XML parse tree

    Returns a Maxima expression which is a verbatim reprepresentation of the parse tree

tokens\_from -- extract a list of tokens from WHAT XML TAG??

    tokens\_from(e) where e is a Maxima expression which is a verbatim representation of the parse tree rooted at an `<output>` XML tag (CORRECT ??)

    Returns a list of tokens, e.g.: `[foo, \\(, x, \\+, 123, \\)]`

parse\_token\_list -- reconstruct a Maxima expression from a list of tokens

    parse\_token\_list(l) where l is a list of tokens, e.g.: `[foo, \\(, x, \\+, 123, \\)]`

    Returns a Maxima expression reconstituted from the list of tokens, e.g. `foo(x + 123)` for the above list

split\_mth -- divide mth expressions, with one expression per output label

    split\_mth(e) where e is a Maxima expression `mth()(...)` containing zero or more `lbl()(...)` expressions

    Returns a list of equations of the form `foo = bar` where ??

Example
-------

```{maxima}
load ("from_wxmx.mac");
S: openr ("output-examples-simple.xml");
parse_tree: parse_xml (S);
close (S);
expr_verbatim: expressions_from_xml (parse_tree);
/* COPY THIS TO LEXICAL_SYMBOLS TEST CASES */
extract_outputs (e) :=
  block ([L: [], f],
         f: lambda ([e], if not atom(e) and not atom(op(e)) and op(op(e)) = 'output then push (e, L), e),
         scanmap (f, e),
         reverse (L));
outputs: extract_outputs (expr_verbatim);
outputs_mths: map (first, outputs); /* ASSUMING EACH OUTPUT HAS EXACTLY ONE ARGUMENT, NAMELY MTH !! */
outputs_mths_lbls: apply (append, map (split_mth, outputs_mths));
/* NEXT ONE FAILS IF LAMBDA ARGUMENT IS E INSTEAD OF EQ !! LEXICAL SYMBOL FIXES IT !! */
outputs_tokens: map (lambda ([eq], lhs(eq) = tokens_from (rhs (eq))), outputs_mths_lbls);
myf: lambda ([l], block ([e], if errcatch (e: parse_token_list (l)) = [] then l else e));
outputs_expressions: map (lambda ([eq], lhs(eq) = myf (rhs (eq))), outputs_tokens);
strip_label (s) := parse_string (s);
outputs_expressions_labeled: map (lambda ([eq], ?mlabel (strip_label (first (lhs(eq))), rhs(eq))), outputs_expressions); /* ASSUMING LHS(E) = LBL()("(%O1) ") OR STLT !! */
for x in outputs_expressions_labeled do ?displa(x);
```

References
----------

[1] https://github.com/rpgoldman/xmls
