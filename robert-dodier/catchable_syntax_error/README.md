## About catchable\_syntax\_error

This is an alternative implementation of the internal Lisp function
MREAD-SYNERR, which is called by the Maxima parser to handle syntax
errors. This implementation allows `errcatch` to catch syntax errors,
while the default implementation of MREAD-SYNERR does not allow
`errcatch` to catch syntax errors.

This is just one possible implementation. It has the defect that it
changes MREAD-SYNERR to not output line number information when a
syntax error is detected. It seems plausible that the line number
info could be preserved, but I couldn't figure it out.

### Examples

Here's an example. The file `foo.mac` contains a syntax error:
`2 x;` instead of `2*x;`

```
(%i1) load ("foo.mac");
incorrect syntax: x is not an infix operator
2 x;
  ^
(%i2) 
```

OK, I'll try to catch the error with `errcatch`.

```
(%i2) errcatch (load ("foo.mac"));
incorrect syntax: x is not an infix operator
2 x;
  ^
(%i3) 
```

Rats! The syntax error has bypassed `errcatch` and gone straight back
to the input prompt; I was hoping that `errcatch` would return `[]` as
it always does when it catches an error.

Now try it with the new implementation of MREAD-SYNERR. `errcatch`
catches the syntax error.

```
(%i3) load ("catchable_syntax_error.lisp") $
(%i4) errcatch (load ("foo.mac"));
syntax error: x is not an infix operator
(%o4)                          []
```

I'll use the catchable syntax error to create a function `safe_load` to
return `OOPS(<filename>)` to indicate an error.

```
(%i5) safe_load (f) := (errcatch (load (f)), if %% = [] then OOPS(f) else first(%%));
(%o5) safe_load(f) := (errcatch(load(f)), 
                          if %% = [] then OOPS(f) else first(%%))
(%i6) safe_load ("foo.mac");
syntax error: x is not an infix operator
(%o6)                  OOPS(foo.mac)
```

Syntax errors can be caused by `parse_string` (and also `eval_string`)
and `errcatch` catches them.

```
(%i7) errcatch (parse_string ("foo bar"));
syntax error: bar is not an infix operator
(%o7)                          []
```

After a syntax error is detected (by any function), the global variable
`error` contains the error message (which is also printed on the console
when the error occurs).

```
(%i8) error;
(%o8)     [syntax error: bar is not an infix operator]
```

These examples were generated with Maxima 5.43.0. I think it this should
work with any version which is not too old, but I didn't specifically
test to see which versions work.
