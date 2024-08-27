% Package `combining_diacritics`
% Robert Dodier
% Aug. 26, 2024

Package `combining_diacritics` implements a way to type in an ASCII-only name
and get a name with a diacritical mark (accents, etc) in return.
For example, to type in `yhat` and get `ŷ`.
From that point forward, `yhat` is forgotten and only `ŷ` exists in the Maxima session.

The diacritical marks are Unicode combining characters.
The name of the symbol `ŷ` actually comprises two characters, `y` and the character known as `#\COMBINING_CIRCUMFLEX_ACCENT`.
At present (August 2024), four diacritics are defined:
circumflex, centered dot above, macron (short overline), and right arrow above.
Each diacritic is represented as 1-character string,
namely `combining_circumflex`, `combining_dot`, `combining_macron`, and `combining_arrow`.

The function `define_input_alias` defines an input alias,
taking three arguments:

 * `y`: symbol for the alias (this is the symbol which will appear in input)
 * `x`: symbol to which the diacritic will be applied
 * `diacritic`: one of the diacritic strings mentioned above

E.g. `define_input_alias(yhat, y, combining_circumflex)` defines `yhat` as an input alias for `ŷ`.

Example session:
```{maxima}
(%i2) load ("combining_diacritics.lisp") $
(%i3) stringdisp: true $
(%i4) [combining_circumflex, combining_dot, combining_macron, combining_arrow];
(%o4)                 ["̂", "̇", "̄", "⃗"]
(%i5) define_input_alias (yhat, y, combining_circumflex);
(%o5)                          ŷ
(%i6) define_input_alias (xbar, x, combining_macron);
(%o6)                          x̄
(%i7) define_input_alias (zdot, z, combining_dot);
(%o7)                          ż
(%i8) define_input_alias (rarrow, r, combining_arrow);
(%o8)                          r⃗
(%i9) yhat + xbar + zdot + rarrow;
(%o9)                   ż + ŷ + x̄ + r⃗
(%i10) yhat: 123;
(%o10)                         123
(%i11) xbar: 999;
(%o11)                         999
(%i12) ''%o9;
(%o12)                   ż + r⃗ + 1122
(%i13) rarrow[n] - rarrow[1]^2;
                                    2
(%o13)                      r⃗  - r⃗
                              n     1
```
This last expression (%o13) illustrates a problem with the Maxima pretty printer console interface.
The placement of elements within the display is determined from how many characters are in a symbol's name,
not how much space the symbol actually occupies.
For that reason, the superscript and subscripts are placed too far away from the symbols to which they pertain.

Note that this scheme makes the input alias ephemeral and the symbol which has a name containing diacritics is permanent.
It's also possible to preserve the input symbol throughout, and only change the way it is displayed to use diacritics.
At this point, I believe it is too early to tell which scheme is more generally useful.

This package requires a Unicode-aware Lisp.
SBCL, ECL, Clozure CL, and CMUCL are Unicode-aware,
and I believe this package will work with them.
GCL is not Unicode-aware (as of GCL 2.6.14) so the package cannot work.
