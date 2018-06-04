# texify

`texify` is a Maxima package to produce plain TeX or LaTeX representations
of Maxima expressions. The produced TeX/LaTeX representation can be customized
using various cascading styles.

## Usage

From Maxima `texify` should be loaded with the following.

    (%i1) load("texify.lisp")$

Once `texify` has been loaded Maxima expressions can be converted to TeX or
LaTeX by using the `texify` function. For example, using the default styles
produces the following Leibniz's notation for derivatives.

    (%i1) load("texify.lisp")$
    (%i2) texify(diff(f(x),x));
    \[\frac{d}{\mathop{dx}}f\left(x\right)\]
    (%o2)                                false

Additional styles may be globally added using `texify_add_styles`, globally
removed using `texify_remove_styles` or temporarily added by including in a call
to `texify`. The current global styles are stored in the variable
`texify_styles`. By default this variable is set to produce LaTeX code. To
produce plain TeX the `latex` style needs to be removed. For instance, the
following will produce plain TeX and override the default Leibniz derivative
style with Lagrange's notional for one expression.

    (%i1) load("texify.lisp")$
    (%i2) texify_styles;
    (%o2)                 [latex, tex_prefix_functions, tex]
    (%i3) texify_remove_styles(latex);
    (%o3)                     [tex_prefix_functions, tex]
    (%i4) texify(diff(f(x),x))$
    $${d}\over{\mathop{d{x}}}{f}\left({x}\right)$$
    (%i5) texify(diff(f(x),x), true, tex_diff_lagrange)$
    $${f}'\left({x}\right)$$

## Functions

-   `texify(expression, destination, style, ...)` — Convert expression to
    TeX/LaTeX using styles listed in `texify_styles` variable prefixed by any
    styles explicitly specified. `expression` and `destination` follow the same
    semantics as Maxima's `tex` function. Specifically, a label specified in
    `expression` will result in the value of that label being used. A value of
    `true` in `destination` will result in the output being sent to
    `*standard-output*`, whereas a value of `false` will return a string. If
    `destination` is a stream then the result will be written to that stream.
    Finally, if `destination` is a string the output will appended to a file of
    that name and will be created if needed.

-   `texify_inline(expression, destination, style, ...)` — Convert
    expression to _inline_ TeX/LaTeX using styles listed in `texify_styles`
    variable prefixed by any styles explicitly specified and using the sample
    semantics as `texify`.

-   `texify_available_styles()` — Return all available styles as a list.

-   `texify_add_styles(style, ...)` — Add specified styles to global
    `texify_styles` list.

-   `texify_remove_styles(style, ...)` — Remove specified styles from
    global `texify_styles` list.

## Styles

-   `tex` — Base TeX style must always be the last style in
    `texify_styles`.

-   `tex_inv_trig_herschel` — Use Herschel's notation for the inverse
    trignometric functions.

-   `tex_diff_upright_d` — Use an upright "d" in Leibniz's or Euler's
    derivative notations. If using Euler's derivative notation then
    `tex_diff_euler` should also be specified.

-   `tex_no_label` — Do not display a label even if one is present.

-   `tex_eq_number` — Number equations using default TeX/LaTeX method versus
    labelling with specified output label.

-   `tex_no_math_delimiters` — Do not wrap output in math delimeters.

-   `tex_pmatrix` — Display matrices using parenthesis versus the default
    which is to use brackets.

-   `tex_diff_lagrange` — Display single variable derivatives using
    Lagrange's notation.

-   `tex_diff_newton` — Display time derivatives using Newton's notation.

-   `tex_diff_euler` — Display derivatives using Euler's notation.

-   `tex_int_euler` — Display integrals using Euler's notation.

-   `tex_prefix_functions` — Display trigonometric and exponential functions
    using prefix notation. Parenthesis will be dropped if possible and positive
    exponents will be displayed next to the function name.

-   `latex` — Produce LaTeX code versus TeX code. The `tex` style still
    needs to be included in `texify_styles`.

-   `latex_diff_upright_d` — Use upright a "d" in Leibniz's and Euler's
    notation.

-   `amsmath` — Use appropriate styles for AMS-LaTeX package `amsmath`.

-   `amsmath_pmatrix` — Display matrices using parenthesis (`pmatrix`
    environment) versus the default which is to use brackets.

-   `breqn` — Use the `breqn` math environments for automatic equation
    breaking.

-   `mathtools` — Use `coloneqq` and related macros from the `mathtools`
    package for Maxima's define, assign, etc.

-   `nicefrac` — Use slash fractions for inline math and units from the
    `ezunits` package.

-   `siunitx` — Use the `siunitx` package to typeset units from `ezunits`.
