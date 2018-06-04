# Changes

All significant changes to this project will be documented in the notes below.

## 2018-06-04

### Added

-   `tex_no_math_delimiters` style for no math delimeters or labels.
-   Support for big floats.

### Changes

-   The functions `texify` and `texify_inline` are now backward compatible with
    Maxima's function `tex`.

### Fixed

-   Remove unneeded math delimiters in LaTeX `mlabel` format.
-   Avoid using `mathop` in Euler notation for correct placement of subscript.
-   Wrapping with parenthesis is now done based on normalized expression. This
    fixes some issues with `mrat`.
-   Digit extraction from symbols like `a0` or `a_1_2` is now done by function
    included in texify, not `extract-trailing-digits` from  `mactex.lisp`. This
    avoids issues in loading order.

## 2017-05-30

Initial release
