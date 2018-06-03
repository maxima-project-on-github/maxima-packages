# Changes

All significant changes to this project will be documented in the notes below.
This project adheres to [Semantic Versioning](http://semver.org/).

## [v0.2.0][] -

### Fixed

-   Wrapping with parenthesis is now done based on normalized expression. This
    fixes some issues with `mrat`.
-   Digit extraction from symbols like `a0` or `a_1_2` is now done by function
    included in texify, not `extract-trailing-digits` from  `mactex.lisp`. This
    avoids issues in loading order.

## [v0.1.0][] - 2017-05-30

Initial release
