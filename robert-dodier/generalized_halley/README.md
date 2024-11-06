### Summary

This package, `generalized_halley`, contains functions to construct an iterator
for a generalized Halley method to solve multidimensional nonlinear equations.

This package only constructs the iterator; establishing a stopping criterion 
and calling the iterator is left to the caller.

Code here follows the approach outlined by Gundersen and Steihaug. [1]

[1] Geir Gundersen and Trond Steihaug.
"On large scale unconstrained optimization problems and higher order methods."
Optimization Methods & Software, vol. 25, issue 3 (June 2010), pp 337--358.
https://doi.org/10.1080/10556780903239071
Retrieved from: https://optimization-online.org/wp-content/uploads/2007/03/1610.pdf

As described by Gundersen and Steihaug,
the generalized Halley method is parametrized by a constant, namely `α`.
Different values of `α` yield some methods known by name:
`α = 0` yields Chebyshev's method,
`α = 1/2` yields Halley's method,
and `α = 1` yields the super-Halley method.

### Usage

Load `generalized_halley` and construct an iterator for an example problem.
Here `α = 1/2`, so this implements Halley's method.

```{maxima}
(%i1) load ("generalized_halley.mac") $

(%i2) F: [2*3^u - v/u - 5, u + 2^v - 4];
                             v      u       v
(%o2)                     [- ─ + 2 3  - 5, 2  + u - 4]
                             u
(%i3) halley_iterator: construct_generalized_halley_update (F, [u, v], 1/2);
(%o3) lambda([x_k], block([u, v], [u, v] : x_k, 
                          1     v      u       v
generalized_halley_update(─, [- ─ + 2 3  - 5, 2  + u - 4], 
                          2     u
                                 ┌                                          ┐
┌                             ┐  │       2     u   2 v           1          │
│ v              u       1    │  │ [2 log (3) 3  - ───, 0]      [──, 0]     │
│ ── + 2 log(3) 3      - ─    │  │                  3             2         │
│  2                     u    │  │                 u             u          │
│ u                           │, │                                          │, 
│                             │  │          1                      2     v  │
│                           v │  │         [──, 0]          [0, log (2) 2 ] │
│        1          log(2) 2  │  │           2                              │
└                             ┘  │          u                               │
                                 └                                          ┘
x_k)))
```

Use the iterator to generate successive estimates of a solution.
The initial point is `[2.0, 2.0]`.

```{maxima}
(%i4) ev (halley_iterator ([2.0, 2.0]), numer);
(%o4)               [1.1117228381556121, 1.4849774563022784]
(%i5) ev (halley_iterator (%), numer);
(%o5)               [1.0664132421723913, 1.5522505012801135]
(%i6) ev (halley_iterator (%), numer);
(%o6)               [1.0666183918166123, 1.5525647566467795]
(%i7) ev (halley_iterator (%), numer);
(%o7)               [1.0666183895954067, 1.5525647668417866]
(%i8) ev (halley_iterator (%), numer);
(%o8)               [1.0666183895954067, 1.5525647668417863]
```

Just for fun,
let's look at the code that's generated automatically by differentiating `F` twice.
If we were going to generate code for Python, C/C++, Fortran, etc.,
this is the function that we would translate.

```{maxima}
(%i9) grind (halley_iterator);

lambda([x_k],
       block([u,v],[u,v]:x_k,
             generalized_halley_update(1/2,[-(v/u)+2*3^u-5,2^v+u-4],
                                       matrix([v/u^2+2*log(3)*3^u,-(1/u)],
                                              [1,log(2)*2^v]),
                                       matrix(
                                        [[2*log(3)^2*3^u-(2*v)/u^3,0],
                                         [1/u^2,0]],
                                        [[1/u^2,0],[0,log(2)^2*2^v]]),x_k)))$
(%o9)                                 done
```

The function `generalized_halley_update` only carries out linear algebra calculations;
no further symbolic operations are needed in the iterator.
The symbolic functions such as differentiation are only needed to construct the iterator,
and aren't needed for the numerical solution.
