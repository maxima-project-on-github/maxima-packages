### Summary

This package, chebyshev\_primality, contains functions to experiment with a method
of testing primality via Chebyshev polynomials.

### Usage

```{maxima}
load ("/home/robert/.maxima/mixima/mixima.mac");
load ("chebyshev_primality.mac");
makelist (isprime (2^n + 1), n, 1, 20);
```

### Original Mathematica version

The original version of this code was written for Mathematica by Mamuka Jibladze.
See: https://mathoverflow.net/questions/286304/chebyshev-polynomials-of-the-first-kind-and-primality-testing
and: https://mathoverflow.net/users/41291/%e1%83%9b%e1%83%90%e1%83%9b%e1%83%a3%e1%83%99%e1%83%90-%e1%83%af%e1%83%98%e1%83%91%e1%83%9a%e1%83%90%e1%83%ab%e1%83%94

Copied here under terms of the Creative Commons Share-Alike license
(https://creativecommons.org/licenses/by-sa/4.0/)
as specified by the Stack Overflow Public Network Terms of Service
(https://stackoverflow.com/legal/terms-of-service#licensing).

### Maxima translation

This translation to Maxima was carried out by the Mixima translator,
by John Lapeyre, with minor modifications by Robert Dodier;
see: https://github.com/maxima-project-on-github/mixima
As a derived work of the original Mathemtica version,
this translation is covered by the same license (namely CC-SA).

```{maxima}
load ("/home/robert/.maxima/mixima/mixima.mac");
with_stdout ("chebyshev_primality.mac",
  miximaTransFile("chebyshev_primality.m"));
```

The function `miximaTransFile` prints the Maxima translation
of the Mathematica file `chebyshev\_primality.m"`
and I collected that output to a file via `with_stdout`.
I then loaded it into Maxima via `batch`,
and reformatted it in a manner which is slightly easier to read via `grind`
(these additional steps are not shown above).
The output of `grind` is what is found in `chebyshev\_primality.mac`.
