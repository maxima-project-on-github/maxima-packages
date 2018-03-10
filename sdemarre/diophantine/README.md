Maxima program to solve diophantine equations of the form ax^2+bxy+cy^2+dx+ey+f=0 with a,b,c,d,e,f constant integers.
Based on Dario Alpern's solution/code found at https://www.alpertron.com.ar/QUAD.HTM

[Installing](README.md#install)
[Usage](README.md#usage)
[Limitations](README.md#limitations)
[Tests](README.md#tests)
[Graphical examples](README.md#graphical-examples)

## Install
Put the source in some folder, and in maxima do the following:

```
	(%i1) diophantine_source_dir:"<some-folder>"$
	(%i2) push(sconcat(diophantine_source_dir, "$$$.mac"), file_search_maxima)$
	(%i3) push(sconcat(diophantine_source_dir, "$$$.lisp"), file_search_lisp)$
```

Putting these lines in your maxima-init.mac saves you from having to type this in every new maxima session.

Alternatively, if you're using Robert Dodier's [asdf loader](https://github.com/robert-dodier/maxima-asdf), you can do the following:

```
	(%i1) install_github("sdemarre", "diophantine", "master")$
	(%i2) asdf_load_source("diophantine")$
```

## Usage

```
	(%i1) load(diophantine)$

	(%i2) diophantine_solve(-9*x+11*y=5);
	(%o2) [[x = 11*%z1 - 3,y = 9*%z1 - 2]]
```

The result is always a (possibly empty) list of solutions. When there are infinitely many solutions, solutions use a parameter %z1 or %n1, which means "any integer" or "any natural number" respectively. Some equations have several sets of those, e.g.

```
	(%i3) diophantine_solve(18*y^2-24*x*y+7*y+8*x^2+5*x+16);
	(%o3) [[x = (-174*%z1^2)+17*%z1-2,y = (-116*%z1^2)+21*%z1-2],
	       [x = (-174*%z1^2)+41*%z1-4,y = (-116*%z1^2)+37*%z1-4]]
```

Generating some specific values when the solution contains such a parameter can be done like this

```
	(%i4) diophantine_solve(3*x+2*y-8);
	(%o4) [[x=2*%z1+2,y=1-3*%z1]]

	(%i5) diophantine_instantiate_solutions(%o4,-3,3);
	(%o5) [[x = - 4, y = 10], [x = - 2, y = 7], [x = 0, y = 4], [x = 2, y = 1],
	      	  [x = 4, y = - 2], [x = 6, y = - 5], [x = 8, y = - 8]]
```

## Limitations

The test file (rtest_diophantine.mac) contains possibly interesting examples and examples that show the following issues:
* Even apparently simple equations can take a very long time to solve (one of the reasons is trying to factor large integers).

## Tests

There is a file with tests available, run it like this:

```
	(%i6) batch(rtest_diophantine, test);
```

## Graphical examples:

```
	(%i7) load(diophantine_draw)$
	(%i8) diophantine_draw_example();
```

![Graphical example](diophantine_draw_example.png)

```
	(%i9) diophantine_draw_example2();
```

![Graphical example 2](diophantine_draw_example2.png)

```
	(%i10) diophantine_draw_example3();
```

![Graphical example 3](diophantine_draw_example3.png)

```
	(%i11) diophantine_draw_example4();
```

![Graphical example 4](diophantine_draw_example4.png)

In general, you can use diophantine_draw() to draw the equation with (some of) its solutions:

```
	(%i17) diophantine_draw(x^2-7*y^2=-3);
```
