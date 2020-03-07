Solver for systems of linear diophantine equations.
At the core of the algorithm is the computation of the smith normal form of the system matrix.
We can describe the system as A.x = b, with all entries of A, b and x integers.
The smith normal S has the property that A = U . S . V', S a diagonal matrix, which allows us to easily solve the original system.

* _smith_normal_form.mac_
maxima implementation of smith normal form

how to use:
put the _smith_normal_form.mac_ in a folder that's in your _file_search_maxima_ path, or extend _file_search_maxima_ with the path where you have put the .mac file.
```
(%i1) load(smith_normal_form);
(%o1)     smith_normal_form.mac

(%i2) smith_normal_form(matrix([1,2,3,4],[5,6,7,8]));

                                               [ 1  1  0  0 ]
                                               [            ]
                 [ 1   0  ]  [ 1   0   0  0 ]  [ 2  1  0  0 ]
(%o2)           [[        ], [              ], [            ]]
                 [ 1  - 1 ]  [ 0  - 4  0  0 ]  [ 3  1  1  0 ]
                                               [            ]
                                               [ 4  1  0  1 ]

```

* _diophantine_system.mac_
maxima implementation of a solver for systems of linear diophantine equations.
how to use:
put the _diophantine_system.mac_ in a folder that's in your _file_search_maxima_ path, or extend _file_search_maxima_ with the path where you have put the .mac file.
```
(%i1) load(diophantine_system);
(%o1)     diophantine_system

(%i2) eq: [(-8*x5)+22*x4+4*x2+3*x1 = 25,
           (-12*x5)+46*x4+6*x1 = 2,
		   9*x5-x4+3*x3+4*x2 = 26]$

(%i3) solve_diophantine_system(eq);
(%o3) [x1 = 92 %z2 + 3444 %z1 - 617,
       x2 = (- 3 %z2) - 114 %z1 + 25,
       x4 = (- 12 %z2) - 450 %z1 + 80,
	   x5 = (- 3 %z1) - 2,
	   x3 = 11 %z1 + 8]

(%i4) %o3,%z1=0,%z2=0;
(%o4) [x1 = - 617, x2 = 25, x4 = 80, x5 = - 2, x3 = 8]

(%i5) eq,%o3,ratsimp;
(%o5) [25=25, 2=2, 26=26]
```

