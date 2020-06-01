`complex_plots.mac` -- plot argument and magnitude via Gnuplot

Template files adapted from examples at: `http://gnuplot.sourceforge.net/demo/complex_trig.html`
slurp.lisp adapted from: `http://sodaware.sdf.org/notes/cl-read-file-into-string/`

`complex_plots.mac` copyright 2020 by Robert Dodier
I release this work under terms of the GNU General Public License.

### LIMITATIONS

Current working directory when Maxima is launched must be the `complex_plots` directory.

It is necessary to use 'batch' to read this file, not 'load'.
This is due to the presence of :lisp in this file.

Functions are specified in terms of `x + %i*y`, not a single variable z.

Range is fixed at (-pi/2, pi/2) in x and in y.

Some Maxima functions don't have an equivalent in Gnuplot,
so attempting to plot those will cause an error in Gnuplot.

Maxima doesn't clean up temporary files from complex plots upon program termination.

All of this is really a proof of concept, no doubt there's more work to do.

### EXAMPLES

```
batch ("complex_plots.mac");
complex_plot_2d (atanh (x + %i*y));
complex_plot_3d (atanh (x + %i*y));
complex_plot_2d (2^(x + %i*y));
complex_plot_3d (2^(x + %i*y));
complex_plot_2d ((x + %i*y)^(5/2));
complex_plot_3d ((x + %i*y)^(5/2));
```

