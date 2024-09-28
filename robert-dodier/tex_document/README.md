% Maxima package tex\_document

`tex_document` is a Maxima package to generate a simple LaTeX document from a Maxima batch script:

 + Stand-alone comments become text sections
 + Expressions in the script become \\verbatim sections
 + Results of evaluation (displayed only if an expression is terminated by semicolon) become typeset equations

Example using `foo\_bar.mac` which is in this folder.

```{maxima}
load ("tex_document.mac");

tex_document ("foo_bar.mac", "tmp-foo_bar.tex");
```

