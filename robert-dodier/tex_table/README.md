### tex\_table

`tex_table` is a function to generate LaTeX output for a table,
as defined by `defstruct(table(header, rows, outline, hsep, vsep))`.

 * `header` is a list comprising the labels to put on the top of each column
 * `rows` is a list of lists, one for each row of the table. All rows must have the same length.
 * `outline` is a Boolean value, which tells whether the table is outlined
 * `hsep` is a Boolean value, which tells whether the table has horizontal separators
 * `vsep` is a Boolean value, which tells whether the table has vertical separators

Example:
Let `t` be a table, e.g. `t: table([a, b, c], [[1, 2, 3], [4, 5, 6], [7, 8, 9]], false, true, true)`.

 * `t@header` is `[a, b, c]`
 * `t@rows` is `[[1, 2, 3], [4, 5, 6], [7, 8, 9]]`
 * `t@outline` is `false`
 * `t@hsep` is `true`
 * `t@vsep` is `true`

Table instances such as `t` are displayed by Maxima's 2-d pretty printer
as a matrix in which the header is pasted onto the rows of the table.
No attempt is made to honor the Boolean options.

Given `t`, then `tex_table(t)` returns a string containing LaTeX `\begin{tabular} ... \end{tabular}`,
and `print(tex_table(t))` prints the LaTeX output.

The script `tex_table_example.mac` shows an example of constructing a
LaTeX document containing a collection of tables with different options.
In the example, `tex_table` output is wrapped in `\begin{table} ... \end{table}`
so that a caption can be associated with the table.

Although it would be possible to associate `tex_table` with `table` via `texput`,
so that `tex_table` would be called automatically from `tex`,
I chose not to do that, because that introduces messy hassles about the TeX environment
(equation versus table).
