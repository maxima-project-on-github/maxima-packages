### `json_tools`

`json_tools` is a package to read and work with JSON data in Maxima.

This package is experimental and subject to change.

In order to load the package, either
(1) one needs to launch Maxima from the `json_tools` folder,
or (2) `push("path/to/json_tools", file_search_maxima);`

Then `load(json_tools);` loads it.

#### Reading JSON data

`read_json("mydata.json");` parses `"mydata.json"`.

The curly braces `{ ... }` are replaced by `blob(...)`
and the colon `:` is replaced by `=`,
to avoid confusion with existing meaning of `{ ... }` and `:` in Maxima.

Keys remain strings; they are not turned into symbols by `read_json`.

Note that the input can also be a stream instead of a file,
e.g.: `S: openr("mydata.json"); read_json(S);`

#### Blob query operations

The `//` operator selects the value of a key,
e.g.: `blob("foo" = 123) // "foo"` yields `123`.

When the left-hand side of `//` is a list,
then `//` is mapped over the elements of the list,
e.g.: `[blob("aa" = 11), blob("aa" = 22), blob("aa" = 33)] // "aa"` yields `[11, 22, 33]`.

#### Flattening blobs

`flatten_json` turns blobs into `foo.bar.baz = something`.
That might or might not be useful; anyway there it is.

E.g.: `flatten_json([foo, bar], blob("aa" = blob("bb" = 123)));`
yields `foo . bar . aa . bb = 123`
Here the keys have been turned into symbols.

#### Writing blobs as JSON

There is not yet a way to write blobs as JSON.
I'll come up with something soon.
