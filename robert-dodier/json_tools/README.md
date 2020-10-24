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
e.g.: 
```{maxima}
x: blob("foo" = 123);
x // "foo";
```
yields `123`.

`//` can be applied successively,
e.g.:
```{maxima}
x: blob("foo" = blob("bar" = blob("baz" = 456)));
x // "foo" // "bar" // "baz";
```
yields `456`.

When the left-hand side of `//` is a list,
then `//` is mapped over the elements of the list,
e.g.: 
```{maxima}
x: [blob("aa" = 11), blob("aa" = 22), blob("aa" = 33)];
x // "aa";
```
yields `[11, 22, 33]`.

When there is only one key, the result is just the value.
Multiple keys in a list on the right-hand side
yield a result which is again a blob, having just the selected keys,
e.g.:
```{maxima}
x: blob("aa" = 11, "bb" = 22, "cc" = 33, "dd" = 44);
x // ["bb", "dd"];
```
yields `blob("bb" = 22, "dd" = 44)`.

#### Flattening blobs

`flatten_json` turns blobs into `foo.bar.baz = something`.
That might or might not be useful; anyway there it is.

E.g.: 
```{maxima}
x: blob("aa" = blob("bb" = 123));
flatten_json([foo, bar], x);
```
yields `foo . bar . aa . bb = 123`
Here the keys have been turned into symbols.

The first argument `[foo, bar]` are some additional symbols
to put in front of any derived from the blobs.

#### Writing blobs as JSON

There is not yet a way to write blobs as JSON.
I'll come up with something soon.
