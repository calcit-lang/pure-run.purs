## Toy interpreter

> ...on top of PureScript. It's using the `compact.cirru` from calcit-editor.

### Highlights

- trying to follow basic APIs from http://apis.calcit-lang.org/
- to be different, operands in expression like `map ([] 1 2 3) inc` will be placed at first.
  Calcit does use curried functions, so maybe we should not follow Haskell.

### Usages

```bash
yarn global add @calcit/pure-run

pure-run compact.cirru
```

Notice that `compact.cirru` is generated from [calcit-editor](https://github.com/Cirru/calcit-editor). It could also be written manually by following the format. Find an example in [`example/compact.cirru`](example/compact.cirru).

Features:

- some basic math, string, array.
- tail recursion with `recur`.
- some hygienic macros.

### Bundler usages

With help of bundle command, you may write `main.cirru` just like:

```cirru
ns demo.main
  :require
    demo.lib :as lib

defn main ()
  echo "|main file..."
  lib/f1
  echo lib/lib-x
```

with other files under `src/`:

```bash
=>> tree
.
├── bundled.cirru # <-- generated file like compact.cirru
└── src
    └── demo
        ├── lib.cirru
        └── main.cirru
```

then use the command to combine into a single snapshot:

```bash
from=src to=bundled.cirru pure-run-bundle
```

The CLI really dumb and it detects names by reading Cirru tree. Be careful to use it. And you may also need to fix `init-fn` in the `configs` field.

### Development

Dependencies: PureScript, spago, nodejs, yarn.

In dev mode:

```bash
yarn r # to run spago to call Main function

yarn build # to build bundle into dist/index.js
./bin/command.js example/compact.cirru # run bundled script

spago test # test running
```

### License

MIT
