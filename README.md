# ff4s-canvas :chart_with_upwards_trend:

Create [canvas](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API)-based interactive charts the purely functional way.

This is an experimental companion library to [ff4s](https://www.github.com/buntec/ff4s).

Currently only supports Scala 3 (3.10 or newer, like ff4s 0.27).

```scala
libraryDependencies += "io.github.buntec" %%% "ff4s-canvas" % "0.1.0"
```

## Examples

The examples are published at https://buntec.github.io/ff4s-canvas/.

To run them locally (requires [just](https://github.com/casey/just), sbt and Node.js):

```sh
just install          # install Vite
just watch-examples   # recompile the examples on every change
just serve-examples   # in another terminal: serve them at http://127.0.0.1:8080/
```

## Development

Run the tests with `just test` (or `sbt test`).
