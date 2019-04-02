language-ruby
============

Haskell parser for the Ruby 2.7 Language. Project structure is loosly based https://github.com/erikd/language-javascript, lexer is based on https://github.com/bjpop/language-python and parser is based on https://github.com/whitequark/parser.

## Building

```bash
$ stack build
```

## Running tests

```bash
$ stack test --file-watch --fast
```

## Debugging Grammar

```
$ happy src/Parser.y -dai
```

## Nodes naming

AST Nodes naming is the same as [whitequark/parser](https://github.com/whitequark/parser/blob/master/doc/AST_FORMAT.md)'s, with the following notable exceptions and changes:

* `CamelCase` is used instead of `snake_case`. E.g.:
   * `(Str "foo")` instead of `s(:str, "foo")`
   * `(BackRef "$&")` instead of `s(:back_ref, :$&)`
* Special `__` variables have being completly renamed and are not being processed:
  * `File` instead of `__FILE__`
  * `Encoding` instead of `__ENCODING__`
  * `Line` instead of `__LINE__`
* Varargs are replaced by lists:
  * `(Begin [RInt 4, RInt 5])` instead of `s(:begin, s(:int, 4), s(:int, 5))`
* Ruby datatypes are prefixed with an in order to avoid collisions. E.g.:
  * `RInt` instead of `int`
  * `RFloat` instead of `float`

## Pending tasks

This project is just an on-going progress:

* :heavy_check_mark: Grammar is mostly finished
* :negative_squared_cross_mark: Lexer is incompletle
* :negative_squared_cross_mark: Tree builder and AST is mostly unimplemented
* :negative_squared_cross_mark: Happy build times are slow which is cumbersome for development, I should check https://github.com/bjpop/language-python/issues/41. On the meanwhile, just comment grammar rules that are not being developed.
