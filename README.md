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

## Pending tasks

This project is just an on-going progress: 

* :heavy_check_mark: Grammar is mostly finished
* :negative_squared_cross_mark: Lexer is incompletle
* :negative_squared_cross_mark: Tree builder and AST is mostly unimplemented 
* :negative_squared_cross_mark: Happy build times are slow which is cumbersome for development, I should check https://github.com/bjpop/language-python/issues/41. On the meanwhile, just comment grammar rules that are not being developed.  
