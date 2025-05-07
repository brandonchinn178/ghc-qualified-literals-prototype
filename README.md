# GHC QualifiedLiterals prototype

Proposal: https://github.com/ghc-proposals/ghc-proposals/pull/698

## Quickstart

```shell
stack run example
cabal run example
```

## Limitations

The following are limitations of the prototype. These will be resolved when implementing in GHC.

* To simplify the parsing, to used qualified literals in a pattern, you must write it like `Foo.!123`
* Qualified multiline strings are not supported
* Qualified lists cannot contain expressions containing commas
