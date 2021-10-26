# lrpeg development

Contributers are very welcome of course!

## TODO

- More tests
- Better parse error information (now only the line and column is returned)
- Better documentation (you're reading it)
- Detect unreachable choices
- Profile and improve performance
- Create parser using `lrpeg` macro rather than `build.rs`

## lrpeg is self-hosting

The lrpeg parser definitions are parsed using lrpeg itself. The peg for this
is in `src/peg.peg`. You can rebuild the parser after changing `src/peg.peg`
with:

```
cargo run src/peg.peg > src/peg.rs.new
mv src/peg.rs.new src/peg.rs
```

## Roadmap

The aim for lrpeg is to become the new parser for
[solang](https://github.com/hyperledger-labs/solang), a solidity compiler
written in rust.