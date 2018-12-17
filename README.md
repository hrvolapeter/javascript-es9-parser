# Javascript parser

[![Build Status](https://travis-ci.com/retep007/javascript-es9-parser.svg?branch=master)](https://travis-ci.com/retep007/javascript-es9-parser)
[![Coverage Status](https://coveralls.io/repos/github/retep007/javascript-es9-parser/badge.svg?branch=master)](https://coveralls.io/github/retep007/javascript-es9-parser?branch=master)

This project introduces javascript parser written in Rust-lang.

The project is currently under development. API is not stable and [ECMA 2018](http://www.ecma-international.org/ecma-262/9.0/index.html#sec-statements) is not fully supported yet. However, it may be satissfactory for simple use cases.

## Documentation
Documentation of public API [here](https://retep007.github.io/javascript-es9-parser/)
Documentation of implementaiton also with explanation can be found in specific files

## Development
### Build
You need nightly version of rust compiler. Project contains cargo package manager. Whole library can be build
with `cargo build --release`. Recommended setup is through [rustup](https://rustup.rs/) installer.
### Testing
Tests inlcuding intergration tests can be run with `cargo test --all`.
### Benchmarking
For running available benchmarks use `cargo bench`