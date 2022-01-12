# Runscript

Runscript is a tool like `make` which manages run commands. Runscript can also handle building, but it doesn't track file update times (yet?), so you may still want to use make for that (but you can call it from runscript).

For an example of a useful runscript, see [the runfile for this repository](run).

**Note: This readme is for runscript 2.0. See [the tree at v1.3.0](https://github.com/TheOnlyMrCat/runscript/tree/v1.3.0) for information on the latest stable release**

## Features

- Does **not** use Makefile syntax
- Supports most of the shell features you know and love (parsing by [conch_parser](https://github.com/ipetkov/conch-parser) currently)
- Multiple 'phases' per target, so you can chose to only build, build & run, or only run a target.
- Fancy output while running scripts

### To be implemented

- `source`ing external script files (such as `.env` files)
- A number of more advanced shell features

## How to install

Binaries are available from the [releases](https://github.com/TheOnlyMrCat/runscript) page. Alternatively, you can use cargo to
build and install it. Supporting package managers is difficult, so I'm not going to try doing it until this gets real use. (Feel
free to open an issue about it)

### Cargo

If you have cargo, and are fine with building from source, runscript is on [crates.io](https://crates.io/crates/runscript)

```sh
cargo install runscript
```

## License

Licensed under either of

- Apache License, Version 2.0, (LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license (LICENSE-MIT or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.