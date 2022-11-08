# Runscript

Runscript is a tool like `make` (or, perhaps, [`just`](https://github.com/casey/just)) which manages project-specific
commands. When you want to run your program, simply type `run`.

For an example of a useful runscript, see [the runfile for this repository](run).

## Features

- Does **not** use Makefile syntax
- Emulates a shell and executes commands directly, instead of invoking `sh`.
- Supports most of the shell features you know and love (parsing adapted from [ipetkov/conch_parser](https://github.com/ipetkov/conch-parser))
- Can invoke an external tool (not necessarily a shell!) to run a script, if the buildin shell proves inadequate.
- Multiple 'phases' per target, so you can chose to build, run, or test a target. If those options aren't enough, you can name your phases whatever you want!
- Imports targets from multiple files, meaning you can have a personal `.run` file alongside a source-controlled `run` file
- Fancy output while running scripts

### Still to do

- A number of more advanced shell features, including:
  - A few shell builtin commands (e.g. `exit`, `nohup`)
  - Local variables (?)
  - Arithmetic and a number of parameter substitutions
  - Here-documents
- Replace manual SIGHUP'ing with `setpgid`
- Remove most sources of panicking (fuzz testing?)
- Support Windows properly (?)

### Non-goals

Despite having `-c` and `-s` options for executing single commands and shell scripts, respectively, Runscript
won't emulate a POSIX shell perfectly. I'll try to make it useful enough for most use cases, but if you want a
POSIX shell, just use a POSIX shell. That being said, feel free to test the limitations of those two flags! I do have
to be able to execute shell scripts to implement the `source` builtin.

## How to install

```sh
git clone https://git.sr.ht/~theonlymrcat/runscript
cd runscript
cargo install --release
```

Supporting package managers is difficult, so I'm not going to try doing it until this gets real use. (Feel free to send an email about it)

## License

Licensed under the Apache License, Version 2.0 (<http://www.apache.org/licenses/LICENSE-2.0>).

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be licensed as above, without any additional terms or conditions.
