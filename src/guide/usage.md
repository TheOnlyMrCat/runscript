# Basic Usage

Once you have the `run` binary installed, you can execute runscripts with it.

## A basic runscript

The syntax of a runscript is inspired by the [TOML](https://toml.io) configuration language. Runscript looks for a file called
`run` or `.run` in the current directory, and any parent directory. Here is an example for executing a simple C program:

```run
# Saved as `run` in the current directory

[prog:build]
make

[prog:run]
::default-phase build run
./prog
```

There are two things of note here. First are the script headers: `[prog:build]` and `[prog:run]`. These define *targets* in
your runscript. Similar to `make`, the first target in a file is chosen as the default target, but unlike in `make`, targets
can have multiple *phases*. In this case (and in most cases), there is a `build` phase for building the target and a `run`
phase for running the target.

If the phase is omitted from a script header, e.g. `[prog]`, the phase is assumed to be `run`. You cannot define the same
`[target:phase]` twice in the same runscript.

We'll get to the `::default-phase` in a bit.

This runscript can be executed by simply executing `run` in the same directory as the runscript.

```ansi
$ run
\x1b[1;91mBuild\x1b[0m prog
> \x1b[1mmake\x1b[0m
make: `prog' is up to date
\x1b[1;94mRun\x1b[0m prog
> \x1b[1m./prog\x1b[0m
Hello, world!
```

You can also choose to only build or only run the target with flags: `run -b` or `run -r`.

Now, back to the `::default-phase build run`. This line tells Runscript to execute both the `build` and `run` phases
sequentially, by default. If this line were not included, the command `run` would only execute the `run` phase.

## Shell scripting

Runscripts support most basic shell features, such as parameter substitution, flow control, and background jobs.

Here's an example runscript which takes positional parameters:

```run
[target]
cargo run -q -- $@
```

Arguments can be passed to this script by separating them from the main command with a `--`:

```ansi
$ run -- Arguments
\x1b[1;94mRun\x1b[0m target
> \x1b[1mcargo\x1b[0m run -q -- \x1b[96mArguments\x1b[0m
Found 1 positional parameter
```

## Multiple targets

A runscript can, of course, define multiple targets. You could, for example, have a default target for testing your program,
and a separate target for packaging it for release.

In addition to this, any identifier can be used as a phase name, not just `build`, `run`, and `test`. Here's an example
runscript showcasing all of these features:

```run
[program:run]
cargo run -- $@

[program:bench]
cargo bench

[pkg]
cargo build --release
tar czf program.tar.gz -C target/release program
```

In this runscript, the three targets can be executed in the following ways:

```ansi
$ run -- Argument   \x1b[90m# Executes [program:run] with arguments {"Argument"}\x1b[0m
$ run program:bench \x1b[90m# Executes [program:bench]\x1b[0m
$ run pkg           \x1b[90m# Executes [pkg]\x1b[0m
```

## External script invocation

If Runscript's builtin shell is inadequate, you can tell the script to use a different shell, such as `bash` or
`zsh`. This is done by putting the path to the shell on the end of the header line:

```run
[external] /bin/bash
echo Foo
```

```ansi
$ run
\x1b[1;94mRun\x1b[0m external
Foo
```

This can be done on a runscript-wide basis with the `::default-shell` option:

```run
::default-shell /bin/bash

[external]
echo Foo
```
