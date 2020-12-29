# Runscript

Runscript is a tool like `make` which manages run commands. Runscript can also handle building, but it doesn't track file update times (yet?), so you may still want to use make for that (but you can call it from runscript).

For an example of a useful runscript, see [the runfile for this repository](run).

## How to install

Binaries are available from the [releases](https://github.com/TheOnlyMrCat/runscript) page. Alternatively, you can use cargo to
build and install it. Supporting package managers is difficult, so I'm not going to try doing it until this gets real use.

### Cargo

If you have cargo, and are fine with building from source, runscript is on [crates.io](https://crates.io/crates/runscript)

```sh
cargo install runscript
```

## How it works

When `run` is executed, it looks for a file `run` in the current working directory (other files can be specified). It
searches this runfile for scripts, which look like the following:

```run
#target phase
commands
#/
```

`target` can be `-` for the default target, `#` for the global target, or anything else for a named target (matching the regex
`[A-Za-z0-9_]*`).

`phase` can be one of the following sets of characters:

| Text    | Phase         |
| ------: | :------------ |
|    `b!` | Build Only    |
|     `b` | Build         |
| (blank) | Build and Run |
|    `br` | Build and Run |
|     `r` | Run           |
|    `r!` | Run Only      |

The phases that are executed depend on flags passed to `run`. 

|         Phase | `-b`, `--build-only` | (blank), `--build-and-run` | `-r`, `--run-only` |
| ------------: | :------------------: | :------------------------: | :----------------: |
|    Build Only |          ✓           |                            |                    |
|         Build |          ✓           |             ✓              |                    |
| Build and Run |                      |             ✓              |                    |
|           Run |                      |             ✓              |         ✓          |
|      Run Only |                      |                            |         ✓          |

If multiple phase flags are passed, the one provided last is used.

`commands` is a newline-separated list of terminal commands to be executed. In other words, a shell script. Not all script
features are currently supported, however. A list of supported features follows:

* Commands and arguments
* Argument interpolation:
  * `*.run` glob matching (only `*`, `?`, `**`, `{}`)
  * `$VAR` environment variables
  * `$(echo)` internal subcommands
  * `$1` positional arguments
* Single and double-quoted arguments
* Command chaining by `&&`, `||`, and `|` (no `;`, just use newlines)
* `VAR=val` Environment variable setting (with interpolation)