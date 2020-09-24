# Runscript

Runscript is a tool like `make` which manages run commands. Runscript can also handle building, but it doesn't track file update times (yet?), so you may still want to use make for that (but you can call it from runscript).

## How to install

Binaries are available from the [releases](https://github.com/TheOnlyMrCat/runscript) page. Alternatively, you can use cargo to
build and install it. Supporting package managers is difficult, so I'm not going to try doing it until this gets real use.

### Cargo

If you have cargo, and are fine with building from source, runscript is on [crates.io](https://crates.io/crates/runscript)

```sh
cargo install runscript
```

## How it works

### Phases

Runscripts can be divided into 5 phases:

1. Build Only
2. Build
3. Build and Run
4. Run
5. Run Only

Of course, not all phases need to be used. Up to three of these phases will be run on one `run` invocation, depending on the arguments passed to the command:

|         Phase | `-b`, `--build-only` | `--build-and-run` | `-r`, `--run-only` |
| ------------: | :------------------: | :---------------: | :----------------: |
|    Build Only |          ✓           |                   |                    |
|         Build |          ✓           |         ✓         |                    |
| Build and Run |                      |         ✓         |                    |
|           Run |                      |         ✓         |         ✓          |
|      Run Only |                      |                   |         ✓          |

If no arguments are passed, it assumes `--build-and-run`. If multiple arguments are passed, the one provided last is used.

### Targets

The first non-option argument is the target.

The target consists of two parts: A file, and a target, separated by a `:`. A file only can be specified with a trailing `:`, and if there's no `:` the argument is interpreted as a target only.

Files are found by iteratively searching parent directories for a file which matches what was specified. When a file is specified, e.g. `f`, runscript looks in the current working directory for `f.run`, then `f/run`. If no file is specified, runscript looks in the current working directory for `run`. If it couldn't find a runfile in the current working directory, it sets the effective current working directory to the parent directory of the CWD, then repeats the search.

The working directory of all the commands inside the runfile is the directory the runfile is in

```shell
run        # Runs the default target of ./run
run test   # Runs the target "test" of ./run
run test:  # Runs the default target of ./test.run or ./test/run
run test:t # Runs the target "t" of ./test.run or ./test/run
```



## How it looks

```run
! This is a single-line comment
! Multi-line comments do not exist
! Comments can only appear outside of blocks

! Global block. Runs after the specified target block with the same build phase
##
echo Normal shell commands go here
echo Supports $VARIABLES and $(echo subcommands)
echo 'single quoted' and "double quoted" arguments work
echo and "double quoted" args support "$VARS and $(sub)commands" as well
#/

! Default command block. Runs if no target is specified
#-
echo "run" doesn't spawn a new command
echo it is instead re-entrant
run target
#/

! Phases can be specified on any block
#target b!
echo Build Only
#/

! Targets can only be defined once per phase
#target b
echo Build
#/

! If no phase is specified, it defaults to Build and Run
#target br
echo Build and Run
#/

#target r
echo Run
#/

#target r!
echo RunOnly
#/
```

