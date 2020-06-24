# Runscript

Runscript is a tool like `make` which manages run commands. Runscript can also handle building, but it doesn't track file update times (yet?), so you may still want to use make for that (but you can call it from runscript).

## How to install

Binaries are available from the [releases](https://github.com/TheOnlyMrCat/runscript) page. I'll add package manager support later.

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

When no arguments are passed, it assumes `--build-and-run`.

### Targets

The first non-option argument is the target.

The target consists of two parts: A file, and a target, separated by a `:`. A file only can be specified with a trailing `:`, and if there's no `:` the argument is interpreted as a target only.

When a file is specified, `f`, runscript looks in `f.run`. If no file is specified, runscript looks in the current working directory for `run`.

```shell
run        # Runs the default target of ./run
run test   # Runs the target "test" of ./run
run test:  # Runs the default target of ./test.run
run test:t # Runs the target "t" of ./test.run
```



## How it looks

```run
! This is a single-line comment
! Multi-line comments do not exist
! Comments can only appear outside of blocks

! Global block. Runs after target block
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

