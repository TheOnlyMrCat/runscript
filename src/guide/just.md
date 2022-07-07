# Comparison with [casey/just](https://github.com/casey/just)

`run` and `just` were written to accomplish, in essence, the same task of managing project-specific commands. Here is a
breakdown of the main differences between the two command runners:

**Disclaimers:**

- I have not actually used `just`. This feature breakdown is based solely on the features listed in its
[documentation](https://just.systems/man/en).
- This comparison is currently incomplete. Expect more headings to be added in the future.

## Command execution

`just` always delegates script execution to an external shell. By default, each line of a recipe is executed by a individual
`sh` instances, but a shell an be supplied for an entire recipe by placing a shebang `#!/bin/sh` line at the start of the
recipe, or per-Justfile with a setting `set shell := /bin/sh`.

`run`, by default, tries to execute scripts itself, and provide output while doing so. It can be instructed to delegate
to an external shell in the script header `[script] /bin/sh` or a file-wide option `::default-shell /bin/sh`.

For both tools, this allows recipes/scripts to be written in any language, not just shell language.

## Variables, positional parameters, and substitution

`just` allows variables to be defined at the top of a Justfile, which are evaluated before the recipe is executed and can be
substituted into a recipe. Recipes can also define positional parameters `build TARGET:` that can be substituted in the same
way. Variable substitutions are done with double curly braces `cc -o {{TARGET}} {{TARGET}}.c`, and positional parameters are
passed on the command-line in the same way as a recipe target `just build my-target`.

`run` does not have the same concept of variables or script-specified positional parameters. These designs are handled by
the shell language instead. Positional parameters are passed on the command-line separated from the main command by a `--`:
`run target -- argument`, and can be read by the script using `$1`, `$2`, etc. parameter substitutions.
