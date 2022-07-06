# Installation

Currently, Runscript does not support any package managers.

## Pre-built binaries

Executable binaries are available for download on the [GitHub Releases page](https://github.com/TheOnlyMrCat/runscript/releases).
Download the binary for your platform (Windows, macOS, or Linux) and extract the archive. The archive contains the `run`
executable.

To make it easier to run, put the path to the binary into your `PATH`.

## Build from source using rust

Alternatively, you can use the `cargo` package manager to build the master branch of Runscript. To do this, you will need
to install Rust and Cargo. Follow the instructions on the [Rust installation page](https://www.rust-lang.org/tools/install).

Once you have installed Rust, the following command can be used to build and install Runscript:

```sh
cargo install --git https://github.com/TheOnlyMrCat/runscript runscript
```
