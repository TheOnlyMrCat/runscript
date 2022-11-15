#[test]
fn cli() {
    trycmd::TestCases::new()
        .default_bin_name("run")
        .env("RUNSCRIPT_CONFIG_DIR", "tests/config")
        .case("tests/cli/*.toml");
}

#[test]
fn shell_language() {
    trycmd::TestCases::new()
        .default_bin_name("run")
        .env("RUNSCRIPT_CONFIG_DIR", "tests/config")
        .case("tests/sh/*.toml");
}

#[test]
#[cfg_attr(not(feature = "old-parser"), ignore)]
fn old_parser() {
    trycmd::TestCases::new()
        .default_bin_name("run")
        .env("RUNSCRIPT_CONFIG_DIR", "tests/config")
        .case("tests/old/*.toml");
}
