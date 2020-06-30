fn main() {
    lalrpop::Configuration::new()
        .emit_rerun_directives(true)
        .emit_report(true)
        .force_build(true)
        .process_current_dir()
        .unwrap();
}