extern crate compiletest_rs as compiletest;

use std::path::PathBuf;

fn run_mode(mode: &'static str) {
    let mut config = compiletest::Config::default();

    config.mode = mode.parse().expect("Invalid mode");
    config.src_base = PathBuf::from(format!("../../components/evaluator/tests/{}", mode));
    config.rustdoc_path = Some(PathBuf::from(""));
    config.rustc_path = PathBuf::from("../../target/debug/evaluator");
    // config.link_deps(); // Populate config.target_rustcflags with dependencies on
    // the path config.clean_rmeta(); // If your tests import the parent crate,
    // this helps with E0464

    compiletest::run_tests(&config);
}

#[test]
fn compile_test() {
    run_mode("ui");
    run_mode("compile-fail");
    run_mode("run-pass");
}
