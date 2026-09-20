Deliberately holds no `zelkova.toml` — `compile_package_reports_a_missing_manifest` in
`tests/pipeline.rs` asserts that a package directory with no manifest is a `CompilationError`
naming the missing file, not a silent empty compile.
