use std::path::Path;

const GRAMMAR: &[&str] = &["wgsl", "latex", "cue"];

fn main() {
    for name in GRAMMAR {
        let source = Path::new("grammar").join(name).join("src");
        let mut compiler = cc::Build::new();
        compiler
            .warnings(false)
            .debug(false)
            .opt_level(1)
            .flag_if_supported("-std=c11")
            .flag_if_supported("/std:c11")
            .include(&source)
            .file(source.join("parser.c"));

        let scanner = source.join("scanner.c");
        if scanner.exists() {
            compiler.file(scanner);
        }

        compiler.compile(&format!("forge_grammar_{name}"));
    }
    println!("cargo:rerun-if-changed=grammar");
}
