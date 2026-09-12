use std::path::{Path, PathBuf};

const GRAMMAR: &[(&str, &str)] = &[
    ("rust", ""),
    ("typescript", "typescript/"),
    ("tsx", "tsx/"),
    ("lua", ""),
    ("vim", ""),
    ("vimdoc", ""),
    ("json", ""),
    ("query", ""),
    ("javascript", ""),
    ("css", ""),
    ("html", ""),
    ("wgsl", ""),
    ("glsl", ""),
    ("c_sharp", ""),
    ("toml", ""),
    ("slang", ""),
    ("yaml", ""),
    ("nu", ""),
    ("markdown", "tree-sitter-markdown/"),
    ("markdown_inline", "tree-sitter-markdown-inline/"),
    ("latex", ""),
    ("cue", ""),
];

fn main() {
    for (name, location) in GRAMMAR {
        let source = grammar_source(name, location);
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

fn grammar_source(name: &str, location: &str) -> PathBuf {
    Path::new("grammar").join(name).join(location).join("src")
}
