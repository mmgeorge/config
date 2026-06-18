use std::collections::BTreeMap;
use std::path::Path;
use std::process::Command;

#[derive(Clone, Debug, Eq, PartialEq)]
struct FunctionSignature {
    params: Vec<u8>,
    results: Vec<u8>,
}

#[test]
#[ignore = "builds the wasm artifact and validates the Zellij plugin ABI"]
fn wasm_exports_required_zellij_plugin_functions() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let build_status = Command::new("cargo")
        .args(["build", "--target", "wasm32-wasip1", "--release"])
        .current_dir(manifest_dir)
        .status()
        .expect("failed to run cargo build");
    assert!(build_status.success(), "wasm release build failed");

    let wasm_path = manifest_dir.join("target/wasm32-wasip1/release/sessions.wasm");
    let wasm = std::fs::read(&wasm_path).expect("failed to read built wasm");
    let signatures = exported_function_signatures(&wasm);

    assert_eq!(signatures.get("_start"), Some(&signature(&[], &[])));
    assert_eq!(signatures.get("load"), Some(&signature(&[], &[])));
    assert_eq!(signatures.get("update"), Some(&signature(&[], &[0x7f])));
    assert_eq!(signatures.get("render"), Some(&signature(&[0x7f, 0x7f], &[])));
    assert_eq!(signatures.get("pipe"), Some(&signature(&[], &[0x7f])));
    assert_eq!(signatures.get("plugin_version"), Some(&signature(&[], &[])));
}

fn signature(params: &[u8], results: &[u8]) -> FunctionSignature {
    FunctionSignature {
        params: params.to_vec(),
        results: results.to_vec(),
    }
}

fn exported_function_signatures(wasm: &[u8]) -> BTreeMap<String, FunctionSignature> {
    assert_eq!(&wasm[..8], b"\0asm\x01\0\0\0", "invalid wasm header");

    let sections = read_sections(wasm);
    let types = read_type_section(wasm, sections[&1].clone());
    let function_type_indices = read_function_type_indices(wasm, &sections);
    let exports = read_export_section(wasm, sections[&7].clone());

    exports
        .into_iter()
        .filter_map(|(name, export)| {
            if export.kind != 0 {
                return None;
            }
            let type_index = function_type_indices[export.index as usize] as usize;
            Some((name, types[type_index].clone()))
        })
        .collect()
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Export {
    kind: u8,
    index: u32,
}

fn read_sections(wasm: &[u8]) -> BTreeMap<u8, std::ops::Range<usize>> {
    let mut cursor = 8;
    let mut sections = BTreeMap::new();
    while cursor < wasm.len() {
        let section_id = wasm[cursor];
        cursor += 1;
        let section_size = read_u32(wasm, &mut cursor) as usize;
        let section_start = cursor;
        let section_end = section_start + section_size;
        sections.insert(section_id, section_start..section_end);
        cursor = section_end;
    }
    sections
}

fn read_type_section(wasm: &[u8], section: std::ops::Range<usize>) -> Vec<FunctionSignature> {
    let mut cursor = section.start;
    let type_count = read_u32(wasm, &mut cursor);
    let mut types = Vec::new();
    for _type_index in 0..type_count {
        assert_eq!(wasm[cursor], 0x60, "expected function type");
        cursor += 1;
        let params = read_valtype_vector(wasm, &mut cursor);
        let results = read_valtype_vector(wasm, &mut cursor);
        types.push(FunctionSignature { params, results });
    }
    types
}

fn read_function_type_indices(
    wasm: &[u8],
    sections: &BTreeMap<u8, std::ops::Range<usize>>,
) -> Vec<u32> {
    let mut indices = Vec::new();

    if let Some(import_section) = sections.get(&2) {
        let mut cursor = import_section.start;
        let import_count = read_u32(wasm, &mut cursor);
        for _import_index in 0..import_count {
            skip_name(wasm, &mut cursor);
            skip_name(wasm, &mut cursor);
            let kind = wasm[cursor];
            cursor += 1;
            match kind {
                0 => indices.push(read_u32(wasm, &mut cursor)),
                1 => skip_table_type(wasm, &mut cursor),
                2 => skip_limits(wasm, &mut cursor),
                3 => cursor += 2,
                _ => panic!("unknown import kind {kind}"),
            }
        }
    }

    let function_section = sections
        .get(&3)
        .expect("missing wasm function section")
        .clone();
    let mut cursor = function_section.start;
    let function_count = read_u32(wasm, &mut cursor);
    for _function_index in 0..function_count {
        indices.push(read_u32(wasm, &mut cursor));
    }

    indices
}

fn read_export_section(wasm: &[u8], section: std::ops::Range<usize>) -> BTreeMap<String, Export> {
    let mut cursor = section.start;
    let export_count = read_u32(wasm, &mut cursor);
    let mut exports = BTreeMap::new();
    for _export_index in 0..export_count {
        let name = read_name(wasm, &mut cursor);
        let kind = wasm[cursor];
        cursor += 1;
        let index = read_u32(wasm, &mut cursor);
        exports.insert(name, Export { kind, index });
    }
    exports
}

fn read_valtype_vector(wasm: &[u8], cursor: &mut usize) -> Vec<u8> {
    let count = read_u32(wasm, cursor);
    let mut values = Vec::new();
    for _value_index in 0..count {
        values.push(wasm[*cursor]);
        *cursor += 1;
    }
    values
}

fn read_name(wasm: &[u8], cursor: &mut usize) -> String {
    let name_length = read_u32(wasm, cursor) as usize;
    let name = String::from_utf8(wasm[*cursor..*cursor + name_length].to_vec())
        .expect("export name is not utf-8");
    *cursor += name_length;
    name
}

fn skip_name(wasm: &[u8], cursor: &mut usize) {
    let name_length = read_u32(wasm, cursor) as usize;
    *cursor += name_length;
}

fn skip_table_type(wasm: &[u8], cursor: &mut usize) {
    *cursor += 1;
    skip_limits(wasm, cursor);
}

fn skip_limits(wasm: &[u8], cursor: &mut usize) {
    let flags = read_u32(wasm, cursor);
    let _minimum = read_u32(wasm, cursor);
    if flags & 1 == 1 {
        let _maximum = read_u32(wasm, cursor);
    }
}

fn read_u32(wasm: &[u8], cursor: &mut usize) -> u32 {
    let mut result = 0;
    let mut shift = 0;
    loop {
        let byte = wasm[*cursor];
        *cursor += 1;
        result |= ((byte & 0x7f) as u32) << shift;
        if byte < 0x80 {
            return result;
        }
        shift += 7;
    }
}
