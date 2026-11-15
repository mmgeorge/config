use serde_json::{Value, json};

/// Describe the project selector shared by every semantic tool.
pub(super) fn workspace_schema() -> Value {
    json!({"type":"object","additionalProperties":false,"required":["path","language"],"properties":{
        "path":{"type":"string","minLength":1,"description":"Project directory returned by lsp_workspace_list, relative to the session repository."},
        "language":{"type":"string","enum":["rust","typescript","lua"],"description":"Language key returned by lsp_workspace_list; typescript includes JavaScript."}
    }})
}
/// Build common source-query fields; each tool adds its own options.
pub(super) fn base_schema(operation: &str) -> (serde_json::Map<String, Value>, Vec<&'static str>) {
    let position = matches!(operation, "definition" | "context" | "references" | "calls");
    let mut properties = serde_json::Map::from_iter([(
        "path".into(),
        json!({"type":"string","minLength":1,"description":"Session-repository-relative or authorized absolute path; not relative to workspace.path."}),
    )]);
    let fields = &mut properties;
    let required = if matches!(
        operation,
        "overview" | "source_symbols" | "workspace_list" | "context"
    ) {
        Vec::new()
    } else {
        vec!["workspace"]
    };
    if !matches!(operation, "overview" | "source_symbols" | "workspace_list") {
        fields.insert("workspace".into(), workspace_schema());
    }
    if position {
        fields.insert("line".into(), json!({"type":"integer","minimum":1,"description":"One-based source line from a tool result."}));
        fields.insert("column".into(), json!({"type":"integer","minimum":1,"description":"One-based Unicode code-point column of the symbol."}));
        fields.insert("symbol".into(), json!({"type":"string","minLength":1,"description":"Exact short or qualified name. With line, locate its spelling on that line. Ambiguity returns candidates."}));
    }
    if operation != "status" {
        fields.insert("token_budget".into(), json!({"type":"integer","minimum":1,"maximum":8000,"default":2000,"description":"Estimated output tokens, not an exact model tokenizer count."}));
    }
    if !matches!(operation, "context" | "status" | "paths") {
        fields.insert("limit".into(), json!({"type":"integer","minimum":1,"default":20,"description":"Requested maximum result count. Output budget and pagination still apply."}));
        fields.insert("offset".into(), json!({"type":"integer","minimum":0,"default":0,"description":"Use returned next_offset. Each page queries current source."}));
    }
    if matches!(operation, "overview" | "source_symbols") {
        fields.insert("depth".into(), json!({"type":"integer","minimum":0,"maximum":8,"default":2,"description":"Directory levels below path. Zero reads immediate files."}));
        fields.insert("visibility".into(), json!({"type":"string","enum":["all","public","internal","private"],"default":if operation == "overview" {"public"} else {"all"}}));
    }
    if matches!(operation, "symbols" | "references" | "calls" | "paths") {
        fields.insert(
            "scope".into(),
            json!({"type":"string","enum":["workspace","all"],"default":"workspace"}),
        );
    }

    (properties, required)
}
