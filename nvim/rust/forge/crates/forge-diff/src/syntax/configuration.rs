use tree_sitter::Parser;

use super::{SyntaxError, SyntaxLanguage};

/// Validates complete configuration documents without rewriting values or layout.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConfigurationFormat {
    /// Strict JSON values.
    Json,
    /// JSON with comments and trailing commas.
    Jsonc,
    /// TOML tables and values.
    Toml,
    /// YAML documents with block scalars.
    Yaml,
    /// Well-formed XML documents and project manifests.
    Xml,
    /// Git ignore patterns retained verbatim, including comments, escapes, and negation.
    Gitignore,
}

impl ConfigurationFormat {
    /// Decode TypeScript configuration without discarding comments manually.
    pub fn json_value(text: &str) -> Result<serde_json::Value, SyntaxError> {
        jsonc_parser::parse_to_serde_value::<serde_json::Value>(text, &jsonc_parser::ParseOptions::default())
            .map_err(|error| SyntaxError::Query(error.to_string()))
    }
    /// Select a configuration format by extension or recognized configuration filename.
    pub fn for_path(path: &str) -> Option<Self> {
        let normalized = path.replace('\\', "/").to_ascii_lowercase();
        let name = normalized.rsplit('/').next()?;
        if name == ".gitignore" { return Some(Self::Gitignore); }
        let extension = name.rsplit_once('.')?.1;
        match extension {
            "json"
                if name == "tsconfig.json"
                    || name.starts_with("tsconfig.")
                    || name == "jsconfig.json"
                    || name.starts_with("jsconfig.")
                    || normalized.starts_with(".vscode/")
                    || normalized.contains("/.vscode/") =>
            {
                Some(Self::Jsonc)
            }
            "json" => Some(Self::Json),
            "jsonc" => Some(Self::Jsonc),
            "toml" => Some(Self::Toml),
            "yaml" | "yml" => Some(Self::Yaml),
            "xml" | "csproj" | "fsproj" | "vbproj" | "props" | "targets" | "resx" | "plist"
            | "xsd" | "xsl" | "xslt" => Some(Self::Xml),
            _ => None,
        }
    }

    /// Return the native inspection filetype independently of grammar availability.
    pub fn name(self) -> &'static str {
        match self {
            Self::Json => "json",
            Self::Jsonc => "jsonc",
            Self::Toml => "toml",
            Self::Yaml => "yaml",
            Self::Xml => "xml",
            Self::Gitignore => "gitignore",
        }
    }

    /// Reject malformed configuration while preserving the caller's exact text.
    pub fn validate(self, path: &str, text: &str) -> Result<(), SyntaxError> {
        let invalid = |error: String| {
            SyntaxError::Query(format!(
                "invalid {} configuration in {path}: {error}",
                self.name()
            ))
        };
        match self {
            Self::Gitignore => {
                if text.contains('\0') { return Err(invalid("NUL is not allowed".into())); }
            }
            Self::Json => {
                serde_json::from_str::<serde_json::Value>(text)
                    .map_err(|error| invalid(error.to_string()))?;
            }
            Self::Jsonc => {
                let options = jsonc_parser::ParseOptions {
                    allow_comments: true,
                    allow_trailing_commas: true,
                    allow_loose_object_property_names: false,
                    allow_missing_commas: false,
                    allow_single_quoted_strings: false,
                    allow_hexadecimal_numbers: false,
                    allow_unary_plus_numbers: false,
                    allow_bare_decimal_point_numbers: false,
                    allow_non_finite_numbers: false,
                    allow_extended_string_escapes: false,
                };
                if text.trim().is_empty() {
                    return Err(invalid("expected a JSON value".into()));
                }
                jsonc_parser::parse_to_serde_value::<serde_json::Value>(text, &options)
                    .map_err(|error| invalid(error.to_string()))?;
            }
            Self::Toml => {
                toml::from_str::<toml::Value>(text).map_err(|error| invalid(error.to_string()))?;
            }
            Self::Yaml => {
                let mut parser = Parser::new();
                parser
                    .set_language(&SyntaxLanguage::Yaml.grammar())
                    .map_err(|error| invalid(error.to_string()))?;
                let tree = parser
                    .parse(text, None)
                    .ok_or_else(|| invalid("parsing was cancelled".into()))?;
                if tree.root_node().has_error() {
                    return Err(invalid("invalid YAML syntax".into()));
                }
            }
            Self::Xml => {
                roxmltree::Document::parse_with_options(
                    text,
                    roxmltree::ParsingOptions {
                        allow_dtd: true,
                        nodes_limit: 1024 * 1024,
                        ..Default::default()
                    },
                )
                .map_err(|error| invalid(error.to_string()))?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::{DeclarationOverview, DeclarationVisibility};

    #[test]
    fn configuration_preserves_values_layout_visibility_and_navigation() {
        for (path, text) in [
            (".gitignore", "# Build output\r\n/target/\r\n!Cargo.lock\r\n\\#literal\r\ncache\\ \r\n"),
            ("nested/.gitignore", "*.tmp\n!keep.tmp\n"),
            (
                "package.json",
                "{\r\n  \"scripts\": {\"start\": \"node server.js\"}\r\n}\r\n",
            ),
            (
                "tsconfig.json",
                "{\n  // Type checks\n  \"compilerOptions\": {\"strict\": true,},\n}\n",
            ),
            (
                "settings.jsonc",
                "{\"endpoint\": \"https://example.com/*literal*/\",}\n",
            ),
            ("Cargo.toml", "[package]\nname = \"arena\"\n"),
            (
                "ci.yml",
                "# Build\njobs:\n  build:\n    script: |\n      echo first\n        echo second\n",
            ),
            (
                "App.csproj",
                "<Project>\r\n  <!-- Keep settings -->\r\n  <PropertyGroup><TargetFramework>net9.0</TargetFramework></PropertyGroup>\r\n</Project>\r\n",
            ),
            (
                "Info.plist",
                "<?xml version=\"1.0\"?><!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\"><plist version=\"1.0\"><dict/></plist>",
            ),
        ] {
            assert_eq!(
                DeclarationOverview::extract(path, text).unwrap(),
                text,
                "{path}"
            );
            assert_eq!(
                DeclarationOverview::format_with_width(path, text, 40).unwrap(),
                text,
                "{path}"
            );
            let presentation = DeclarationOverview::present(path, text).unwrap();
            assert_eq!(presentation.text, text);
            for (row, target) in presentation.source.iter().enumerate() {
                assert_eq!(target.unwrap().line as usize, row + 1);
            }
            for public_only in [false, true] {
                let visibility = DeclarationVisibility::analyze(path, text, public_only).unwrap();
                assert!(visibility.rows.iter().all(|visible| *visible));
                assert!(visibility.replacement.is_empty());
            }
        }
    }

    #[test]
    fn malformed_configuration_and_wrong_json_dialect_are_rejected() {
        for (path, text) in [
            (".gitignore", "target\0/"),
            ("package.json", "{\"name\": \"app\",}"),
            ("package.json", "// Comment\n{}"),
            ("package.json", "{} {}"),
            ("tsconfig.json", "{\"strict\": true \"other\": false}"),
            ("settings.jsonc", "{unquoted: true}"),
            ("Cargo.toml", "name = \"one\"\nname = \"two\""),
            ("ci.yaml", "jobs: [unclosed"),
            ("pom.xml", "<project><name>app</project>"),
            ("pom.xml", "<project/> <project/>"),
        ] {
            assert!(
                DeclarationOverview::parse(path, text).is_err(),
                "{path}: {text}"
            );
        }
    }
}
