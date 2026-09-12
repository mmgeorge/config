use tree_sitter::Language;
use tree_sitter_language::LanguageFn;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SyntaxLanguage {
    Rust,
    Typescript,
    Tsx,
    Lua,
    Vim,
    Vimdoc,
    Json,
    Query,
    Javascript,
    Css,
    Html,
    Wgsl,
    Glsl,
    CSharp,
    Toml,
    Slang,
    Yaml,
    Nu,
    Markdown,
    MarkdownInline,
    Latex,
    Cue,
}

impl SyntaxLanguage {
    pub const ALL: &[Self] = &[
        Self::Rust,
        Self::Typescript,
        Self::Tsx,
        Self::Lua,
        Self::Vim,
        Self::Vimdoc,
        Self::Json,
        Self::Query,
        Self::Javascript,
        Self::Css,
        Self::Html,
        Self::Wgsl,
        Self::Glsl,
        Self::CSharp,
        Self::Toml,
        Self::Slang,
        Self::Yaml,
        Self::Nu,
        Self::Markdown,
        Self::MarkdownInline,
        Self::Latex,
        Self::Cue,
    ];
    pub fn name(self) -> &'static str {
        match self {
            Self::Rust => "rust",
            Self::Typescript => "typescript",
            Self::Tsx => "tsx",
            Self::Lua => "lua",
            Self::Vim => "vim",
            Self::Vimdoc => "vimdoc",
            Self::Json => "json",
            Self::Query => "query",
            Self::Javascript => "javascript",
            Self::Css => "css",
            Self::Html => "html",
            Self::Wgsl => "wgsl",
            Self::Glsl => "glsl",
            Self::CSharp => "c_sharp",
            Self::Toml => "toml",
            Self::Slang => "slang",
            Self::Yaml => "yaml",
            Self::Nu => "nu",
            Self::Markdown => "markdown",
            Self::MarkdownInline => "markdown_inline",
            Self::Latex => "latex",
            Self::Cue => "cue",
        }
    }
    pub fn for_path(path: &str) -> Option<Self> {
        let name = path.rsplit(['/', '\\']).next()?;
        let extension = name.rsplit_once('.')?.1;
        Self::from_name(match extension {
            "rs" => "rust",
            "md" => "markdown",
            "scm" => "query",
            "yml" => "yaml",
            "jsx" | "mjs" | "cjs" => "javascript",
            _ => extension,
        })
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "rust" => Some(Self::Rust),
            "typescript" | "ts" => Some(Self::Typescript),
            "tsx" | "typescriptreact" => Some(Self::Tsx),
            "lua" => Some(Self::Lua),
            "vim" => Some(Self::Vim),
            "vimdoc" | "help" => Some(Self::Vimdoc),
            "json" => Some(Self::Json),
            "query" => Some(Self::Query),
            "javascript" | "js" | "javascriptreact" => Some(Self::Javascript),
            "css" => Some(Self::Css),
            "html" => Some(Self::Html),
            "wgsl" | "wgslx" => Some(Self::Wgsl),
            "glsl" | "frag" | "vert" => Some(Self::Glsl),
            "c_sharp" | "cs" | "csharp" => Some(Self::CSharp),
            "toml" => Some(Self::Toml),
            "slang" | "shaderslang" => Some(Self::Slang),
            "yaml" => Some(Self::Yaml),
            "nu" => Some(Self::Nu),
            "markdown" => Some(Self::Markdown),
            "markdown_inline" => Some(Self::MarkdownInline),
            "latex" | "tex" => Some(Self::Latex),
            "cue" => Some(Self::Cue),
            _ => None,
        }
    }
    pub(crate) fn grammar(self) -> Language {
        unsafe extern "C" {
            fn tree_sitter_rust() -> *const ();
            fn tree_sitter_typescript() -> *const ();
            fn tree_sitter_tsx() -> *const ();
            fn tree_sitter_lua() -> *const ();
            fn tree_sitter_vim() -> *const ();
            fn tree_sitter_vimdoc() -> *const ();
            fn tree_sitter_json() -> *const ();
            fn tree_sitter_query() -> *const ();
            fn tree_sitter_javascript() -> *const ();
            fn tree_sitter_css() -> *const ();
            fn tree_sitter_html() -> *const ();
            fn tree_sitter_wgsl() -> *const ();
            fn tree_sitter_glsl() -> *const ();
            fn tree_sitter_c_sharp() -> *const ();
            fn tree_sitter_toml() -> *const ();
            fn tree_sitter_slang() -> *const ();
            fn tree_sitter_yaml() -> *const ();
            fn tree_sitter_nu() -> *const ();
            fn tree_sitter_markdown() -> *const ();
            fn tree_sitter_markdown_inline() -> *const ();
            fn tree_sitter_latex() -> *const ();
            fn tree_sitter_cue() -> *const ();
        }
        let function = match self {
            Self::Rust => tree_sitter_rust as unsafe extern "C" fn() -> *const (),
            Self::Typescript => tree_sitter_typescript as unsafe extern "C" fn() -> *const (),
            Self::Tsx => tree_sitter_tsx as unsafe extern "C" fn() -> *const (),
            Self::Lua => tree_sitter_lua as unsafe extern "C" fn() -> *const (),
            Self::Vim => tree_sitter_vim as unsafe extern "C" fn() -> *const (),
            Self::Vimdoc => tree_sitter_vimdoc as unsafe extern "C" fn() -> *const (),
            Self::Json => tree_sitter_json as unsafe extern "C" fn() -> *const (),
            Self::Query => tree_sitter_query as unsafe extern "C" fn() -> *const (),
            Self::Javascript => tree_sitter_javascript as unsafe extern "C" fn() -> *const (),
            Self::Css => tree_sitter_css as unsafe extern "C" fn() -> *const (),
            Self::Html => tree_sitter_html as unsafe extern "C" fn() -> *const (),
            Self::Wgsl => tree_sitter_wgsl as unsafe extern "C" fn() -> *const (),
            Self::Glsl => tree_sitter_glsl as unsafe extern "C" fn() -> *const (),
            Self::CSharp => tree_sitter_c_sharp as unsafe extern "C" fn() -> *const (),
            Self::Toml => tree_sitter_toml as unsafe extern "C" fn() -> *const (),
            Self::Slang => tree_sitter_slang as unsafe extern "C" fn() -> *const (),
            Self::Yaml => tree_sitter_yaml as unsafe extern "C" fn() -> *const (),
            Self::Nu => tree_sitter_nu as unsafe extern "C" fn() -> *const (),
            Self::Markdown => tree_sitter_markdown as unsafe extern "C" fn() -> *const (),
            Self::MarkdownInline => {
                tree_sitter_markdown_inline as unsafe extern "C" fn() -> *const ()
            }
            Self::Latex => tree_sitter_latex as unsafe extern "C" fn() -> *const (),
            Self::Cue => tree_sitter_cue as unsafe extern "C" fn() -> *const (),
        };
        // Each linked generated function returns a static language from the pinned grammar manifest.
        Language::new(unsafe { LanguageFn::from_raw(function) })
    }
}

pub(crate) struct QueryAsset {
    pub language: &'static str,
    pub kind: &'static str,
    pub origin: &'static str,
    pub text: &'static str,
}

pub(crate) const QUERY_ASSET: &[QueryAsset] = &[
    QueryAsset {
        language: "javascript",
        kind: "diff_context",
        origin: "forge",
        text: include_str!("../../query/forge/javascript/diff_context.scm"),
    },
    QueryAsset {
        language: "python",
        kind: "diff_context",
        origin: "forge",
        text: include_str!("../../query/forge/python/diff_context.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "diff_context",
        origin: "forge",
        text: include_str!("../../query/forge/rust/diff_context.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "diff_inventory",
        origin: "forge",
        text: include_str!("../../query/forge/rust/diff_inventory.scm"),
    },
    QueryAsset {
        language: "slang",
        kind: "diff_context",
        origin: "forge",
        text: include_str!("../../query/forge/slang/diff_context.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "diff_context",
        origin: "forge",
        text: include_str!("../../query/forge/tsx/diff_context.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "diff_inventory",
        origin: "forge",
        text: include_str!("../../query/forge/tsx/diff_inventory.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "diff_context",
        origin: "forge",
        text: include_str!("../../query/forge/typescript/diff_context.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "diff_inventory",
        origin: "forge",
        text: include_str!("../../query/forge/typescript/diff_inventory.scm"),
    },
    QueryAsset {
        language: "json",
        kind: "parent",
        origin: "repository",
        text: include_str!("../../query/repository/json/parent.scm"),
    },
    QueryAsset {
        language: "lua",
        kind: "parent",
        origin: "repository",
        text: include_str!("../../query/repository/lua/parent.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "aerial",
        origin: "repository",
        text: include_str!("../../query/repository/rust/aerial.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "highlights",
        origin: "repository",
        text: include_str!("../../query/repository/rust/highlights.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "parent",
        origin: "repository",
        text: include_str!("../../query/repository/rust/parent.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "textobjects",
        origin: "repository",
        text: include_str!("../../query/repository/rust/textobjects.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "parent",
        origin: "repository",
        text: include_str!("../../query/repository/tsx/parent.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "textobjects",
        origin: "repository",
        text: include_str!("../../query/repository/tsx/textobjects.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "aerial",
        origin: "repository",
        text: include_str!("../../query/repository/typescript/aerial.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "highlights",
        origin: "repository",
        text: include_str!("../../query/repository/typescript/highlights.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "parent",
        origin: "repository",
        text: include_str!("../../query/repository/typescript/parent.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "textobjects",
        origin: "repository",
        text: include_str!("../../query/repository/typescript/textobjects.scm"),
    },
    QueryAsset {
        language: "wesl",
        kind: "highlights",
        origin: "repository",
        text: include_str!("../../query/repository/wesl/highlights.scm"),
    },
    QueryAsset {
        language: "c",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/c/folds.scm"),
    },
    QueryAsset {
        language: "c",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/c/highlights.scm"),
    },
    QueryAsset {
        language: "c",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/c/indents.scm"),
    },
    QueryAsset {
        language: "c",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/c/injections.scm"),
    },
    QueryAsset {
        language: "c",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/c/locals.scm"),
    },
    QueryAsset {
        language: "c_sharp",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/c_sharp/folds.scm"),
    },
    QueryAsset {
        language: "c_sharp",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/c_sharp/highlights.scm"),
    },
    QueryAsset {
        language: "c_sharp",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/c_sharp/injections.scm"),
    },
    QueryAsset {
        language: "c_sharp",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/c_sharp/locals.scm"),
    },
    QueryAsset {
        language: "cpp",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/cpp/folds.scm"),
    },
    QueryAsset {
        language: "cpp",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/cpp/highlights.scm"),
    },
    QueryAsset {
        language: "cpp",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/cpp/indents.scm"),
    },
    QueryAsset {
        language: "cpp",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/cpp/injections.scm"),
    },
    QueryAsset {
        language: "cpp",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/cpp/locals.scm"),
    },
    QueryAsset {
        language: "css",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/css/folds.scm"),
    },
    QueryAsset {
        language: "css",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/css/highlights.scm"),
    },
    QueryAsset {
        language: "css",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/css/indents.scm"),
    },
    QueryAsset {
        language: "css",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/css/injections.scm"),
    },
    QueryAsset {
        language: "cue",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/cue/folds.scm"),
    },
    QueryAsset {
        language: "cue",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/cue/highlights.scm"),
    },
    QueryAsset {
        language: "cue",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/cue/indents.scm"),
    },
    QueryAsset {
        language: "cue",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/cue/injections.scm"),
    },
    QueryAsset {
        language: "cue",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/cue/locals.scm"),
    },
    QueryAsset {
        language: "ecma",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/ecma/folds.scm"),
    },
    QueryAsset {
        language: "ecma",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/ecma/highlights.scm"),
    },
    QueryAsset {
        language: "ecma",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/ecma/indents.scm"),
    },
    QueryAsset {
        language: "ecma",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/ecma/injections.scm"),
    },
    QueryAsset {
        language: "ecma",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/ecma/locals.scm"),
    },
    QueryAsset {
        language: "glsl",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/glsl/folds.scm"),
    },
    QueryAsset {
        language: "glsl",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/glsl/highlights.scm"),
    },
    QueryAsset {
        language: "glsl",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/glsl/indents.scm"),
    },
    QueryAsset {
        language: "glsl",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/glsl/injections.scm"),
    },
    QueryAsset {
        language: "glsl",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/glsl/locals.scm"),
    },
    QueryAsset {
        language: "hlsl",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/hlsl/folds.scm"),
    },
    QueryAsset {
        language: "hlsl",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/hlsl/highlights.scm"),
    },
    QueryAsset {
        language: "hlsl",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/hlsl/indents.scm"),
    },
    QueryAsset {
        language: "hlsl",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/hlsl/injections.scm"),
    },
    QueryAsset {
        language: "hlsl",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/hlsl/locals.scm"),
    },
    QueryAsset {
        language: "html",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/html/folds.scm"),
    },
    QueryAsset {
        language: "html",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/html/highlights.scm"),
    },
    QueryAsset {
        language: "html",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/html/indents.scm"),
    },
    QueryAsset {
        language: "html",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/html/injections.scm"),
    },
    QueryAsset {
        language: "html",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/html/locals.scm"),
    },
    QueryAsset {
        language: "html_tags",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/html_tags/highlights.scm"),
    },
    QueryAsset {
        language: "html_tags",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/html_tags/indents.scm"),
    },
    QueryAsset {
        language: "html_tags",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/html_tags/injections.scm"),
    },
    QueryAsset {
        language: "javascript",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/javascript/folds.scm"),
    },
    QueryAsset {
        language: "javascript",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/javascript/highlights.scm"),
    },
    QueryAsset {
        language: "javascript",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/javascript/indents.scm"),
    },
    QueryAsset {
        language: "javascript",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/javascript/injections.scm"),
    },
    QueryAsset {
        language: "javascript",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/javascript/locals.scm"),
    },
    QueryAsset {
        language: "json",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/json/folds.scm"),
    },
    QueryAsset {
        language: "json",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/json/highlights.scm"),
    },
    QueryAsset {
        language: "json",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/json/indents.scm"),
    },
    QueryAsset {
        language: "json",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/json/locals.scm"),
    },
    QueryAsset {
        language: "jsx",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/jsx/folds.scm"),
    },
    QueryAsset {
        language: "jsx",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/jsx/highlights.scm"),
    },
    QueryAsset {
        language: "jsx",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/jsx/indents.scm"),
    },
    QueryAsset {
        language: "jsx",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/jsx/injections.scm"),
    },
    QueryAsset {
        language: "latex",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/latex/folds.scm"),
    },
    QueryAsset {
        language: "latex",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/latex/highlights.scm"),
    },
    QueryAsset {
        language: "latex",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/latex/injections.scm"),
    },
    QueryAsset {
        language: "lua",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/lua/folds.scm"),
    },
    QueryAsset {
        language: "lua",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/lua/highlights.scm"),
    },
    QueryAsset {
        language: "lua",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/lua/indents.scm"),
    },
    QueryAsset {
        language: "lua",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/lua/injections.scm"),
    },
    QueryAsset {
        language: "lua",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/lua/locals.scm"),
    },
    QueryAsset {
        language: "markdown",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/markdown/folds.scm"),
    },
    QueryAsset {
        language: "markdown",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/markdown/highlights.scm"),
    },
    QueryAsset {
        language: "markdown",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/markdown/indents.scm"),
    },
    QueryAsset {
        language: "markdown",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/markdown/injections.scm"),
    },
    QueryAsset {
        language: "markdown_inline",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/markdown_inline/highlights.scm"),
    },
    QueryAsset {
        language: "markdown_inline",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/markdown_inline/injections.scm"),
    },
    QueryAsset {
        language: "nu",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/nu/folds.scm"),
    },
    QueryAsset {
        language: "nu",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/nu/highlights.scm"),
    },
    QueryAsset {
        language: "nu",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/nu/indents.scm"),
    },
    QueryAsset {
        language: "nu",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/nu/injections.scm"),
    },
    QueryAsset {
        language: "query",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/query/folds.scm"),
    },
    QueryAsset {
        language: "query",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/query/highlights.scm"),
    },
    QueryAsset {
        language: "query",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/query/indents.scm"),
    },
    QueryAsset {
        language: "query",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/query/injections.scm"),
    },
    QueryAsset {
        language: "query",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/query/locals.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/rust/folds.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/rust/highlights.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/rust/indents.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/rust/injections.scm"),
    },
    QueryAsset {
        language: "rust",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/rust/locals.scm"),
    },
    QueryAsset {
        language: "slang",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/slang/folds.scm"),
    },
    QueryAsset {
        language: "slang",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/slang/highlights.scm"),
    },
    QueryAsset {
        language: "slang",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/slang/indents.scm"),
    },
    QueryAsset {
        language: "slang",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/slang/injections.scm"),
    },
    QueryAsset {
        language: "slang",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/slang/locals.scm"),
    },
    QueryAsset {
        language: "toml",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/toml/folds.scm"),
    },
    QueryAsset {
        language: "toml",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/toml/highlights.scm"),
    },
    QueryAsset {
        language: "toml",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/toml/indents.scm"),
    },
    QueryAsset {
        language: "toml",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/toml/injections.scm"),
    },
    QueryAsset {
        language: "toml",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/toml/locals.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/tsx/folds.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/tsx/highlights.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/tsx/indents.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/tsx/injections.scm"),
    },
    QueryAsset {
        language: "tsx",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/tsx/locals.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/typescript/folds.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/typescript/highlights.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/typescript/indents.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/typescript/injections.scm"),
    },
    QueryAsset {
        language: "typescript",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/typescript/locals.scm"),
    },
    QueryAsset {
        language: "vim",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/vim/folds.scm"),
    },
    QueryAsset {
        language: "vim",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/vim/highlights.scm"),
    },
    QueryAsset {
        language: "vim",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/vim/injections.scm"),
    },
    QueryAsset {
        language: "vim",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/vim/locals.scm"),
    },
    QueryAsset {
        language: "vimdoc",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/vimdoc/highlights.scm"),
    },
    QueryAsset {
        language: "vimdoc",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/vimdoc/injections.scm"),
    },
    QueryAsset {
        language: "wgsl",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/wgsl/folds.scm"),
    },
    QueryAsset {
        language: "wgsl",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/wgsl/highlights.scm"),
    },
    QueryAsset {
        language: "wgsl",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/wgsl/indents.scm"),
    },
    QueryAsset {
        language: "wgsl",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/wgsl/injections.scm"),
    },
    QueryAsset {
        language: "yaml",
        kind: "folds",
        origin: "upstream",
        text: include_str!("../../query/upstream/yaml/folds.scm"),
    },
    QueryAsset {
        language: "yaml",
        kind: "highlights",
        origin: "upstream",
        text: include_str!("../../query/upstream/yaml/highlights.scm"),
    },
    QueryAsset {
        language: "yaml",
        kind: "indents",
        origin: "upstream",
        text: include_str!("../../query/upstream/yaml/indents.scm"),
    },
    QueryAsset {
        language: "yaml",
        kind: "injections",
        origin: "upstream",
        text: include_str!("../../query/upstream/yaml/injections.scm"),
    },
    QueryAsset {
        language: "yaml",
        kind: "locals",
        origin: "upstream",
        text: include_str!("../../query/upstream/yaml/locals.scm"),
    },
];
