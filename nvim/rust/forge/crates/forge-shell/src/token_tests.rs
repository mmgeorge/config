use super::*;

#[test]
fn quoted_arguments_decode_across_shells() {
    for (shell, source, expected) in [
        (CommandShell::PowerShell, "probe 'it''s data'", "it's data"),
        (CommandShell::PowerShell, "probe \"a`\"b\"", "a\"b"),
        (CommandShell::PowerShell, "probe \"a\"\"b\"", "a\"b"),
        (CommandShell::PowerShell, "probe \"a`nb`t\"", "a\nb\t"),
        (CommandShell::PowerShell, "probe \"a`u{1f980}b\"", "a🦀b"),
        (CommandShell::PowerShell, "probe \"a`\nb\"", "a\nb"),
        (CommandShell::PowerShell, "probe 'a`nb\\path'", "a`nb\\path"),
        (CommandShell::PowerShell, "probe \"\"", ""),
        (CommandShell::Bash, "probe 'it'\\''s data'", "it's data"),
        (CommandShell::Bash, "probe a\\\nb", "ab"),
        (CommandShell::Bash, "probe \"a\\\nb\"", "ab"),
        (CommandShell::Bash, "probe \"a\\qb\"", "a\\qb"),
        (CommandShell::Bash, "probe \"a\\\"b\"", "a\"b"),
        (CommandShell::Bash, "probe a\\|b", "a|b"),
        (CommandShell::Bash, "probe ''", ""),
        (CommandShell::Zsh, "probe 'it'\\''s data'", "it's data"),
        (CommandShell::Zsh, "probe \"a\\qb\"", "a\\qb"),
        (CommandShell::Zsh, "probe \"a\\\"b\"", "a\"b"),
        (CommandShell::Zsh, "probe a\\|b", "a|b"),
        (CommandShell::Zsh, "probe ''", ""),
        (CommandShell::Nushell, "probe `a path`", "a path"),
        (CommandShell::Nushell, "probe \"a\\nb\\t\"", "a\nb\t"),
        (CommandShell::Nushell, "probe \"a\\u{1f980}b\"", "a🦀b"),
        (CommandShell::Nushell, "probe 'a\\nb'", "a\\nb"),
        (CommandShell::Nushell, "probe \"a\\\"b\"", "a\"b"),
        (CommandShell::Nushell, "probe ''", ""),
    ] {
        let decoded = shell_token_list(source, shell).unwrap();
        assert_eq!(decoded[1], expected, "{shell:?}: {source}");
    }
}

#[test]
fn lexical_escapes_preserve_arguments_and_rejected_syntax_stays_rejected() {
    for (shell, source, expected) in [
        (CommandShell::PowerShell, "probe a`|b", "a|b"),
        (CommandShell::PowerShell, "probe a`\nb", "a\nb"),
        (CommandShell::Zsh, "probe a\\\nb", "ab"),
        (CommandShell::Zsh, "probe \"a\\\nb\"", "ab"),
    ] {
        let tokens = shell_token_list(source, shell).unwrap();
        assert_eq!(tokens, ["probe", expected]);
        let parsed = normalize_command_in(source, shell);
        if shell == CommandShell::PowerShell {
            assert!(parsed.failure.is_none());
            assert_eq!(parsed.invocation_list.len(), 1);
            assert_eq!(parsed.invocation_list[0].token_list, tokens);
        } else {
            assert!(parsed.failure.is_some());
        }
    }
}
