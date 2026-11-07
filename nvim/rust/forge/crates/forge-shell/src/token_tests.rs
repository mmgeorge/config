use super::*;

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
