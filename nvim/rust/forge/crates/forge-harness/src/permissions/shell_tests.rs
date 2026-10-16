use super::*;
use crate::permissions::command::normalize_command_in;
use crate::permissions::document::PermissionDecision;
use crate::permissions::document::parse_permission_document;
use crate::permissions::matcher::{
    CompiledPermissionDocument, PermissionRequest, PermissionTarget,
};
use crate::session::PermissionMode;

fn policy(default: &str, probe: &str) -> CompiledPermissionDocument {
    let document = serde_json::json!({"permission": {"bash": {"*": default, "probe *": probe}}});
    CompiledPermissionDocument::compile(
        parse_permission_document(&document.to_string()).unwrap(),
        ".",
    )
}

fn check_matrix(shell: CommandShell, fixture_list: &[&str]) {
    let mut failure_list = Vec::new();
    for fixture in fixture_list {
        let source = fixture.replace("CHECK", "probe");
        let parsed = normalize_command_in(&source, shell);
        for invocation in &parsed.invocation_list {
            assert_eq!(
                source.get(invocation.source_range.clone()),
                Some(invocation.source.as_str()),
                "source coordinates lost their executable occurrence: {source}"
            );
        }
        let request = PermissionRequest {
            id: "matrix".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: source.clone(),
                shell: Some(shell),
            }],
        };
        let allowed = policy("allow", "allow")
            .evaluate(PermissionMode::Write, &request)
            .unwrap();
        let denied = policy("allow", "deny")
            .evaluate(PermissionMode::Write, &request)
            .unwrap();
        let asked = policy("allow", "ask")
            .evaluate(PermissionMode::Write, &request)
            .unwrap();
        if parsed.ambiguous || allowed.decision != PermissionDecision::Allow
            || denied.decision != PermissionDecision::Deny || asked.decision != PermissionDecision::Ask
            || asked.pending_target_list.is_empty()
            || !asked.pending_target_list.iter().all(|target| matches!(target,
                PermissionTarget::Command { command, .. } if command.trim_start_matches('^').starts_with("probe"))) {
            let mut parser = Parser::new();
            parser.set_language(&shell.grammar().unwrap()).unwrap();
            let tree = parser.parse(&source, None).unwrap();
            failure_list.push(format!("{source}\n{parsed:?}\n{}", tree.root_node().to_sexp()));
        }
    }
    assert!(
        failure_list.is_empty(),
        "{shell:?}: {} failures\n{}",
        failure_list.len(),
        failure_list.join("\n\n")
    );
}

fn reject_matrix(shell: CommandShell, fixture_list: &[&str]) {
    let mut failure_list = Vec::new();
    for source in fixture_list {
        let request = PermissionRequest {
            id: "matrix".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: (*source).into(),
                shell: Some(shell),
            }],
        };
        for mode in [
            PermissionMode::Read,
            PermissionMode::Write,
            PermissionMode::Yolo,
        ] {
            if policy("allow", "allow").evaluate(mode, &request).is_ok() {
                failure_list.push(format!("{mode:?}: {source}"));
            }
        }
    }
    assert!(
        failure_list.is_empty(),
        "{shell:?} accepted unresolved syntax: {failure_list:?}"
    );
}

#[test]
fn powershell_commands_nested_in_every_supported_construct_obey_policy() {
    check_matrix(
        CommandShell::PowerShell,
        &[
            "CHECK --version",
            "CHECK 'a;b|c&d'",
            "CHECK \"a;b|c\"",
            "CHECK 'it''s data'",
            "CHECK one; CHECK two",
            "CHECK one | CHECK two",
            "CHECK one && CHECK two",
            "CHECK one || CHECK two",
            "# comment containing danger\nCHECK value # comment",
            "Get-ChildItem registry | ForEach-Object { CHECK $_.FullName -Filter 'bevy*' }",
            "1 | % { CHECK $_ }",
            "1 | foreach { CHECK $_ }",
            "1 | Where-Object { CHECK $_ }",
            "1 | ? { CHECK $_ }",
            "ForEach-Object -Begin { CHECK begin } -Process { CHECK process } -End { CHECK end }",
            "ForEach-Object { ForEach-Object { CHECK nested } }",
            "if ($true) { CHECK yes } else { CHECK no }",
            "if ($false) { Write-Output skip } elseif ($true) { CHECK yes }",
            "if (CHECK condition) { Write-Output yes }",
            "foreach ($entry in @(1,2)) { CHECK $entry }",
            "for ($index=0; $index -lt 2; $index++) { CHECK $index }",
            "while ($true) { CHECK body; break }",
            "do { CHECK body } while ($false)",
            "do { CHECK body } until ($true)",
            "switch ('a') { 'a' { CHECK match }; default { CHECK other } }",
            "try { CHECK body } catch { CHECK error } finally { CHECK cleanup }",
            "Write-Output $(CHECK nested)",
            "Write-Output (CHECK nested)",
            "Write-Output \"value $(CHECK nested)\"",
            "$value = CHECK nested; Write-Output $value",
        ],
    );
}

#[test]
fn powershell_comma_arguments_keep_independent_permissions_and_source_ranges() {
    let source = "Get-ChildItem D:/.local/share/nvim/forge,D:/.cache/nvim/forge -ErrorAction SilentlyContinue |\n  Select-Object FullName,LastWriteTime;\n  rg -n 'markdown|wrap|table|timeline|database' .agents/rules/forge.md .agents/rules/nvim.md";
    let parsed = normalize_command_in(source, CommandShell::PowerShell);
    assert!(!parsed.ambiguous, "{parsed:?}");
    assert_eq!(parsed.invocation_list.len(), 3);
    for invocation in &parsed.invocation_list {
        assert_eq!(&source[invocation.source_range.clone()], invocation.source,);
        for decision in ["ask", "deny"] {
            let mut rules = serde_json::json!({"*": "allow"});
            rules[format!("{} *", invocation.command_name)] = decision.into();
            let document = serde_json::json!({"permission": {"bash": rules}});
            let compiled = CompiledPermissionDocument::compile(
                parse_permission_document(&document.to_string()).unwrap(),
                ".",
            );
            let request = PermissionRequest {
                id: "comma-arguments".into(),
                provider: "test".into(),
                reason: None,
                target_list: vec![PermissionTarget::Command {
                    command: source.into(),
                    shell: Some(CommandShell::PowerShell),
                }],
            };
            let result = compiled.evaluate(PermissionMode::Write, &request).unwrap();
            assert_eq!(
                result.decision,
                if decision == "ask" {
                    PermissionDecision::Ask
                } else {
                    PermissionDecision::Deny
                }
            );
            if decision == "ask" {
                assert_eq!(
                    result.pending_target_list,
                    vec![PermissionTarget::Command {
                        command: invocation.source.clone(),
                        shell: Some(CommandShell::PowerShell),
                    }]
                );
            }
        }
    }
    check_matrix(
        CommandShell::PowerShell,
        &[
            "CHECK one,two,three",
            "CHECK -Path D:/one,D:/two -ErrorAction Stop",
            "CHECK café,世界",
            "CHECK 'one','two'",
            "CHECK one,two | CHECK three,four; CHECK five,six",
            "if ($true) { CHECK one,two } else { CHECK three,four }",
            "CHECK one,two; Write-Output $(CHECK nested)",
        ],
    );
}

#[test]
fn powershell_unresolved_execution_never_inherits_an_allow_rule() {
    reject_matrix(
        CommandShell::PowerShell,
        &[
            "",
            "  ",
            "CHECK 'unfinished",
            "ForEach-Object { CHECK value",
            "CHECK (value]",
            "& $command value",
            ". ./script.ps1",
            "Invoke-Expression $code",
            "iex 'probe'",
            "ForEach-Object -MemberName Delete",
            "ForEach-Object -Parallel { probe }",
            "ForEach-Object { probe } $script",
            "custom-wrapper { probe }",
            "function custom { probe }; custom",
            "[System.IO.File]::WriteAllText('path','text')",
            "$object.Delete()",
            "probe > file",
            "$env:PATH = 'bad'; probe",
            "do { probe } while (",
            "pwsh -EncodedCommand cAByAG8AYgBlAA==",
            "fish -c 'probe'",
            "probe one,two; & $command value",
            "probe one,two; Invoke-Expression $code",
            "probe one,two; probe 'unfinished",
            "probe one,two > file",
            "probe one,",
            "probe one,,two",
            "probe one,two; if ($true) { probe value",
        ],
    );
}

const POSIX_FIXTURE: &[&str] = &[
    "CHECK --version",
    "CHECK 'a;b|c&d'",
    "CHECK \"a;b|c\"",
    "CHECK 'it'\\''s data'",
    "CHECK one; CHECK two",
    "CHECK one | CHECK two",
    "CHECK one && CHECK two",
    "CHECK one || CHECK two",
    "# danger in a comment\nCHECK value # danger",
    "CHECK one\nCHECK two",
    "if true; then CHECK yes; else CHECK no; fi",
    "if CHECK condition; then true; fi",
    "if false; then true; elif true; then CHECK yes; fi",
    "for entry in one two; do CHECK \"$entry\"; done",
    "while true; do CHECK body; break; done",
    "until false; do CHECK body; break; done",
    "case value in value) CHECK match;; *) CHECK other;; esac",
    "{ CHECK group; }",
    "(CHECK subshell)",
    "! CHECK negated",
    "printf '%s' \"$(CHECK substitution)\"",
    "printf '%s' `CHECK substitution`",
    "cat <(CHECK process)",
    "CHECK \"${value:-literal}\"",
    "if true; then for entry in one; do (CHECK nested); done; fi",
];

const POSIX_REJECT: &[&str] = &[
    "",
    "CHECK 'unfinished",
    "if true; then CHECK value",
    "(CHECK value",
    "${command} argument",
    "\"$command\" argument",
    "$(printf probe) argument",
    "eval 'probe'",
    "source ./script",
    ". ./script",
    "custom() { probe; }; custom",
    "CHECK > file",
    "cat <<EOF\nvalue\nEOF",
    "PATH=/bad CHECK",
    "export PATH=/bad; CHECK",
    "unset PATH; CHECK",
    "bash -c \"$code\"",
    "sh ./script",
    "zsh -c 'probe' extra",
];

#[test]
fn bash_commands_nested_in_every_supported_construct_obey_policy() {
    check_matrix(CommandShell::Bash, POSIX_FIXTURE);
}

#[test]
fn bash_unresolved_execution_never_inherits_an_allow_rule() {
    reject_matrix(CommandShell::Bash, POSIX_REJECT);
}

#[test]
fn zsh_commands_nested_in_every_supported_construct_obey_policy() {
    check_matrix(CommandShell::Zsh, POSIX_FIXTURE);
    check_matrix(
        CommandShell::Zsh,
        &[
            "repeat 2 CHECK body",
            "for entry (one two) CHECK $entry",
            "{ CHECK body; } always { CHECK cleanup; }",
        ],
    );
}

#[test]
fn zsh_unresolved_execution_never_inherits_an_allow_rule() {
    reject_matrix(CommandShell::Zsh, POSIX_REJECT);
    reject_matrix(
        CommandShell::Zsh,
        &["print ${~pattern}", "print *(e:'probe':)"],
    );
}

#[test]
fn nushell_commands_nested_in_every_supported_construct_obey_policy() {
    check_matrix(
        CommandShell::Nushell,
        &[
            "CHECK --version",
            "CHECK 'a;b|c&d'",
            "CHECK \"a;b|c\"",
            "^CHECK external",
            "CHECK one; CHECK two",
            "CHECK one | CHECK two",
            "# danger\nCHECK value # danger",
            "[1 2] | each {|entry| CHECK $entry }",
            "[1 2] | par-each {|entry| CHECK $entry }",
            "[1 2] | where {|entry| CHECK $entry }",
            "[1 2] | filter {|entry| CHECK $entry }",
            "[1 2] | reduce {|entry, accumulator| CHECK $entry }",
            "[1 2] | any {|entry| CHECK $entry }",
            "[1 2] | all {|entry| CHECK $entry }",
            "[1] | each {|outer| [2] | each {|inner| CHECK $inner } }",
            "if true { CHECK yes } else { CHECK no }",
            "if (CHECK condition) { print yes }",
            "for entry in [1 2] { CHECK $entry }",
            "while true { CHECK body; break }",
            "loop { CHECK body; break }",
            "match 1 { 1 => { CHECK match }, _ => { CHECK other } }",
            "try { CHECK body } catch {|error| CHECK $error }",
            "do { CHECK block }",
            "print (CHECK nested)",
            "print $'value (CHECK nested)'",
            "let value = (CHECK nested); print $value",
            "CHECK ...[one two]",
            "[1] | each {|entry| if true { CHECK $entry } }",
        ],
    );
}

#[test]
fn nushell_unresolved_execution_never_inherits_an_allow_rule() {
    reject_matrix(
        CommandShell::Nushell,
        &[
            "",
            "CHECK 'unfinished",
            "each {|entry| CHECK $entry",
            "if true { CHECK value",
            "^$command value",
            "do $closure",
            "each { probe } $closure",
            "do --env { probe }",
            "run-external $command",
            "custom-wrapper { probe }",
            "def custom [] { probe }; custom",
            "alias custom = probe; custom",
            "source script.nu",
            "source-env script.nu",
            "use script.nu",
            "overlay use script.nu",
            "probe out> file",
            "$env.PATH = []; probe",
            "with-env {PATH: []} { probe }",
            "nu --commands $code",
            "nu script.nu",
        ],
    );
}

#[test]
fn parser_preserves_complete_commands_beyond_former_byte_limit() {
    let payload = "retained".repeat(9000);
    let source = format!("probe '{payload}'; probe after");
    assert!(source.len() > 65_536);
    for shell in [
        CommandShell::PowerShell,
        CommandShell::Bash,
        CommandShell::Zsh,
        CommandShell::Nushell,
    ] {
        let parsed = normalize_command_in(&source, shell);
        assert!(!parsed.ambiguous, "{shell:?}");
        assert_eq!(parsed.invocation_list.len(), 2, "{shell:?}");
        assert_eq!(parsed.invocation_list[0].token_list, ["probe", payload.as_str()], "{shell:?}");
        assert_eq!(parsed.invocation_list[1].source, "probe after", "{shell:?}");
        let final_command = source.rfind("probe").unwrap();
        assert!(highlights(&source, shell).iter().any(|capture|
            capture.group == "ForgeHarnessCommand" && capture.range.start >= final_command),
            "{shell:?}");
    }
}

#[test]
fn parser_structural_guards_fail_closed() {
    for shell in [
        CommandShell::PowerShell,
        CommandShell::Bash,
        CommandShell::Zsh,
        CommandShell::Nushell,
    ] {
        assert!(
            normalize_command_in(
                &format!("{}probe{}", "(".repeat(100), ")".repeat(100)),
                shell
            )
            .ambiguous
        );
    }
    assert!(normalize_command_in("probe", CommandShell::Unknown).ambiguous);
}

#[test]
fn explicit_shell_launchers_use_their_own_grammar() {
    for (launcher, expected_shell) in [
        (
            "pwsh -NoProfile -Command '1 | ForEach-Object { probe $_ }'",
            CommandShell::PowerShell,
        ),
        (
            "\"C:/Program Files/PowerShell/7/pwsh.exe\" -Command 'probe x'",
            CommandShell::PowerShell,
        ),
        (
            "/bin/bash -lc 'for x in one; do probe $x; done'",
            CommandShell::Bash,
        ),
        ("/bin/zsh -lc 'repeat 2 probe x'", CommandShell::Zsh),
        (
            "nu --no-config-file --no-history -c '[1] | each {|x| probe $x }'",
            CommandShell::Nushell,
        ),
    ] {
        let parsed = normalize_command_in(launcher, CommandShell::PowerShell);
        assert!(!parsed.ambiguous, "{launcher}: {parsed:?}");
        assert!(
            parsed
                .invocation_list
                .iter()
                .all(|invocation| invocation.shell == expected_shell)
        );
        assert!(
            parsed
                .invocation_list
                .iter()
                .any(|invocation| invocation.command_name == "probe")
        );
    }
}

#[test]
fn wrapper_denials_and_body_denials_cannot_be_bypassed() {
    for (shell, source, wrapper) in [
        (
            CommandShell::PowerShell,
            "1 | ForEach-Object { probe x }",
            "ForEach-Object",
        ),
        (CommandShell::Nushell, "[1] | each {|x| probe $x }", "each"),
    ] {
        let request = PermissionRequest {
            id: "wrapper".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: source.into(),
                shell: Some(shell),
            }],
        };
        let document = serde_json::json!({"permission":{"bash":{"probe *":"allow"}}});
        let compiled = CompiledPermissionDocument::compile(
            parse_permission_document(&document.to_string()).unwrap(),
            ".",
        );
        assert_eq!(
            compiled
                .evaluate(PermissionMode::Write, &request)
                .unwrap()
                .decision,
            PermissionDecision::Allow
        );
        let document =
            serde_json::json!({"permission":{"bash":{"*":"allow",format!("{wrapper} *"):"deny"}}});
        let compiled = CompiledPermissionDocument::compile(
            parse_permission_document(&document.to_string()).unwrap(),
            ".",
        );
        assert_eq!(
            compiled
                .evaluate(PermissionMode::Write, &request)
                .unwrap()
                .decision,
            PermissionDecision::Deny
        );
    }
}

#[test]
fn powershell_foreach_ranges_are_expressions_and_only_loop_commands_need_permission() {
    for range in ["1..1", "0..10", "-2..2", "5..1"] {
        let source = format!("foreach ($iteration in {range}) {{ whoami.exe /groups }}");
        let parsed = normalize_command_in(&source, CommandShell::PowerShell);
        assert!(!parsed.ambiguous, "{source}: {parsed:?}");
        assert_eq!(parsed.invocation_list.len(), 1, "{source}: {parsed:?}");
        assert_eq!(parsed.invocation_list[0].source, "whoami.exe /groups");
        let request = PermissionRequest {
            id: "range".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: source,
                shell: Some(CommandShell::PowerShell),
            }],
        };
        let document = serde_json::json!({"permission":{"bash":{"whoami *":"allow"}}});
        let compiled = CompiledPermissionDocument::compile(
            parse_permission_document(&document.to_string()).unwrap(),
            ".",
        );
        assert_eq!(
            compiled
                .evaluate(PermissionMode::Write, &request)
                .unwrap()
                .decision,
            PermissionDecision::Allow
        );
    }
}

#[test]
fn shell_quote_decoding_preserves_policy_arguments() {
    let mut failure_list = Vec::new();
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
        let decoded = super::super::command::shell_token_list(source, shell).unwrap();
        assert_eq!(decoded[1], expected, "{shell:?}: {source}");
        let parsed = normalize_command_in(source, shell);
        if parsed.ambiguous {
            failure_list.push(format!("{shell:?}: {source}"));
            continue;
        }
        if parsed.invocation_list[0].token_list[1] != expected {
            failure_list.push(format!("{shell:?}: {source}: {parsed:?}"));
            continue;
        }
        let pattern =
            super::super::command::exact_command_pattern(&parsed.invocation_list[0]).unwrap();
        assert_eq!(
            super::super::command::command_token_list(&pattern).unwrap(),
            parsed.invocation_list[0].token_list
        );
        assert_eq!(parsed.invocation_list[0].source, source);
    }
    assert!(failure_list.is_empty(), "{}", failure_list.join("\n"));
}

#[test]
fn unhandled_string_encodings_fail_closed_instead_of_losing_quoting() {
    reject_matrix(CommandShell::Bash, &["probe $'a\\nb'", "probe $\"translated\""]);
    reject_matrix(CommandShell::Zsh, &["probe $'a\\nb'", "probe a\\\nb", "probe \"a\\\nb\""]);
    reject_matrix(CommandShell::PowerShell, &[
        "probe @'\na|b\n'@", "probe “a|b”", "probe a`|b", "probe a`\nb",
    ]);
    reject_matrix(CommandShell::Nushell, &[
        "probe r#'a|b'#", "probe \"a\\qb\"", "probe \"a\\u{110000}b\"",
    ]);
}

#[test]
fn lexical_escapes_remain_exact_when_the_grammar_rejects_the_script() {
    for (shell, source, expected) in [
        (CommandShell::PowerShell, "probe a`|b", "a|b"),
        (CommandShell::PowerShell, "probe a`\nb", "a\nb"),
        (CommandShell::Zsh, "probe a\\\nb", "ab"),
        (CommandShell::Zsh, "probe \"a\\\nb\"", "ab"),
    ] {
        let tokens = super::super::command::shell_token_list(source, shell).unwrap();
        assert_eq!(tokens, ["probe", expected]);
        assert!(normalize_command_in(source, shell).ambiguous);
    }
}

#[test]
fn nested_launcher_coordinates_survive_quotes_escapes_and_unicode() {
    for (shell, source, expected) in [
        (
            CommandShell::PowerShell,
            "pwsh -Command 'probe ''it''''s data''; probe λ'",
            "probe ''it''''s data''",
        ),
        (
            CommandShell::Bash,
            r#"bash -c "probe \"a;b\"; probe λ""#,
            r#"probe \"a;b\""#,
        ),
        (CommandShell::Zsh, "zsh -c 'probe λ; probe two'", "probe λ"),
        (
            CommandShell::Nushell,
            "nu -c 'probe λ; probe two'",
            "probe λ",
        ),
        (
            CommandShell::Bash,
            r#"bash -c 'pwsh -Command "probe λ; probe two"'"#,
            "probe λ",
        ),
    ] {
        let parsed = normalize_command_in(source, shell);
        assert!(!parsed.ambiguous, "{source}: {parsed:?}");
        assert_eq!(parsed.invocation_list.len(), 2, "{source}");
        assert_eq!(
            &source[parsed.invocation_list[0].source_range.clone()],
            expected,
            "{source}"
        );
        assert_eq!(
            &source[parsed.invocation_list[1].source_range.clone()],
            if source.contains("probe two") {
                "probe two"
            } else {
                "probe λ"
            }
        );
    }
}

#[test]
fn every_shell_colors_syntax_without_treating_quoted_commands_as_execution() {
    for (shell, source) in [
        (
            CommandShell::PowerShell,
            "if ($true) { probe --flag 'probe fake'; Write-Output $value } # note",
        ),
        (
            CommandShell::Bash,
            "if true; then probe --flag 'probe fake' $value; fi # note",
        ),
        (
            CommandShell::Zsh,
            "if true; then probe --flag 'probe fake' $value; fi # note",
        ),
        (
            CommandShell::Nushell,
            "if true { probe --flag 'probe fake' $value } # note",
        ),
    ] {
        let capture_list = highlights(source, shell);
        for group in [
            "ForgeHarnessCommand",
            "ForgeHarnessOption",
            "String",
            "Identifier",
            "Keyword",
            "Comment",
        ] {
            assert!(
                capture_list.iter().any(|capture| capture.group == group),
                "{shell:?} lacks {group}: {capture_list:?}"
            );
        }
        for capture in &capture_list {
            assert!(
                source.get(capture.range.clone()).is_some(),
                "{shell:?}: {capture:?}"
            );
        }
        assert!(
            !capture_list
                .iter()
                .any(|capture| capture.group == "ForgeHarnessCommand"
                    && source[capture.range.clone()].contains("fake"))
        );
    }
}
