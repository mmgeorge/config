use crate::permissions::command::normalize_command_in;
use crate::permissions::document::PermissionDecision;
use crate::permissions::document::parse_permission_document;
use crate::permissions::matcher::{
    CompiledPermissionDocument, PermissionRequest, PermissionTarget,
};
use crate::session::ApprovalMode;
use forge_shell::shell::*;

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
        if parsed.failure.is_some() {
            failure_list.push(format!("{source}\n{parsed:?}"));
            continue;
        }
        for invocation in &parsed.invocation_list {
            assert_eq!(
                source.get(invocation.source_range.clone()),
                Some(invocation.source.as_str()),
                "source coordinates lost their executable occurrence: {source}"
            );
        }
        let request = PermissionRequest {
            id: "matrix".into(),
            provider: forge_provider_api::ProviderId::new("test")
                .expect("static provider identity is valid"),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: source.clone(),
                shell: Some(shell),
            }],
        };
        let allowed = policy("allow", "allow").evaluate(&request).unwrap();
        let denied = policy("allow", "deny").evaluate(&request).unwrap();
        let asked = policy("allow", "ask").evaluate(&request).unwrap();
        if parsed.failure.is_some() || allowed.decision != PermissionDecision::Allow
            || denied.decision != PermissionDecision::Deny || asked.decision != PermissionDecision::Ask
            || asked.pending_target_list.is_empty()
            || !asked.pending_target_list.iter().all(|target| matches!(target,
                PermissionTarget::Command { command, .. } if command.trim_start_matches('^').starts_with("probe"))) {
            failure_list.push(format!("{source}\n{parsed:?}"));
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
            provider: forge_provider_api::ProviderId::new("test")
                .expect("static provider identity is valid"),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: (*source).into(),
                shell: Some(shell),
            }],
        };
        for mode in [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow] {
            if policy("allow", "allow").evaluate(&request).is_ok() {
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
            "probe one, two; & $command value",
            "probe one, $(Invoke-Expression $code), two",
            "probe one, (custom-wrapper { probe hidden }), two",
            "probe one, two > file",
            "probe one, two; $env:PATH = 'bad'",
            "probe one, two; probe 'unfinished",
            "probe one, two,",
            "probe one, , two",
            "probe a`\r\nInvoke-Expression $code",
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
            "overlay hide module",
            "overlay new module",
            "overlay list",
            "hide CHECK",
            "hide-env PATH",
            "probe out> file",
            "$env.PATH = []; probe",
            "with-env {PATH: []} { probe }",
            "nu --commands $code",
        ],
    );
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
            .failure
            .is_some()
        );
    }
    assert!(
        normalize_command_in("probe", CommandShell::Unknown)
            .failure
            .is_some()
    );
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
            provider: forge_provider_api::ProviderId::new("test")
                .expect("static provider identity is valid"),
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
            compiled.evaluate(&request).unwrap().decision,
            PermissionDecision::Allow
        );
        let document =
            serde_json::json!({"permission":{"bash":{"*":"allow",format!("{wrapper} *"):"deny"}}});
        let compiled = CompiledPermissionDocument::compile(
            parse_permission_document(&document.to_string()).unwrap(),
            ".",
        );
        assert_eq!(
            compiled.evaluate(&request).unwrap().decision,
            PermissionDecision::Deny
        );
    }
}

#[test]
fn unhandled_string_encodings_fail_closed_instead_of_losing_quoting() {
    reject_matrix(
        CommandShell::Bash,
        &["probe $'a\\nb'", "probe $\"translated\""],
    );
    reject_matrix(
        CommandShell::Zsh,
        &["probe $'a\\nb'", "probe a\\\nb", "probe \"a\\\nb\""],
    );
    reject_matrix(
        CommandShell::PowerShell,
        &["probe @'\na|b\n'@", "probe “a|b”"],
    );
    reject_matrix(
        CommandShell::Nushell,
        &[
            "probe r#'a|b'#",
            "probe \"a\\qb\"",
            "probe \"a\\u{110000}b\"",
        ],
    );
}
