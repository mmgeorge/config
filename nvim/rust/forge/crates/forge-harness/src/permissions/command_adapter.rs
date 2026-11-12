use super::command::CommandInvocation;
use super::shell::CommandShell;

mod git;

/// Match literal tokens and any supported CLI projection without rewriting execution source.
pub(super) fn pattern_score(pattern: &[String], invocation: &CommandInvocation) -> Option<usize> {
    let pattern: Vec<&str> = pattern.iter().map(String::as_str).collect();
    let actual: Vec<&str> = invocation.token_list.iter().map(String::as_str).collect();
    let literal = literal_score(&pattern, &actual, invocation.shell);
    let adapted = match actual.first() {
        Some(program) if token_equal(program, "git", invocation.shell) => {
            git::pattern_score(&pattern, &actual, invocation.shell)
        }
        _ => None,
    };
    literal.max(adapted)
}

/// Score the existing literal-prefix rule, with a terminal wildcard worth zero extra literals.
fn literal_score(pattern: &[&str], actual: &[&str], shell: CommandShell) -> Option<usize> {
    let wildcard = pattern.last() == Some(&"*");
    let literal_count = pattern.len().saturating_sub(usize::from(wildcard));
    if literal_count == 0 {
        return (pattern.len() == 1 && wildcard).then_some(0);
    }
    if actual.len() < literal_count {
        return None;
    }
    pattern[..literal_count]
        .iter()
        .zip(actual)
        .all(|(expected, actual)| token_equal(expected, actual, shell))
        .then_some(literal_count * 2 + usize::from(!wildcard))
}

/// Preserve the shell's existing case sensitivity when comparing rule tokens.
fn token_equal(expected: &str, actual: &str, shell: CommandShell) -> bool {
    if shell == CommandShell::PowerShell {
        expected.eq_ignore_ascii_case(actual)
    } else {
        expected == actual
    }
}
