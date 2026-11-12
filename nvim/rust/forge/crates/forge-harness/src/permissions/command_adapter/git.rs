use super::{CommandShell, literal_score, token_equal};

struct Projection<'a> {
    operation: Vec<&'a str>,
    directory_changes: Vec<&'a str>,
    no_pager: bool,
}

impl<'a> Projection<'a> {
    /// Parse only supported global options, preserving all operands and subcommand arguments.
    fn parse(tokens: &[&'a str], shell: CommandShell) -> Option<Self> {
        if !token_equal(tokens.first()?, "git", shell) {
            return None;
        }
        let mut projection = Self {
            operation: vec![tokens[0]],
            directory_changes: Vec::new(),
            no_pager: false,
        };
        let mut index = 1;
        while let Some(token) = tokens.get(index) {
            match *token {
                "-C" => {
                    projection.directory_changes.push(*tokens.get(index + 1)?);
                    index += 2;
                }
                "--no-pager" => {
                    projection.no_pager = true;
                    index += 1;
                }
                option if option.starts_with('-') => return None,
                _ => {
                    projection.operation.extend_from_slice(&tokens[index..]);
                    return Some(projection);
                }
            }
        }
        None
    }

    /// Require explicitly constrained context while leaving unqualified operation rules unscoped.
    fn context_matches(&self, actual: &Self, shell: CommandShell) -> bool {
        (!self.no_pager || actual.no_pager)
            && (self.directory_changes.is_empty()
                || (self.directory_changes.len() == actual.directory_changes.len()
                    && self
                        .directory_changes
                        .iter()
                        .zip(&actual.directory_changes)
                        .all(|(expected, actual)| token_equal(expected, actual, shell))))
    }
}

/// Apply operation rules across supported Git prefixes while retaining explicit rule specificity.
pub(super) fn pattern_score(
    pattern: &[&str],
    actual: &[&str],
    shell: CommandShell,
) -> Option<usize> {
    let expected = Projection::parse(pattern, shell)?;
    let actual = Projection::parse(actual, shell)?;
    if !expected.context_matches(&actual, shell) {
        return None;
    }
    literal_score(&expected.operation, &actual.operation, shell)?;
    // Context-bearing rules retain their original rank above generic operation rules.
    literal_score(pattern, pattern, shell)
}
