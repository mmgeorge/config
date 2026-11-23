use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Persists a child's resolved provider selection and inherited execution authority.
pub struct AgentSelection {
    /// Captures the selected profile so later edits cannot change this child.
    pub profile: String,
    pub instructions: String,
    /// Chooses the adapter independently of the root provider.
    pub provider: crate::provider::ProviderId,
    /// Retains the advertised model selected before task admission.
    pub model: String,
    /// Retains reasoning validated against that model's supported choices.
    pub reasoning_effort: String,
    /// Retains the selected provider context tier.
    pub context_window: Option<String>,
    /// Retains a compatible provider service tier.
    pub service_tier: crate::provider::ServiceTier,
    /// Prevents cross-provider creation from weakening the root's access policy.
    pub access: crate::session::AccessPolicy,
    /// Retains the root's effective approval mode.
    pub approval_mode: crate::session::ApprovalMode,
}

/// Narrow the current root policy by a child's admitted selection.
///
/// A child never gains write scope its selection gave up, such as a write-off profile, and a
/// root narrowed after admission still narrows the child. Every other dimension follows the
/// current root, which owns read scope, directory grants, and sandboxing.
pub(crate) fn effective_access(
    root: &crate::session::AccessPolicy,
    selection: &crate::session::AccessPolicy,
) -> crate::session::AccessPolicy {
    use crate::session::WriteScope;
    let rank = |scope: WriteScope| match scope {
        WriteScope::Off => 0,
        WriteScope::Workspace => 1,
        WriteScope::Full => 2,
    };
    let mut access = root.clone();
    if rank(selection.write_access) < rank(root.write_access) {
        access.write_access = selection.write_access;
    }
    access
}

/// Limit operations on the child's shared reservation to its effective access policy.
pub(crate) fn repository_access(
    access: &crate::session::AccessPolicy,
) -> crate::repository_access::RepositoryAccess {
    match access.write_access {
        crate::session::WriteScope::Off => crate::repository_access::RepositoryAccess::Read,
        crate::session::WriteScope::Workspace | crate::session::WriteScope::Full => {
            crate::repository_access::RepositoryAccess::Write
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::repository_access::RepositoryAccess;
    use crate::session::{AccessPolicy, WriteScope};

    fn policy(write_access: WriteScope) -> AccessPolicy {
        AccessPolicy {
            write_access,
            ..Default::default()
        }
    }

    /// The effective child policy, not profile wording, selects the reservation.
    #[test]
    fn repository_access_follows_effective_child_permissions() {
        for (root, selection, expected_write, expected_access) in [
            (
                WriteScope::Workspace,
                WriteScope::Workspace,
                WriteScope::Workspace,
                RepositoryAccess::Write,
            ),
            (
                WriteScope::Full,
                WriteScope::Workspace,
                WriteScope::Workspace,
                RepositoryAccess::Write,
            ),
            (
                WriteScope::Workspace,
                WriteScope::Off,
                WriteScope::Off,
                RepositoryAccess::Read,
            ),
            (
                WriteScope::Off,
                WriteScope::Workspace,
                WriteScope::Off,
                RepositoryAccess::Read,
            ),
            (
                WriteScope::Full,
                WriteScope::Full,
                WriteScope::Full,
                RepositoryAccess::Write,
            ),
        ] {
            let effective = effective_access(&policy(root), &policy(selection));
            assert_eq!(
                effective.write_access, expected_write,
                "{root:?} narrowed by {selection:?}"
            );
            assert_eq!(repository_access(&effective), expected_access);
        }
        let mut root = policy(WriteScope::Workspace);
        root.read_access = crate::session::DirectoryScope::Full;
        let effective = effective_access(&root, &policy(WriteScope::Off));
        assert_eq!(effective.read_access, crate::session::DirectoryScope::Full);
    }
}
