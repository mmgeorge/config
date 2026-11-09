//! Canonical provider identities, aliases, labels, and default selection.

use forge_provider_api::{
    ProviderCapability, ProviderDescriptor, ProviderId, ProviderIntegration,
    capability::{CapabilityId, CapabilityRestriction, FeatureLimitation, Support},
};
use forge_provider_api::{
    approval::ApprovalMode, catalog::CatalogCapability, compaction::CompactionCapability,
};
use serde::Serialize;
use std::{path::PathBuf, sync::Arc};
use thiserror::Error;

/// Describes a provider identity available to generic consumers.
#[derive(Clone, Debug, Serialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct ProviderRegistration {
    /// Opaque canonical identity persisted by Harness.
    pub id: ProviderId,
    /// User-facing display label.
    pub label: &'static str,
    /// User-facing integration detail shown in provider selection.
    pub detail: &'static str,
    /// Whether interactive selection may offer this entry.
    pub selectable: bool,
    /// Normalized capability declarations owned by the native registry.
    pub capability: ProviderCapability,
    /// Default executable command for editor launch configuration.
    pub command: &'static [&'static str],
    /// Registry-owned defaults for background generation, when supported.
    pub background_generation: Option<BackgroundGenerationDefaults>,
}

/// Retain immutable registration metadata together with its adapter and fixture controls.
#[derive(Clone)]
pub struct RegisteredProvider {
    descriptor: ProviderDescriptor,
    adapter: Arc<dyn forge_provider_api::provider::Provider>,
    test_control: Option<Arc<dyn forge_provider_api::testing::ProviderTestControl>>,
}

impl RegisteredProvider {
    /// Create a registration, including explicit metadata overrides for generic fixtures.
    pub fn new(
        descriptor: ProviderDescriptor,
        adapter: Arc<dyn forge_provider_api::provider::Provider>,
        test_control: Option<Arc<dyn forge_provider_api::testing::ProviderTestControl>>,
    ) -> Self {
        Self {
            descriptor,
            adapter,
            test_control,
        }
    }

    /// Get immutable identity and baseline support without starting provider work.
    pub fn descriptor(&self) -> &ProviderDescriptor {
        &self.descriptor
    }

    /// Get the adapter installed for this registration.
    pub fn adapter(&self) -> &Arc<dyn forge_provider_api::provider::Provider> {
        &self.adapter
    }

    /// Get optional provider-neutral deterministic controls.
    pub fn test_control(
        &self,
    ) -> Option<&Arc<dyn forge_provider_api::testing::ProviderTestControl>> {
        self.test_control.as_ref()
    }
}

/// Default model and reasoning settings for background generation.
#[derive(Clone, Debug, Serialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct BackgroundGenerationDefaults {
    /// Preferred model identifier.
    pub model: &'static str,
    /// Preferred reasoning level.
    pub reasoning_effort: &'static str,
}

/// Reports identity lookup failures without asking callers to inspect provider names.
#[derive(Debug, Error)]
pub enum ProviderResolutionError {
    /// The registry does not contain the supplied identity or alias.
    #[error("unknown provider identity: {0}")]
    Unknown(String),
}

/// Construct the selected native provider without passing host-owned policies or services.
pub fn create(
    launch: forge_provider_api::ProviderLaunch,
    runtime_root: PathBuf,
) -> Result<RegisteredProvider, forge_provider_api::ProviderError> {
    let id = &launch.kind;
    resolve(id).map_err(|_| {
        forge_provider_api::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::ThreadOpen,
            forge_provider_api::ProviderFailureKind::Initialization,
        ))
    })?;
    let metadata = descriptor(id).expect("resolved identities have metadata");
    let mut test_control = None;
    let provider: Arc<dyn forge_provider_api::provider::Provider> = match id.as_str() {
        "codex" => Arc::new(crate::codex::provider::CodexProvider::new(
            launch.command,
            runtime_root,
        )?),
        "copilot" => Arc::new(crate::copilot::provider::CopilotProvider::new(
            launch.command,
            runtime_root,
        )?),
        "claude" | "antigravity" => Arc::new(crate::acp::provider::AcpProvider::new(
            launch.kind,
            launch.command,
            runtime_root,
        )?),
        "mock" => {
            let provider = crate::mock::MockProvider::new()?;
            test_control = Some(provider.fixture_control());
            Arc::new(provider)
        }
        _ => unreachable!("resolved registry identities have constructors"),
    };
    Ok(RegisteredProvider::new(metadata, provider, test_control))
}

struct RegistryEntry {
    id: &'static str,
    label: &'static str,
    detail: &'static str,
    selectable: bool,
    aliases: &'static [&'static str],
    command: &'static [&'static str],
    background_generation: Option<BackgroundGenerationDefaults>,
}

impl RegistryEntry {
    fn registration(&self) -> ProviderRegistration {
        let Self {
            id,
            label,
            detail,
            selectable,
            aliases: _,
            command,
            background_generation,
        } = self;
        ProviderRegistration {
            id: ProviderId::new(*id).expect("registry IDs are valid"),
            label,
            detail,
            selectable: *selectable,
            capability: capability(id),
            command,
            background_generation: background_generation.clone(),
        }
    }
}

const REGISTRATIONS: &[RegistryEntry] = &[
    RegistryEntry {
        id: "codex",
        label: "Codex CLI",
        detail: "OpenAI Codex app-server",
        selectable: true,
        aliases: &[],
        command: &["codex", "app-server"],
        background_generation: Some(BackgroundGenerationDefaults {
            model: "gpt-6-luna",
            reasoning_effort: "low",
        }),
    },
    RegistryEntry {
        id: "copilot",
        label: "Copilot CLI",
        detail: "GitHub Copilot SDK",
        selectable: true,
        aliases: &[],
        command: &[],
        background_generation: Some(BackgroundGenerationDefaults {
            model: "gpt-6-luna",
            reasoning_effort: "low",
        }),
    },
    RegistryEntry {
        id: "claude",
        label: "Claude Code",
        detail: "Claude Code through ACP",
        selectable: true,
        aliases: &[],
        command: &["claude-agent-acp"],
        background_generation: Some(BackgroundGenerationDefaults {
            model: "haiku",
            reasoning_effort: "low",
        }),
    },
    RegistryEntry {
        id: "antigravity",
        label: "Antigravity",
        detail: "Standalone Antigravity ACP server",
        selectable: true,
        aliases: &["agy"],
        command: &["agy_acp_server"],
        background_generation: None,
    },
    RegistryEntry {
        id: "mock",
        label: "Mock",
        detail: "Harness test provider",
        selectable: false,
        aliases: &[],
        command: &["mock"],
        background_generation: None,
    },
];

/// Resolve a canonical ID or registered legacy alias to its canonical identity.
pub fn select(value: &str) -> Result<ProviderId, ProviderResolutionError> {
    REGISTRATIONS
        .iter()
        .find(|entry| entry.id == value || entry.aliases.contains(&value))
        .map(|entry| ProviderId::new(entry.id).expect("registry IDs are valid"))
        .ok_or_else(|| ProviderResolutionError::Unknown(value.to_owned()))
}

/// Resolve an opaque identity to its registry registration.
pub fn resolve(id: &ProviderId) -> Result<ProviderRegistration, ProviderResolutionError> {
    REGISTRATIONS
        .iter()
        .find(|entry| entry.id == id.as_str())
        .map(RegistryEntry::registration)
        .ok_or_else(|| ProviderResolutionError::Unknown(id.to_string()))
}

/// List registry-owned identities in presentation order.
pub fn descriptors() -> Vec<ProviderRegistration> {
    REGISTRATIONS
        .iter()
        .map(RegistryEntry::registration)
        .collect()
}

/// Get the configured initial provider identity.
pub fn default_id() -> ProviderId {
    select("codex").expect("the registry default is registered")
}

/// Get the provider Background generation uses when settings name none: the registry default
/// when it supports Background sessions, otherwise the first selectable provider that does.
pub fn background_default_id() -> Option<ProviderId> {
    let supports = |entry: &&RegistryEntry| capability(entry.id).integration.background_generation;
    REGISTRATIONS
        .iter()
        .find(|entry| entry.id == default_id().as_str())
        .filter(supports)
        .or_else(|| {
            REGISTRATIONS
                .iter()
                .filter(|entry| entry.selectable)
                .find(supports)
        })
        .map(|entry| ProviderId::new(entry.id).expect("registry IDs are valid"))
}

/// Get the first provider identity excluded from interactive selection, when one exists.
pub fn nonselectable_id() -> Option<ProviderId> {
    REGISTRATIONS
        .iter()
        .find(|entry| !entry.selectable)
        .map(|entry| ProviderId::new(entry.id).expect("registry IDs are valid"))
}

/// Get another selectable identity for neutral identity-separation fixtures.
pub fn alternate_id(id: &ProviderId) -> Option<ProviderId> {
    REGISTRATIONS
        .iter()
        .find(|entry| entry.selectable && entry.id != id.as_str())
        .map(|entry| ProviderId::new(entry.id).expect("registry IDs are valid"))
}

/// Resolve an opaque identity to its complete provider descriptor.
pub fn descriptor(id: &ProviderId) -> Result<ProviderDescriptor, ProviderResolutionError> {
    let registration = resolve(id)?;
    Ok(ProviderDescriptor {
        kind: registration.id,
        label: registration.label.to_owned(),
        capability: registration.capability,
    })
}

fn capability(id: &str) -> ProviderCapability {
    let approvals = vec![ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow];
    let catalog = CatalogCapability {
        skill: true,
        mcp: true,
    };
    match id {
        "codex" => ProviderCapability {
            integration: ProviderIntegration {
                background_generation: true,
                isolated_generation: true,
                terminal_termination: true,
                native_agent_suppression: true,
                mcp_runtime_status: true,
                mcp_tool_catalog: true,
                tracked_submission: true,
                resource_target: Some("codexcli".into()),
                isolation_modes: if cfg!(windows) {
                    vec![
                    forge_provider_api::model::IsolationMode { id: "elevated".into(), label: "Elevated".into(), description: "Dedicated low-privilege users and filesystem ACLs. Setup needs admin approval; sandboxed commands do not run as administrator.".into() },
                    forge_provider_api::model::IsolationMode { id: "unelevated".into(), label: "Unelevated".into(), description: "Restricted token for the current user without admin setup. Weaker network isolation; denied-read paths are unsupported.".into() },
                    forge_provider_api::model::IsolationMode { id: "mxc".into(), label: "MXC".into(), description: "Prefer Microsoft Execution Containers where supported; falls back to Elevated. Child processes stop when the command exits.".into() },
                ]
                } else {
                    Vec::new()
                },
                limitations: vec![FeatureLimitation {
                    id: CapabilityId::FilesystemAccess,
                    support: Support::Limited {
                        restrictions: vec![
                            CapabilityRestriction::PermissionCallbacksOnly,
                            CapabilityRestriction::UnconfinedReads,
                        ],
                    },
                }],
                ..Default::default()
            },
            os_sandbox: true,
            native_fork: true,
            native_compact: true,
            compaction: CompactionCapability {
                lifecycle: true,
                background: false,
                summary: false,
                metrics: false,
            },
            native_steer: true,
            native_turn_rollback: true,
            native_conversation_rewind: true,
            model_selection: true,
            effort_selection: true,
            fast_mode: true,
            ultrafast_mode: true,
            permission_control: true,
            background_terminals: true,
            approval_mode_list: approvals,
            agent: Default::default(),
            catalog,
        },
        "copilot" => ProviderCapability {
            integration: ProviderIntegration {
                background_generation: true,
                isolated_generation: true,
                terminal_termination: true,
                native_agent_suppression: true,
                mcp_runtime_status: true,
                mcp_tool_catalog: true,
                tracked_submission: true,
                resource_target: Some("copilotcli".into()),
                ..Default::default()
            },
            native_fork: true,
            native_compact: true,
            compaction: CompactionCapability {
                lifecycle: true,
                background: true,
                summary: true,
                metrics: true,
            },
            native_steer: true,
            native_turn_rollback: true,
            native_conversation_rewind: true,
            model_selection: true,
            effort_selection: true,
            permission_control: true,
            background_terminals: true,
            approval_mode_list: approvals,
            agent: Default::default(),
            catalog,
            ..Default::default()
        },
        "claude" | "antigravity" => {
            let claude = id == "claude";
            ProviderCapability {
                integration: ProviderIntegration {
                    background_generation: claude,
                    tracked_submission: true,
                    native_agent_suppression: claude,
                    resource_target: Some(
                        if claude {
                            "claudecode"
                        } else {
                            "antigravity-cli"
                        }
                        .into(),
                    ),
                    limitations: if claude {
                        vec![]
                    } else {
                        vec![
                            FeatureLimitation {
                                id: CapabilityId::FileDiff,
                                support: Support::Limited {
                                    restrictions: vec![CapabilityRestriction::UnstructuredDiffs],
                                },
                            },
                            FeatureLimitation {
                                id: CapabilityId::PermissionCallbacks,
                                support: Support::Limited {
                                    restrictions: vec![
                                        CapabilityRestriction::EnterpriseTerminalPolicy,
                                    ],
                                },
                            },
                            FeatureLimitation {
                                id: CapabilityId::SkillCatalog,
                                support: Support::Limited {
                                    restrictions: vec![CapabilityRestriction::InheritedSkills],
                                },
                            },
                            FeatureLimitation {
                                id: CapabilityId::SkillMutation,
                                support: Support::Unsupported,
                            },
                            FeatureLimitation {
                                id: CapabilityId::McpMutation,
                                support: Support::Unsupported,
                            },
                        ]
                    },
                    ..Default::default()
                },
                native_fork: claude,
                native_compact: claude,
                native_steer: claude,
                model_selection: true,
                effort_selection: claude,
                permission_control: true,
                catalog,
                approval_mode_list: approvals,
                compaction: CompactionCapability {
                    lifecycle: claude,
                    summary: claude,
                    ..Default::default()
                },
                ..Default::default()
            }
        }
        "mock" => ProviderCapability {
            integration: ProviderIntegration {
                background_generation: true,
                isolated_generation: true,
                terminal_termination: false,
                native_agent_suppression: true,
                mcp_runtime_status: true,
                mcp_tool_catalog: true,
                local_acceptance: true,
                resource_target: Some("mock".into()),
                ..Default::default()
            },
            native_fork: true,
            native_compact: true,
            compaction: CompactionCapability {
                lifecycle: true,
                background: false,
                summary: true,
                metrics: true,
            },
            native_steer: true,
            native_turn_rollback: true,
            native_conversation_rewind: true,
            model_selection: true,
            effort_selection: true,
            fast_mode: true,
            ultrafast_mode: true,
            permission_control: true,
            background_terminals: false,
            approval_mode_list: approvals,
            agent: Default::default(),
            catalog: Default::default(),
            ..Default::default()
        },
        _ => unreachable!("registry only calls capability for known identities"),
    }
}
