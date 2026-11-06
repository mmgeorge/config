# Provider API ownership follow-up

The provider API must describe provider operations and normalized observations, not Harness approval screens, workflow ownership, or scheduler implementation. Move host-only records out of `forge-provider-api`, keep worker records private to the runtime, and split records that currently combine semantic provider data with host presentation or policy.

This follow-up records the ownership audit of the structs in [the event-stream overhaul plan](eventstream-overhaul.md). It complements the four work items in [the existing follow-up document](evenstream-followup.md); it does not describe runtime changes already implemented. Some listed records already exist in the provider API, while others are proposed records that must never be placed there.

## Why these records do not belong in the provider API

The host-only records are not observations derived from a native provider. They describe higher-level Harness state shared across providers: approval presentation and saved policy, pending user decisions, selected-skill attribution, retry scheduling, and host session/workflow ownership. Harness creates and enforces these concepts, so requiring adapters to carry them makes the provider boundary depend on the application built above it.

The intended direction is provider operations and observations into Harness, followed by Harness-owned policy, workflow, and presentation decisions. A provider does not decide which host rule to save, which editor highlight group to use, which Harness goal phase is active, or how many retries the host has scheduled.

This is an ownership rule, not a requirement that every API value originate in a native notification. Host-assigned correlation IDs, semantic configuration, permission requests/responses, and normalized operation results legitimately cross the boundary. Private worker records also have a different exclusion reason: they implement the adapter/runtime rather than define its public contract. Keep those distinctions explicit instead of removing every host-created value.

## Records that belong outside the provider API

The destination follows the component that creates and enforces the record's meaning. Removing a type from the public provider contract includes its definitions, exports, callback signatures, event payloads, and transitive dependencies; do not retain aliases or optional UI fields as compatibility shortcuts.

| Struct | Destination | Why |
| --- | --- | --- |
| `ApprovalRequestId` | Harness approvals. | Identifies a host-managed approval prompt, not a native callback request. |
| `ApprovalTargetId` | Harness approvals. | Identifies a target within that host prompt. |
| `ApprovalChoiceId` | Harness approval UI. | Identifies an offered host policy decision. |
| `PermissionApprovalRequest` | Harness approvals. | Combines host choices, presentation, policy scope, and deadline. |
| `PermissionApprovalTarget` | Harness approvals. | Contains display information and offered choices. |
| `PermissionApprovalChoice` | Harness approvals. | Contains host rule/grant choices. |
| `ApprovalRulePattern` | Harness permission policy. | Describes a saved host rule. |
| `ResolvedPermissionTarget` | Harness permission policy. | Records decisions, their sources, and persisted effects. |
| `PermissionApprovalResolution` | Harness approvals. | Records the host prompt's committed resolution. |
| `PermissionApprovalCancellation` | Harness approvals. | Records cancellation of a host pending decision. |
| `DirectoryAccessGrant` | Harness permission policy. | Publishes a persisted host grant and its approval origin. |
| `ApprovalCommand` | Harness approval presentation. | Contains display focus ranges and syntax highlights. |
| `CommandHighlight` | Shared shell service, consumed by Harness. | Its highlight group and priority are presentation data; the record must not appear in provider-facing payloads. |
| `InputDeadline` | Harness approvals/questions. | Manages the host's user-decision timeout. |
| `SkillRead` | Harness skill attribution. | Harness derives this annotation from tool observations. |
| `ProviderRetry` | Harness retry scheduler. | Contains host attempt counters and scheduled retry time. |
| `SessionId` | Harness session ownership. | This particular definition identifies a Harness session, not a provider conversation. |
| `ThreadOwner` | Harness ownership registry. | Contains the Harness session/agent ownership pair. |
| `TurnContext` | Internal ownership layer. | Must remain private; separate its Harness owner from adapter-required IDs and settings. |
| `PreparingTurn` | Private provider runtime. | Owns preparation and worker handles. |
| `RunningTurn` | Private provider runtime. | Owns the active worker. |
| `CollectingTurn` | Private provider runtime. | Owns cleanup and its retained result. |
| `PublishingTurn` | Private provider runtime. | Owns completion publication. |
| `UnavailableTurn` | Private provider runtime. | Retains unfinished cleanup and failure state. |
| `RetainedTask` | Private provider runtime. | Implements cancellation-safe worker joining. |

The current source also defines `ApprovalRequestView`, `ApprovalItem`, and `ApprovalChoice` in [provider API approval records](nvim/rust/forge/crates/forge-provider-api/src/approval.rs). The plan's proposed approval records replace those existing presentation types; all three must also leave `forge-provider-api` completely.

Moving presentation does not mean adapters stop translating native authorization requests. Keep semantic permission targets, scoped request correlation, and normalized allow/deny/cancel responses at the boundary. Keep native option IDs private to the owning adapter, and leave saved rules, prompt choices, countdowns, and syntax highlighting in Harness.

## Records that need a responsibility split

These records contain useful provider data and must not be deleted wholesale. Separate the fields according to ownership while preserving their current operational meaning and user-authored settings.

| Struct | Keep in provider contract | Move out or derive elsewhere |
| --- | --- | --- |
| `Capability` | Actual support and typed restrictions. | Menu groups, labels, and host-authored explanatory presentation. |
| `CapabilityManifest` | Provider support/version facts and effective support revisions. | Harness workflow capability composition and menu metadata. |
| `ContextUsage` | Observed tokens used and context capacity. | Calculate `remaining_percent` in Harness. |
| `TerminalSnapshot` | Actual terminal inventory. | Review `starting` and `supported` against operation/capability state rather than treating them as native inventory observations. |
| `DirectoryGrant` | Read/write directory values required to configure provider access. | Saved host-grant state and approval provenance. |

The existing [capability definitions](nvim/rust/forge/crates/forge-provider-api/src/capability.rs) mix menu metadata and host workflow concepts with native support declarations. Review `CapabilityManifest.task_steering`, `TaskKind`, and host-owned `CapabilityId` entries together; a native adapter must not own Plan/Execute/Goal transitions merely because it supports steering.

Inspect related request inputs as well: [`PromptRequest`](nvim/rust/forge/crates/forge-provider-api/src/session.rs) currently carries `TaskStart` and Harness-specific [`PromptMode`](nvim/rust/forge/crates/forge-provider-api/src/input.rs) values. Convert host workflow intent into host-prepared instructions and genuinely necessary provider-neutral execution options rather than blindly moving required behavior or retaining workflow coupling under a new name.

## Data that remains appropriate at the boundary

The audit does not require relocating every value that Harness displays. Native message text, command titles, checklist priorities, measurement labels, reported usage, and sanitized failures are legitimate normalized provider data.

Keep thread/turn/item IDs, operation and binding IDs, opaque recovery tokens, delivery correlation IDs, turn results, message/tool payloads, observed child relationships, reported file changes, and compaction measurements where their public contract requires them. Opaque native history IDs remain valid integration values; they do not justify exposing native parser state or host workflow state.

The concrete examples motivating this split are already present in source. [`SkillRead`](nvim/rust/forge/crates/forge-provider-api/src/skill.rs) is produced by the [Harness skill observer](nvim/rust/forge/crates/forge-harness/src/skill.rs), [`ProviderRetry`](nvim/rust/forge/crates/forge-provider-api/src/retry.rs) carries host scheduling state, and [`InputDeadline`](nvim/rust/forge/crates/forge-provider-api/src/deadline.rs) carries host pending-decision timing.

## Implementation and verification

Trace definitions and all call sites before relocation, then change each type at its definition and update dependent producers, consumers, and serialization together. Reuse existing ownership and shell-analysis services instead of adding a reverse dependency from the provider API to Harness.

| Boundary | Required assertion |
| --- | --- |
| Public API surface. | No host approval-presentation, decision-deadline, skill-attribution, scheduler, or private worker record remains in definitions, re-exports, events, callbacks, or transitive payloads. |
| Provider permissions. | Native adapters still receive semantic targets and translate responses without UI records or saved-rule ownership. |
| Approval rendering and policy. | Highlighting, exact/broad choices, timeouts, persistence errors, and read/write separation retain their behavior. |
| Capability discovery. | Semantic support and restrictions remain accurate while Harness supplies menus and host workflow availability. |
| Skill attribution and retries. | Annotations target their original item; retries retain cancellation, delay, delivery, and replay-safety rules. |
| Context and terminals. | Derived context percentages remain correct; unsupported, opening, successful-empty, and failed inventory states remain distinguishable. |
| Ownership and cleanup. | Moving private records does not abandon worker handles or change exact-turn cancellation and retained cleanup. |
| Settings and state. | Persistent user settings survive; incompatible disposable contracts use a new version rather than session migration. |

Follow [the build and test procedure](nvim/contributing/forge/building-and-testing.md) and the existing permission, provider, and fresh-host fixtures. An independent provider API build alone is insufficient: consumers must preserve the behavior formerly carried by the relocated records.
