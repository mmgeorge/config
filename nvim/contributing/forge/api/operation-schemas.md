# Operation parameter schemas

The discriminator selects exactly one accepted operation. The tables expand the native Serde declarations recorded in the usage audit. Every listed operation has a current producer. Removed writer operations do not appear here.

`String`, opaque identity newtypes, and `PathBuf` are JSON strings. Integer types are JSON integers. `Vec<T>` is an array, `Option<T>` accepts omission or null, and other named types retain their native object or enum schema. A field without `Option` or `serde(default)` is required. `serde(default)` applies the declared Rust default, not the default of the entire containing object.

Document and view identities belong to the owning open document and registered view. Revisions fence stale content, and input sequences order one view's actions. Session identity remains in the request envelope rather than these parameter objects. See [request ownership](lua-client.md#existing-session-requests).

The tables describe input structure. Each method's namespace entry describes its effect, result, and error boundary. Method-specific validation still rejects invalid paths, stale revisions, unknown identities, and unsupported action targets.

## repository.write / WriteRequest / trace

Select `operation = "trace"`. The native declaration is in [WriteRequest](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"trace"` | Required discriminator |
| `after` | `Option<u64>` | Optional, null or omitted |

Producer: [nvim/lua/forge/git/write.lua:25](../../../../nvim/lua/forge/git/write.lua).

## repository.write / WriteRequest / prepare

Select `operation = "prepare"`. The native declaration is in [WriteRequest](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"prepare"` | Required discriminator |
| `workspace` | `PathBuf` | Required |
| `action` | `WriteAction` | Required |

Producer: [nvim/lua/forge/git/write.lua:150](../../../../nvim/lua/forge/git/write.lua).

## repository.write / WriteRequest / submit

Select `operation = "submit"`. The native declaration is in [WriteRequest](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"submit"` | Required discriminator |
| `intent` | `String` | Required |

Producer: [nvim/lua/forge/git/write.lua:165](../../../../nvim/lua/forge/git/write.lua).

## repository.write / WriteRequest / acknowledge

Select `operation = "acknowledge"`. The native declaration is in [WriteRequest](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"acknowledge"` | Required discriminator |
| `operation_id` | `u64` | Required |

Producer: [nvim/lua/forge/git/write.lua:142](../../../../nvim/lua/forge/git/write.lua).

## repository.write / WriteRequest / cancel

Select `operation = "cancel"`. The native declaration is in [WriteRequest](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"cancel"` | Required discriminator |
| `intent` | `Option<String>` | Optional, null or omitted |
| `operation_id` | `Option<u64>` | Optional, null or omitted |

Producer: [nvim/lua/forge/git/write.lua:101](../../../../nvim/lua/forge/git/write.lua).

## repository.status.document / StatusRequest / local

Select `operation = "local"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"local"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `filename` | `PathBuf` | Required |

Producer: [nvim/lua/forge/status.lua:819](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / comparison

Select `operation = "comparison"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"comparison"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `workspace` | `PathBuf` | Required |
| `reference` | `String` | Required |
| `worktree` | `bool` | Required |
| `path` | `Option<String>` | Optional, null or omitted |

Producer: [nvim/lua/forge/status.lua:308](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / body_snapshot

Select `operation = "body_snapshot"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"body_snapshot"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `file` | `u64` | Required |
| `generation` | `u64` | Required |

Producer: [nvim/lua/forge/status.lua:670](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / open

Select `operation = "open"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `workspace` | `PathBuf` | Required |

Producer: [nvim/lua/forge/status.lua:813](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / snapshot

Select `operation = "snapshot"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"snapshot"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/status.lua:141](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / refresh

Select `operation = "refresh"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"refresh"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/status.lua:442](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / demand

Select `operation = "demand"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"demand"` | Required discriminator |
| `input` | `forge_status::StatusInput` | Required |

Producer: [nvim/lua/forge/status.lua:321](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / open_target

Select `operation = "open_target"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open_target"` | Required discriminator |
| `input` | `forge_status::StatusInput` | Required |

Producer: [nvim/lua/forge/status.lua:545](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / input

Select `operation = "input"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"input"` | Required discriminator |
| `input` | `forge_status::StatusInput` | Required |
| `selection` | `Option<forge_status::StatusSelection>` | Optional, null or omitted |

Producer: [nvim/lua/forge/status.lua:560](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / close_view

Select `operation = "close_view"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `forge_buffer::identity::ViewId` | Required |

Producer: [nvim/lua/forge/status.lua:174](../../../../nvim/lua/forge/status.lua).

## repository.status.document / StatusRequest / close

Select `operation = "close"`. The native declaration is in [StatusRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/status.lua:105](../../../../nvim/lua/forge/status.lua).

## repository.source.document / SourceRequest / open

Select `operation = "open"`. The native declaration is in [SourceRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `workspace` | `PathBuf` | Required |
| `revision` | `String` | Required |
| `path` | `String` | Required |

Producer: [nvim/lua/forge/source_document.lua:188](../../../../nvim/lua/forge/source_document.lua).

## repository.source.document / SourceRequest / demand

Select `operation = "demand"`. The native declaration is in [SourceRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"demand"` | Required discriminator |
| `input` | `forge_buffer::input::DocumentInput` | Required |

Producer: [nvim/lua/forge/source_document.lua:137](../../../../nvim/lua/forge/source_document.lua).

## repository.source.document / SourceRequest / snapshot

Select `operation = "snapshot"`. The native declaration is in [SourceRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"snapshot"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/source_document.lua:161](../../../../nvim/lua/forge/source_document.lua).

## repository.source.document / SourceRequest / close_view

Select `operation = "close_view"`. The native declaration is in [SourceRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `forge_buffer::identity::ViewId` | Required |

Producer: [nvim/lua/forge/source_document.lua:50](../../../../nvim/lua/forge/source_document.lua).

## repository.source.document / SourceRequest / close

Select `operation = "close"`. The native declaration is in [SourceRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/source_document.lua:23](../../../../nvim/lua/forge/source_document.lua).

## repository.status.context / StatusContextRequest / action

Select `operation = "action"`. The native declaration is in [StatusContextRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"action"` | Required discriminator |
| `input` | `forge_status::StatusInput` | Required |

Producer: [nvim/lua/forge/views/status/status_context.lua:381](../../../../nvim/lua/forge/views/status/status_context.lua).

## repository.status.context / StatusContextRequest / issues

Select `operation = "issues"`. The native declaration is in [StatusContextRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"issues"` | Required discriminator |
| `input` | `forge_status::StatusInput` | Required |
| `text` | `String` | Required |

Producer: [nvim/lua/forge/views/status/status_context.lua:399](../../../../nvim/lua/forge/views/status/status_context.lua).

## repository.status.context / StatusContextRequest / commit_message

Select `operation = "commit_message"`. The native declaration is in [StatusContextRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"commit_message"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `workspace` | `PathBuf` | Required |
| `oid` | `String` | Required |

Producer: [nvim/lua/forge/source_document.lua:186](../../../../nvim/lua/forge/source_document.lua).

## repository.status.context / StatusContextRequest / about_message

Select `operation = "about_message"`. The native declaration is in [StatusContextRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"about_message"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `text` | `String` | Required |

Producer: [nvim/lua/forge/source_document.lua:187](../../../../nvim/lua/forge/source_document.lua).

## session.document / SessionSourceRequest / open_diff

Select `operation = "open_diff"`. The native declaration is in [SessionSourceRequest](../../../rust/forge/src/router.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open_diff"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `input` | `forge_buffer::input::DocumentInput` | Required |

Producer: [nvim/lua/forge/source_document.lua:185](../../../../nvim/lua/forge/source_document.lua).

## github.issue.document / IssueRequest / open

Select `operation = "open"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `directory` | `PathBuf` | Required |
| `database` | `PathBuf` | Required |
| `repository` | `GithubRepositoryId` | Required |
| `number` | `u64` | Required |

Producer: [nvim/lua/github/issue_document.lua:510](../../../../nvim/lua/github/issue_document.lua).

## github.issue.document / IssueRequest / snapshot

Select `operation = "snapshot"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"snapshot"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/github/issue_document.lua:260](../../../../nvim/lua/github/issue_document.lua).

## github.issue.document / IssueRequest / save

Select `operation = "save"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"save"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `capture` | `Vec<RegionEdit>` | Optional, declared default |

Producer: [nvim/lua/github/issue_document.lua:247](../../../../nvim/lua/github/issue_document.lua).

## github.issue.document / IssueRequest / refresh

Select `operation = "refresh"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"refresh"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/github/issue_document.lua:284](../../../../nvim/lua/github/issue_document.lua).

## github.issue.document / IssueRequest / view

Select `operation = "view"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `width` | `WidthProfile` | Required |

Producer: [nvim/lua/github/issue_document.lua:131](../../../../nvim/lua/github/issue_document.lua).

## github.issue.document / IssueRequest / close_view

Select `operation = "close_view"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |

Producer: [nvim/lua/github/issue_document.lua:169](../../../../nvim/lua/github/issue_document.lua).

## github.issue.document / IssueRequest / act

Select `operation = "act"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"act"` | Required discriminator |
| `input` | `DocumentInput` | Required |

Producer: [nvim/lua/github/issue_document.lua:407](../../../../nvim/lua/github/issue_document.lua).

## github.issue.document / IssueRequest / resolve

Select `operation = "resolve"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"resolve"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `operation_id` | `String` | Required |
| `resolution` | `forge_github::review_mutation::RecoveryResolution` | Required |

Producer: [nvim/lua/github/issue_document.lua:312](../../../../nvim/lua/github/issue_document.lua).

## github.issue.document / IssueRequest / close

Select `operation = "close"`. The native declaration is in [IssueRequest](../../../rust/forge/src/issue_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/github/issue_document.lua:190](../../../../nvim/lua/github/issue_document.lua).

## github.notification.document / NotificationRequest / open

Select `operation = "open"`. The native declaration is in [NotificationRequest](../../../rust/forge/src/notification_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `workspace` | `PathBuf` | Required |
| `hostname` | `String` | Required |

Producer: [nvim/lua/forge/notifications.lua:199](../../../../nvim/lua/forge/notifications.lua).

## github.notification.document / NotificationRequest / snapshot

Select `operation = "snapshot"`. The native declaration is in [NotificationRequest](../../../rust/forge/src/notification_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"snapshot"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/notifications.lua:178](../../../../nvim/lua/forge/notifications.lua).

## github.notification.document / NotificationRequest / view

Select `operation = "view"`. The native declaration is in [NotificationRequest](../../../rust/forge/src/notification_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `width` | `WidthProfile` | Required |

Producer: [nvim/lua/forge/notifications.lua:83](../../../../nvim/lua/forge/notifications.lua).

## github.notification.document / NotificationRequest / close_view

Select `operation = "close_view"`. The native declaration is in [NotificationRequest](../../../rust/forge/src/notification_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |

Producer: [nvim/lua/forge/notifications.lua:95](../../../../nvim/lua/forge/notifications.lua).

## github.notification.document / NotificationRequest / act

Select `operation = "act"`. The native declaration is in [NotificationRequest](../../../rust/forge/src/notification_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"act"` | Required discriminator |
| `input` | `DocumentInput` | Required |

Producer: [nvim/lua/forge/notifications.lua:109](../../../../nvim/lua/forge/notifications.lua).

## github.notification.document / NotificationRequest / close

Select `operation = "close"`. The native declaration is in [NotificationRequest](../../../rust/forge/src/notification_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/notifications.lua:9](../../../../nvim/lua/forge/notifications.lua).

## repository.walkthrough.document / WalkthroughRequest / open

Select `operation = "open"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `workspace` | `PathBuf` | Required |
| `inventory` | `Option<bool>` | Optional, null or omitted |

Producer: [nvim/lua/forge/walkthrough.lua:247](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / snapshot

Select `operation = "snapshot"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"snapshot"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:224](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / view

Select `operation = "view"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `width` | `WidthProfile` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:49](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / close_view

Select `operation = "close_view"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:38](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / change

Select `operation = "change"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"change"` | Required discriminator |
| `input` | `DocumentInput` | Required |
| `document` | `DocumentId` | Required |
| `annotation_document` | `DocumentId` | Required |
| `review` | `Option<bool>` | Optional, null or omitted |

Producer: [nvim/lua/forge/source_document.lua:184](../../../../nvim/lua/forge/source_document.lua).

## repository.walkthrough.document / WalkthroughRequest / annotation_source

Select `operation = "annotation_source"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"annotation_source"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `input` | `DocumentInput` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:166](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / annotation_snapshot

Select `operation = "annotation_snapshot"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"annotation_snapshot"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `annotation` | `DocumentId` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:197](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / annotation_view

Select `operation = "annotation_view"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"annotation_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `annotation` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `width` | `WidthProfile` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:144](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / annotation_close_view

Select `operation = "annotation_close_view"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"annotation_close_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `annotation` | `DocumentId` | Required |
| `view` | `ViewId` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:130](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / annotation_close

Select `operation = "annotation_close"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"annotation_close"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `annotation` | `DocumentId` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:112](../../../../nvim/lua/forge/walkthrough.lua).

## repository.walkthrough.document / WalkthroughRequest / close

Select `operation = "close"`. The native declaration is in [WalkthroughRequest](../../../rust/forge/src/walkthrough_routes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/walkthrough.lua:11](../../../../nvim/lua/forge/walkthrough.lua).

## session.document / PresentationRequest / status_action

Select `operation = "status_action"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"status_action"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `status` | `DocumentId` | Required |
| `revision` | `DocumentRevision` | Required |
| `view` | `ViewId` | Required |
| `sequence` | `InputSequence` | Required |
| `target` | `TargetId` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:838](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / node

Select `operation = "node"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"node"` | Required discriminator |
| `request` | `super::nodes::NodeRequest` | Flattened fields from this type |

Producer: [nvim/lua/forge/views/harness/presentation.lua:625](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / background_terminals

Select `operation = "background_terminals"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"background_terminals"` | Required discriminator |

Producer: [nvim/lua/forge/views/harness/controller.lua:1462](../../../../nvim/lua/forge/views/harness/controller.lua).

## session.document / PresentationRequest / recap

Select `operation = "recap"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"recap"` | Required discriminator |
| `model` | `String` | Required |

Producer: [nvim/lua/forge/views/harness/recap.lua:28](../../../../nvim/lua/forge/views/harness/recap.lua).

## session.document / PresentationRequest / session_name

Select `operation = "session_name"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"session_name"` | Required discriminator |
| `model` | `String` | Required |

Producer: [nvim/lua/forge/views/harness/session_name.lua:50](../../../../nvim/lua/forge/views/harness/session_name.lua).

## session.document / PresentationRequest / terminate_terminal

Select `operation = "terminate_terminal"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"terminate_terminal"` | Required discriminator |
| `id` | `String` | Required |

Producer: [nvim/lua/forge/views/harness/controller.lua:1475](../../../../nvim/lua/forge/views/harness/controller.lua).

## session.document / PresentationRequest / highlight

Select `operation = "highlight"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"highlight"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:755](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / plan_open

Select `operation = "plan_open"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"plan_open"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `plan_id` | `String` | Required |
| `digest` | `String` | Required |
| `revision` | `Option<u32>` | Optional, null or omitted |
| `width` | `WidthProfile` | Required |
| `saved_source_digest` | `Option<String>` | Optional, null or omitted |
| `focused_annotation` | `Option<String>` | Optional, null or omitted |

Producer: [nvim/lua/forge/views/plan_review/document.lua:490](../../../../nvim/lua/forge/views/plan_review/document.lua).

## session.document / PresentationRequest / plan_action

Select `operation = "plan_action"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"plan_action"` | Required discriminator |
| `input` | `DocumentInput` | Required |

Producer: [nvim/lua/forge/views/plan_review/document.lua:604](../../../../nvim/lua/forge/views/plan_review/document.lua).

## session.document / PresentationRequest / plan_definition

Select `operation = "plan_definition"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"plan_definition"` | Required discriminator |
| `input` | `DocumentInput` | Required |
| `source` | `crate::plan::WorkspaceDefinition` | Required |
| `target` | `crate::plan::DefinitionLocation` | Required |

Producer: [nvim/lua/forge/views/plan_review/document.lua:569](../../../../nvim/lua/forge/views/plan_review/document.lua).

## session.document / PresentationRequest / plan_save_annotations

Select `operation = "plan_save_annotations"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"plan_save_annotations"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `saved_source_digest` | `String` | Required |
| `annotation` | `Vec<crate::plan::ReviewAnnotation>` | Required |

Producer: [nvim/lua/forge/views/plan_review/document.lua:210](../../../../nvim/lua/forge/views/plan_review/document.lua).

## session.document / PresentationRequest / plan_view

Select `operation = "plan_view"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"plan_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `width` | `Option<WidthProfile>` | Optional, null or omitted |

Producer: [nvim/lua/forge/views/plan_review/document.lua:475](../../../../nvim/lua/forge/views/plan_review/document.lua).

## session.document / PresentationRequest / plan_close

Select `operation = "plan_close"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"plan_close"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/views/plan_review/document.lua:112](../../../../nvim/lua/forge/views/plan_review/document.lua).

## session.document / PresentationRequest / open

Select `operation = "open"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `width` | `WidthProfile` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:338](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / sync

Select `operation = "sync"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"sync"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `revision` | `DocumentRevision` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:540](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / staging

Select `operation = "staging"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"staging"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `sequence` | `u64` | Required |
| `queue` | `Vec<StagedInput>` | Optional, declared default |
| `steering` | `Vec<StagedInput>` | Optional, declared default |

Producer: [nvim/lua/forge/views/harness/presentation.lua:289](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / snapshot

Select `operation = "snapshot"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"snapshot"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:227](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / input

Select `operation = "input"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"input"` | Required discriminator |
| `input` | `DocumentInput` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:771](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / navigate_prompt

Select `operation = "navigate_prompt"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"navigate_prompt"` | Required discriminator |
| `input` | `DocumentInput` | Required |
| `previous` | `bool` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:874](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / select_agent

Select `operation = "select_agent"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"select_agent"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `sequence` | `u64` | Required |
| `run_id` | `Option<String>` | Optional, null or omitted |

Producer: [nvim/lua/forge/views/harness/presentation.lua:902](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / close

Select `operation = "close"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:181](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / resize

Select `operation = "resize"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"resize"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `width` | `WidthProfile` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:813](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / close_view

Select `operation = "close_view"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"close_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:791](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / open_view

Select `operation = "open_view"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"open_view"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `view` | `ViewId` | Required |
| `width` | `WidthProfile` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:201](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / PresentationRequest / tool_open

Select `operation = "tool_open"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"tool_open"` | Required discriminator |
| `input` | `DocumentInput` | Required |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/views/harness/tool_output.lua:126](../../../../nvim/lua/forge/views/harness/tool_output.lua).

## session.document / PresentationRequest / tool_demand

Select `operation = "tool_demand"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"tool_demand"` | Required discriminator |
| `document` | `DocumentId` | Required |
| `revision` | `DocumentRevision` | Required |

Producer: [nvim/lua/forge/views/harness/tool_output.lua:75](../../../../nvim/lua/forge/views/harness/tool_output.lua).

## session.document / PresentationRequest / tool_export

Select `operation = "tool_export"`. The native declaration is in [PresentationRequest](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `operation` | `"tool_export"` | Required discriminator |
| `document` | `DocumentId` | Required |

Producer: [nvim/lua/forge/views/harness/tool_output.lua:109](../../../../nvim/lua/forge/views/harness/tool_output.lua).

## repository.write / WriteAction / create_branch

Select `kind = "create_branch"`. These fields belong to `action` inside the writer prepare request. The native declaration is in [WriteAction](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `kind` | `"create_branch"` | Required discriminator |
| `name` | `String` | Required |

Producer: [nvim/lua/forge/views/status/status_context.lua:157](../../../../nvim/lua/forge/views/status/status_context.lua).

## repository.write / WriteAction / commit_editor

Select `kind = "commit_editor"`. These fields belong to `action` inside the writer prepare request. The native declaration is in [WriteAction](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `kind` | `"commit_editor"` | Required discriminator |
| `command` | `String` | Required |
| `nvim_server` | `String` | Required |
| `amend` | `bool` | Optional, declared default |

Producer: [nvim/lua/forge/integrations/commit.lua:400](../../../../nvim/lua/forge/integrations/commit.lua).

## repository.write / WriteAction / push

Select `kind = "push"`. These fields belong to `action` inside the writer prepare request. The native declaration is in [WriteAction](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `kind` | `"push"` | Required discriminator |

Producer: [nvim/lua/forge/views/status/status_context.lua:145](../../../../nvim/lua/forge/views/status/status_context.lua).

## repository.write / WriteAction / publish_branch

Select `kind = "publish_branch"`. These fields belong to `action` inside the writer prepare request. The native declaration is in [WriteAction](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `kind` | `"publish_branch"` | Required discriminator |
| `name` | `String` | Required |
| `head` | `Option<String>` | Optional, null or omitted |

Producer: [nvim/lua/github/open_pr.lua:596](../../../../nvim/lua/github/open_pr.lua).

## repository.write / WriteAction / pull

Select `kind = "pull"`. These fields belong to `action` inside the writer prepare request. The native declaration is in [WriteAction](../../../rust/forge/src/repository_write.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `kind` | `"pull"` | Required discriminator |

Producer: [nvim/lua/forge/views/status/status_context.lua:146](../../../../nvim/lua/forge/views/status/status_context.lua).

## session.document / NodeAction / set_expansion

Select `action = "set_expansion"`. These fields extend the `node` operation and its shared `NodeRequest` fields. The native declaration is in [NodeAction](../../../rust/forge/crates/forge-harness/src/buffer/nodes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `action` | `"set_expansion"` | Required discriminator |
| `expanded` | `bool` | Required |

Producer: [nvim/lua/forge/views/harness/presentation.lua:627](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / NodeAction / load_more

Select `action = "load_more"`. These fields extend the `node` operation and its shared `NodeRequest` fields. The native declaration is in [NodeAction](../../../rust/forge/crates/forge-harness/src/buffer/nodes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `action` | `"load_more"` | Required discriminator |

Producer: [nvim/lua/forge/views/harness/presentation.lua:627](../../../../nvim/lua/forge/views/harness/presentation.lua).

## session.document / NodeAction / retry_loading

Select `action = "retry_loading"`. These fields extend the `node` operation and its shared `NodeRequest` fields. The native declaration is in [NodeAction](../../../rust/forge/crates/forge-harness/src/buffer/nodes.rs).

| Field | Native type / JSON value | Presence |
| --- | --- | --- |
| `action` | `"retry_loading"` | Required discriminator |

Producer: [nvim/lua/forge/views/harness/presentation.lua:627](../../../../nvim/lua/forge/views/harness/presentation.lua).

