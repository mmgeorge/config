# Forge API reference

One Lua client owns one native connection. Each operation carries its own session, repository, or document identity. The host correlates replies by request ID and executes admitted requests independently.

The [Lua client](lua-client.md) defines initialization, explicit context, callbacks, progress, and recovery. The shared [method policy](../../../lua/forge/protocol_contract.json) determines session context, readiness, admission, and mutation recovery. Provider-native protocols are separate from this interface.

The [native schemas](native-schemas.md) preserve accepted field types, defaults, and Serde encodings. The [session runtime schemas](runtime-schemas.md) define snapshots, input operations, models, and saved session state. The [operation schemas](operation-schemas.md) enumerate the typed multiplexed operations and their fields. [Session command configuration](session-command-configuration.md) specifies the configuration actions sharing the command carrier.

The [repository result schemas](repository-schemas.md) define status inventory, captured navigation, source delivery, and Git write actions. Wire examples are captured before Lua null normalization. Fixture paths are replaced with `/fixture` for readability.

The [GitHub request and result schemas](github-schemas.md) define remote reads, captured writes, durable outcomes, and explicit recovery resolutions. The [native review schemas](review-schemas.md) define review, issue, and notification document results, including custom serialization rules.

## Reference coverage

The index covers every retained method, including explicitly identified live integration-control APIs. The [usage ledger](../../../../md/rename-usage-ledger.json) records its production or test-control caller. The [completed implementation plan](../../../../rename.md), [API audit](../../../../md/rename-api-audit.json), and [verification record](../../../../md/rename-verification.md) record reviewed contracts and verified examples for all 147 methods.

| Method | Reference | Session context |
| --- | --- | --- |
| `agent.profile.get` | [shared](shared.md#agentprofileget) | Optional explicit session ID |
| `agent.profile.save` | [shared](shared.md#agentprofilesave) | Optional explicit session ID |
| `background.session.document` | [background](background.md#backgroundsessiondocument) | Session ID forbidden |
| `background.settings` | [background](background.md#backgroundsettings) | Session ID forbidden |
| `github.actor.get` | [github](github.md#githubactorget) | Session ID forbidden |
| `github.comment` | [github](github.md#githubcomment) | Session ID forbidden |
| `github.issue.document` | [github](github.md#githubissuedocument) | Session ID forbidden |
| `github.issue.get` | [github](github.md#githubissueget) | Session ID forbidden |
| `github.issue.list` | [github](github.md#githubissuelist) | Session ID forbidden |
| `github.issue.sync` | [github](github.md#githubissuesync) | Session ID forbidden |
| `github.notification.document` | [github](github.md#githubnotificationdocument) | Session ID forbidden |
| `github.notification.list` | [github](github.md#githubnotificationlist) | Session ID forbidden |
| `github.pull_request` | [github](github.md#githubpull_request) | Session ID forbidden |
| `github.pull_request.creation.context` | [github](github.md#githubpull_requestcreationcontext) | Session ID forbidden |
| `github.recovery.acknowledge` | [github](github.md#githubrecoveryacknowledge) | Session ID forbidden |
| `github.recovery.draft.settle` | [github](github.md#githubrecoverydraftsettle) | Session ID forbidden |
| `github.recovery.inspect` | [github](github.md#githubrecoveryinspect) | Session ID forbidden |
| `github.recovery.resolve` | [github](github.md#githubrecoveryresolve) | Session ID forbidden |
| `github.repository.metadata` | [github](github.md#githubrepositorymetadata) | Session ID forbidden |
| `github.review.document.batch.begin` | [github](github.md#githubreviewdocumentbatchbegin) | Session ID forbidden |
| `github.review.document.batch.recover` | [github](github.md#githubreviewdocumentbatchrecover) | Session ID forbidden |
| `github.review.document.batch.submit` | [github](github.md#githubreviewdocumentbatchsubmit) | Session ID forbidden |
| `github.review.document.close` | [github](github.md#githubreviewdocumentclose) | Session ID forbidden |
| `github.review.document.comment` | [github](github.md#githubreviewdocumentcomment) | Session ID forbidden |
| `github.review.document.file.get` | [github](github.md#githubreviewdocumentfileget) | Session ID forbidden |
| `github.review.document.file.page.get` | [github](github.md#githubreviewdocumentfilepageget) | Session ID forbidden |
| `github.review.document.file.viewed.set` | [github](github.md#githubreviewdocumentfileviewedset) | Session ID forbidden |
| `github.review.document.header.load` | [github](github.md#githubreviewdocumentheaderload) | Session ID forbidden |
| `github.review.document.input.apply` | [github](github.md#githubreviewdocumentinputapply) | Session ID forbidden |
| `github.review.document.lifecycle.reconcile` | [github](github.md#githubreviewdocumentlifecyclereconcile) | Session ID forbidden |
| `github.review.document.lifecycle.resolve` | [github](github.md#githubreviewdocumentlifecycleresolve) | Session ID forbidden |
| `github.review.document.load` | [github](github.md#githubreviewdocumentload) | Session ID forbidden |
| `github.review.document.open` | [github](github.md#githubreviewdocumentopen) | Session ID forbidden |
| `github.review.document.reconcile` | [github](github.md#githubreviewdocumentreconcile) | Session ID forbidden |
| `github.review.document.render` | [github](github.md#githubreviewdocumentrender) | Session ID forbidden |
| `github.review.document.save` | [github](github.md#githubreviewdocumentsave) | Session ID forbidden |
| `github.review.document.section.get` | [github](github.md#githubreviewdocumentsectionget) | Session ID forbidden |
| `github.review.document.thread.get` | [github](github.md#githubreviewdocumentthreadget) | Session ID forbidden |
| `github.review.document.transition` | [github](github.md#githubreviewdocumenttransition) | Session ID forbidden |
| `github.review.document.view.set` | [github](github.md#githubreviewdocumentviewset) | Session ID forbidden |
| `github.review.draft.get` | [github](github.md#githubreviewdraftget) | Session ID forbidden |
| `github.review.draft.save` | [github](github.md#githubreviewdraftsave) | Session ID forbidden |
| `github.review.mutate` | [github](github.md#githubreviewmutate) | Session ID forbidden |
| `github.review.submission.context` | [github](github.md#githubreviewsubmissioncontext) | Session ID forbidden |
| `host.initialize` | [host](host.md#hostinitialize) | Session ID forbidden |
| `host.shutdown` | [host](host.md#hostshutdown) | Session ID forbidden |
| `host.trace.configure` | [host](host.md#hosttraceconfigure) | Optional explicit session ID |
| `host.trace.status` | [host](host.md#hosttracestatus) | Optional explicit session ID |
| `host.transport.consumed` | [host](host.md#hosttransportconsumed) | Session ID forbidden |
| `permissions.document.open` | [shared](shared.md#permissionsdocumentopen) | Optional explicit session ID |
| `permissions.document.save` | [shared](shared.md#permissionsdocumentsave) | Optional explicit session ID |
| `provider.model.pin.list` | [shared](shared.md#providermodelpinlist) | Optional explicit session ID |
| `provider.model.pin.set` | [shared](shared.md#providermodelpinset) | Optional explicit session ID |
| `repository.generation` | [repository](repository.md#repositorygeneration) | Session ID forbidden |
| `repository.revision.list` | [repository](repository.md#repositoryrevisionlist) | Session ID forbidden |
| `repository.source.document` | [repository](repository.md#repositorysourcedocument) | Session ID forbidden |
| `repository.status.context` | [repository](repository.md#repositorystatuscontext) | Session ID forbidden |
| `repository.status.document` | [repository](repository.md#repositorystatusdocument) | Session ID forbidden |
| `repository.status.navigate` | [repository](repository.md#repositorystatusnavigate) | Session ID forbidden |
| `repository.walkthrough.document` | [repository](repository.md#repositorywalkthroughdocument) | Session ID forbidden |
| `repository.write` | [repository](repository.md#repositorywrite) | Session ID forbidden |
| `session.agent.followup` | [session](session.md#sessionagentfollowup) | Required explicit session ID |
| `session.agent.interrupt` | [session](session.md#sessionagentinterrupt) | Required explicit session ID |
| `session.agent.interrupt_all` | [session](session.md#sessionagentinterrupt_all) | Required explicit session ID |
| `session.agent.message.send` | [session](session.md#sessionagentmessagesend) | Required explicit session ID |
| `session.agent.model.list` | [session](session.md#sessionagentmodellist) | Required explicit session ID |
| `session.agent.start` | [session](session.md#sessionagentstart) | Required explicit session ID |
| `session.approval.continue` | [session](session.md#sessionapprovalcontinue) | Required explicit session ID |
| `session.approval.resolve` | [session](session.md#sessionapprovalresolve) | Required explicit session ID |
| `session.attachment.import` | [session](session.md#sessionattachmentimport) | Required explicit session ID |
| `session.check.cancel` | [session](session.md#sessioncheckcancel) | Required explicit session ID |
| `session.check.complete` | [session](session.md#sessioncheckcomplete) | Required explicit session ID |
| `session.check.editing.set` | [session](session.md#sessioncheckeditingset) | Required explicit session ID |
| `session.check.list` | [session](session.md#sessionchecklist) | Required explicit session ID |
| `session.check.override` | [session](session.md#sessioncheckoverride) | Required explicit session ID |
| `session.check.run` | [session](session.md#sessioncheckrun) | Required explicit session ID |
| `session.check.status` | [session](session.md#sessioncheckstatus) | Required explicit session ID |
| `session.command.apply` | [session](session.md#sessioncommandapply) | Required explicit session ID |
| `session.command.operation.get` | [session](session.md#sessioncommandoperationget) | Required explicit session ID |
| `session.command.status.get` | [session](session.md#sessioncommandstatusget) | Required explicit session ID |
| `session.compact` | [session](session.md#sessioncompact) | Required explicit session ID |
| `session.configure` | [session](session.md#sessionconfigure) | Required explicit session ID |
| `session.delete` | [session](session.md#sessiondelete) | Required explicit session ID |
| `session.document` | [session](session.md#sessiondocument) | Required explicit session ID |
| `session.exchange.comment.save` | [session](session.md#sessionexchangecommentsave) | Required explicit session ID |
| `session.exchange.list` | [session](session.md#sessionexchangelist) | Required explicit session ID |
| `session.exchange.recovery` | [session](session.md#sessionexchangerecovery) | Required explicit session ID |
| `session.exchange.rollback.apply` | [session](session.md#sessionexchangerollbackapply) | Required explicit session ID |
| `session.exchange.rollback.prepare` | [session](session.md#sessionexchangerollbackprepare) | Required explicit session ID |
| `session.exchange.rollback.preview` | [session](session.md#sessionexchangerollbackpreview) | Required explicit session ID |
| `session.fork` | [session](session.md#sessionfork) | Required explicit session ID |
| `session.goal.clear` | [session](session.md#sessiongoalclear) | Required explicit session ID |
| `session.goal.pause` | [session](session.md#sessiongoalpause) | Required explicit session ID |
| `session.goal.resume` | [session](session.md#sessiongoalresume) | Required explicit session ID |
| `session.goal.set` | [session](session.md#sessiongoalset) | Required explicit session ID |
| `session.health.get` | [session](session.md#sessionhealthget) | Required explicit session ID |
| `session.initialize` | [session](session.md#sessioninitialize) | Session ID forbidden |
| `session.input.apply` | [session](session.md#sessioninputapply) | Required explicit session ID |
| `session.input.get` | [session](session.md#sessioninputget) | Required explicit session ID |
| `session.input.operation.get` | [session](session.md#sessioninputoperationget) | Required explicit session ID |
| `session.input.timeout.configure` | [session](session.md#sessioninputtimeoutconfigure) | Required explicit session ID |
| `session.list` | [session](session.md#sessionlist) | Session ID forbidden |
| `session.lsp.analyze` | [session](session.md#sessionlspanalyze) | Required explicit session ID |
| `session.lsp.authorize` | [session](session.md#sessionlspauthorize) | Required explicit session ID |
| `session.lsp.respond` | [session](session.md#sessionlsprespond) | Required explicit session ID |
| `session.new` | [session](session.md#sessionnew) | Optional explicit session ID |
| `session.permission.set` | [session](session.md#sessionpermissionset) | Required explicit session ID |
| `session.plan.accept` | [session](session.md#sessionplanaccept) | Required explicit session ID |
| `session.plan.acceptance.begin` | [session](session.md#sessionplanacceptancebegin) | Required explicit session ID |
| `session.plan.acceptance.cancel` | [session](session.md#sessionplanacceptancecancel) | Required explicit session ID |
| `session.plan.cancel` | [session](session.md#sessionplancancel) | Required explicit session ID |
| `session.plan.changes.request` | [session](session.md#sessionplanchangesrequest) | Required explicit session ID |
| `session.plan.check.update` | [session](session.md#sessionplancheckupdate) | Required explicit session ID |
| `session.plan.declaration.get` | [session](session.md#sessionplandeclarationget) | Required explicit session ID |
| `session.plan.entity.rename` | [session](session.md#sessionplanentityrename) | Required explicit session ID |
| `session.plan.get` | [session](session.md#sessionplanget) | Required explicit session ID |
| `session.plan.list` | [session](session.md#sessionplanlist) | Required explicit session ID |
| `session.plan.question.answer` | [session](session.md#sessionplanquestionanswer) | Required explicit session ID |
| `session.plan.test.delete` | [session](session.md#sessionplantestdelete) | Required explicit session ID |
| `session.preview` | [session](session.md#sessionpreview) | Required explicit session ID |
| `session.prompt.history.record` | [session](session.md#sessionprompthistoryrecord) | Required explicit session ID |
| `session.prompt.submit` | [session](session.md#sessionpromptsubmit) | Required explicit session ID |
| `session.provider.authenticate` | [session](session.md#sessionproviderauthenticate) | Required explicit session ID |
| `session.provider.capabilities.get` | [session](session.md#sessionprovidercapabilitiesget) | Required explicit session ID |
| `session.provider.mcp.list` | [session](session.md#sessionprovidermcplist) | Required explicit session ID |
| `session.provider.mock_response` | [session](session.md#sessionprovidermock_response) | Required explicit session ID |
| `session.provider.model.list` | [session](session.md#sessionprovidermodellist) | Required explicit session ID |
| `session.provider.setup` | [session](session.md#sessionprovidersetup) | Required explicit session ID |
| `session.provider.skill.list` | [session](session.md#sessionproviderskilllist) | Required explicit session ID |
| `session.question.answer` | [session](session.md#sessionquestionanswer) | Required explicit session ID |
| `session.question.ask` | [session](session.md#sessionquestionask) | Required explicit session ID |
| `session.question.continue` | [session](session.md#sessionquestioncontinue) | Required explicit session ID |
| `session.question.skip` | [session](session.md#sessionquestionskip) | Required explicit session ID |
| `session.rename` | [session](session.md#sessionrename) | Required explicit session ID |
| `session.resume` | [session](session.md#sessionresume) | Required explicit session ID |
| `session.state.get` | [session](session.md#sessionstateget) | Required explicit session ID |
| `session.task.get` | [session](session.md#sessiontaskget) | Required explicit session ID |
| `session.trace.clear` | [session](session.md#sessiontraceclear) | Required explicit session ID |
| `session.turn.cancel` | [session](session.md#sessionturncancel) | Required explicit session ID |
| `session.turn.restart` | [session](session.md#sessionturnrestart) | Required explicit session ID |
| `session.turn.steer` | [session](session.md#sessionturnsteer) | Required explicit session ID |
| `settings.get` | [shared](shared.md#settingsget) | Optional explicit session ID |
| `settings.import.apply` | [shared](shared.md#settingsimportapply) | Optional explicit session ID |
| `settings.import.preview` | [shared](shared.md#settingsimportpreview) | Optional explicit session ID |
| `settings.mcp.set_enabled` | [shared](shared.md#settingsmcpset_enabled) | Optional explicit session ID |
| `settings.skill.set_enabled` | [shared](shared.md#settingsskillset_enabled) | Optional explicit session ID |
| `settings.update` | [shared](shared.md#settingsupdate) | Optional explicit session ID |
