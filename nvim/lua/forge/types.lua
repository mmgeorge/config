--- Shared LuaCATS type definitions for the forge plugin.
---
--- Annotation-only: lua-language-server resolves these across the workspace, so nothing
--- requires this file at runtime. Function-local option/result types stay beside their code.

---@alias ForgeGitCommand string[]

---@class ForgeGitCommandResult
---@field ok boolean
---@field code integer
---@field output string
---@field stdout? string
---@field stderr? string
---@field root? string
---@field args ForgeGitCommand

---@class ForgeGitAsyncResult
---@field code integer
---@field stdout string
---@field stderr string
---@field output string

---@alias ForgeGitTextCallback fun(result: ForgeGitAsyncResult)
---@alias ForgeGitListCallback fun(output: string[], code: integer, stderr?: string)

---@class ForgeGitFailure
---@field file? string
---@field path? string
---@field message? string
---@field output? string
---@field stderr? string
---@field stdout? string
---@field code? integer

---@class ForgeGitBackend
---@field systemlist? fun(command: ForgeGitCommand): string[]|string, integer?
---@field system? fun(command: ForgeGitCommand, input?: string): string, integer?
---@field systemlist_async? fun(command: ForgeGitCommand, cb: ForgeGitListCallback)
---@field system_async? fun(command: ForgeGitCommand, input: string?, cb: ForgeGitTextCallback, on_line: fun(line: string)?)
---@field system_stream_async? fun(command: ForgeGitCommand, input: string?, on_line: fun(line: string), cb: ForgeGitTextCallback)
---@field delete? fun(path: string): integer

---@class ForgeHunk
---@field file string
---@field filename? string
---@field section_name? string
---@field pos integer
---@field context? string
---@field context_text? string
---@field diff? string
---@field staged boolean
---@field added integer
---@field removed integer
---@field git_status? string
---@field git_original_file? string
---@field git_path_change_kind? "renamed"|"copied"
---@field raw_hunks? ForgeHunk[]

---@class ForgeStatusFile
---@field filename string
---@field relpath string
---@field original_relpath? string
---@field path_change_kind? "renamed"|"copied"
---@field section_name string
---@field added integer
---@field removed integer
---@field hunks ForgeHunk[]
---@field untracked boolean
---@field status string
---@field git_status? string
---@field diff_source_id? string
---@field diff_file_key? string
---@field preview_state? ForgePreviewState
---@field preview_source? ForgePreviewSource
---@field preview_oid? string
---@field preview_mode? string
---@field preview_binary? boolean
---@field preview_error? string
---@field line_stats_complete? boolean

---@class ForgeStatusCommit
---@field oid string
---@field short_oid string
---@field branch? string
---@field subject string
---@field committed_at? string
---@field authored_at? string
---@field upstream? string
---@field files? ForgeStatusFile[]
---@field files_loaded? boolean
---@field files_loading? boolean
---@field files_error? string

---@class ForgeStatusSection
---@field name string
---@field title string
---@field default_folded boolean
---@field files ForgeStatusFile[]
---@field files_by_name table<string, ForgeStatusFile>
---@field commits? ForgeStatusCommit[]
---@field reviews? ForgeGhSubmittedReview[]
---@field issue_comments? table[]
---@field upstream? string
---@field file_key_prefix? string
---@field file_entry_kind? "file"|"commit_file"|"pr_file"|"pr_review_file"
---@field hunk_entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"

---@class ForgeStatusEntry
---@field id? string
---@field default_folded? boolean
---@field kind "section"|"file"|"hunk"|"context_line"|"commit"|"commit_message"|"commit_file"|"commit_hunk"|"pr_file"|"pr_hunk"|"pr_comment"|"pr_comment_reply"|"pr_review"|"pr_review_file"|"pr_review_hunk"|"review_comment"|"comment_box"|"pr"|"about"|"pr_check"|"pr_head_section"|"pr_head_line"
---@field section? ForgeStatusSection
---@field file? ForgeStatusFile
---@field preview_omitted? boolean
---@field hunk? ForgeHunk
---@field commit? ForgeStatusCommit
---@field pr_review? ForgeGhSubmittedReview
---@field pr_comment? ForgeGhPendingReviewComment|ForgeGhIssueComment
---@field pr_comment_body? boolean
---@field pr_comment_body_index? integer
---@field pr_comment_reply? ForgeGhReviewCommentReply
---@field review_comment? table
---@field review_reply? table
---@field review_readonly_body? boolean
---@field review_reply_draft? ForgePrReplyDraft
---@field review_reply_draft_body? boolean
---@field review_reply_draft_body_index? integer
---@field comment_box? ForgeCommentDescriptor
---@field comment_box_source? table
---@field comment_box_index? integer
---@field comment_box_anchor_line? integer
---@field comment_box_anchor_entry_id? string
---@field comment_box_boundary? "header"|"body"|"footer"
---@field diff_line? table
---@field pr? ForgeGhPR
---@field pr_check? ForgeGhPRCheck
---@field about? ForgeAICommitState
---@field fold_target_id? string
---@field diff_lines? table[]
---@field inline_jump_spans? table[]
---@field commit_subject_start_col? integer
---@field commit_subject_end_col? integer
---@field walkthrough_step? table

---@class ForgePrReplyDraft

---@alias ForgeStatusViewKind "status"|"pr"|"diff"|"review"

---@class ForgeListCursorTarget
---@field buf integer
---@field id string
---@field fallback_line integer

---@class ForgeVisualSelection
---@field buf integer
---@field entries ForgeStatusEntry[]
---@field start_line integer
---@field end_line integer
---@field cursor_target? ForgeListCursorTarget

---@class ForgeStatusPRState
---@field state "fetching"|"ready"|"closed"|"none"|"unavailable"|"error"
---@field pr? ForgeGhPR
---@field message? string
---@field lookup_started? boolean

---@class ForgeStatusRemoteActionState
---@field action "push"|"pull"
---@field state "running"

---@class ForgeStatusHeadLine
---@field segments table[]
---@field entry? ForgeStatusEntry
---@field parent_id? string
---@field default_folded? boolean

---@alias ForgeStatusSectionName "unstaged"|"staged"|"ignored"|"unmerged"|"recent"|"pr_commits"

---@class ForgeTreeSitterContextPending
---@field pending true
---@field callbacks table<string, fun(context?: ForgeHunkTreeSitterContext|string)>

---@class ForgeTreeSitterSyntax
---@field buf integer
---@field tree any
---@field highlight_query any?

---@class ForgeTreeSitterSyntaxPending
---@field pending true
---@field callbacks table<string, fun(syntax?: ForgeTreeSitterSyntax)>

---@class ForgeHunkTreeSitterContext
---@field label string
---@field start_row integer 0-based row
---@field end_row integer 0-based row
---@field start_text string
---@field end_text string
---@field start_segments ForgeHighlightSegment[]
---@field end_segments ForgeHighlightSegment[]
---@field ancestor_boundaries ForgeHunkBoundaryContext[]
---@field path_start_rows integer[] 1-based rows from the target node path
---@field path_end_rows integer[] 1-based rows from the target node path
---@field sibling_before_rows integer[] 1-based same-parent rows before the target row
---@field sibling_after_rows integer[] 1-based same-parent rows after the target row

---@class ForgeHunkBoundaryContext
---@field key string
---@field row integer 1-based row
---@field text string
---@field segments ForgeHighlightSegment[]
---@field end_row integer 1-based row
---@field end_text string
---@field end_segments ForgeHighlightSegment[]

---@class ForgeHighlightSegment
---@field text string
---@field hl_group? string
