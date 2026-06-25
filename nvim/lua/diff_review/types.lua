--- Shared LuaCATS type definitions for the diff_review plugin.
---
--- Annotation-only: lua-language-server resolves these across the workspace, so nothing
--- requires this file at runtime. Function-local option/result types stay beside their code.

---@alias DiffReviewGitCommand string[]

---@class DiffReviewGitCommandResult
---@field ok boolean
---@field code integer
---@field output string
---@field stdout? string
---@field stderr? string
---@field root? string
---@field args DiffReviewGitCommand

---@class DiffReviewGitAsyncResult
---@field code integer
---@field stdout string
---@field stderr string
---@field output string

---@alias DiffReviewGitTextCallback fun(result: DiffReviewGitAsyncResult)
---@alias DiffReviewGitListCallback fun(output: string[], code: integer, stderr?: string)

---@class DiffReviewGitFailure
---@field file? string
---@field path? string
---@field message? string
---@field output? string
---@field stderr? string
---@field stdout? string
---@field code? integer

---@class DiffReviewGitBackend
---@field systemlist? fun(command: DiffReviewGitCommand): string[]|string, integer?
---@field system? fun(command: DiffReviewGitCommand, input?: string): string, integer?
---@field systemlist_async? fun(command: DiffReviewGitCommand, cb: DiffReviewGitListCallback)
---@field system_async? fun(command: DiffReviewGitCommand, input: string?, cb: DiffReviewGitTextCallback)
---@field system_stream_async? fun(command: DiffReviewGitCommand, input: string?, on_line: fun(line: string), cb: DiffReviewGitTextCallback)
---@field delete? fun(path: string): integer

---@class DiffReviewHunk
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
---@field raw_hunks? DiffReviewHunk[]

---@class DiffReviewStatusFile
---@field filename string
---@field relpath string
---@field original_relpath? string
---@field section_name string
---@field added integer
---@field removed integer
---@field hunks DiffReviewHunk[]
---@field untracked boolean
---@field status string
---@field git_status? string

---@class DiffReviewStatusCommit
---@field oid string
---@field short_oid string
---@field branch? string
---@field subject string
---@field committed_at? string
---@field authored_at? string
---@field upstream? string
---@field files? DiffReviewStatusFile[]
---@field files_loaded? boolean
---@field files_loading? boolean
---@field files_error? string

---@class DiffReviewStatusSection
---@field name string
---@field title string
---@field default_folded boolean
---@field files DiffReviewStatusFile[]
---@field files_by_name table<string, DiffReviewStatusFile>
---@field commits? DiffReviewStatusCommit[]
---@field reviews? DiffReviewGhSubmittedReview[]
---@field issue_comments? table[]
---@field upstream? string
---@field file_key_prefix? string
---@field file_entry_kind? "file"|"commit_file"|"pr_file"|"pr_review_file"
---@field hunk_entry_kind? "hunk"|"commit_hunk"|"pr_hunk"|"pr_review_hunk"

---@class DiffReviewStatusEntry
---@field id? string
---@field kind "section"|"file"|"hunk"|"context_line"|"commit"|"commit_message"|"commit_file"|"commit_hunk"|"pr_file"|"pr_hunk"|"pr_comment"|"pr_comment_reply"|"pr_review"|"pr_review_file"|"pr_review_hunk"|"review_comment"|"pr"|"about"|"pr_check"|"pr_head_section"|"pr_head_line"
---@field section? DiffReviewStatusSection
---@field file? DiffReviewStatusFile
---@field hunk? DiffReviewHunk
---@field commit? DiffReviewStatusCommit
---@field pr_review? DiffReviewGhSubmittedReview
---@field pr_comment? DiffReviewGhPendingReviewComment|DiffReviewGhIssueComment
---@field pr_comment_body? boolean
---@field pr_comment_body_index? integer
---@field pr_comment_reply? DiffReviewGhReviewCommentReply
---@field review_comment? table
---@field review_reply? table
---@field diff_line? table
---@field pr? DiffReviewGhPR
---@field pr_check? DiffReviewGhPRCheck
---@field about? DiffReviewAICommitState
---@field fold_target_id? string
---@field diff_lines? table[]
---@field inline_jump_spans? table[]
---@field commit_subject_start_col? integer
---@field commit_subject_end_col? integer
---@field walkthrough_step? table

---@alias DiffReviewStatusViewKind "status"|"pr"|"diff"|"review"

---@class DiffReviewStatusPRState
---@field state "fetching"|"ready"|"none"|"unavailable"|"error"
---@field pr? DiffReviewGhPR
---@field message? string
---@field lookup_started? boolean

---@class DiffReviewStatusRemoteActionState
---@field action "push"|"pull"
---@field state "running"

---@class DiffReviewStatusHeadLine
---@field segments table[]
---@field entry? DiffReviewStatusEntry
---@field parent_id? string
---@field default_folded? boolean

---@alias DiffReviewStatusSectionName "unstaged"|"staged"|"unmerged"|"recent"|"pr_commits"

---@class DiffReviewTreeSitterContextPending
---@field pending true
---@field callbacks table<string, fun(context?: DiffReviewHunkTreeSitterContext|string)>

---@class DiffReviewTreeSitterSyntax
---@field buf integer
---@field tree TSTree
---@field highlight_query vim.treesitter.Query?

---@class DiffReviewTreeSitterSyntaxPending
---@field pending true
---@field callbacks table<string, fun(syntax?: DiffReviewTreeSitterSyntax)>

---@class DiffReviewHunkTreeSitterContext
---@field label string
---@field start_row integer 0-based row
---@field end_row integer 0-based row
---@field start_text string
---@field end_text string
---@field start_segments DiffReviewHighlightSegment[]
---@field end_segments DiffReviewHighlightSegment[]
---@field ancestor_boundaries DiffReviewHunkBoundaryContext[]
---@field path_start_rows integer[] 1-based rows from the target node path
---@field path_end_rows integer[] 1-based rows from the target node path
---@field sibling_before_rows integer[] 1-based same-parent rows before the target row
---@field sibling_after_rows integer[] 1-based same-parent rows after the target row

---@class DiffReviewHunkBoundaryContext
---@field key string
---@field row integer 1-based row
---@field text string
---@field segments DiffReviewHighlightSegment[]
---@field end_row integer 1-based row
---@field end_text string
---@field end_segments DiffReviewHighlightSegment[]

---@class DiffReviewHighlightSegment
---@field text string
---@field hl_group? string
