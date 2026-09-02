--- Builds the head and about lines for the status, PR, and PR-detail views: the Head/Merge/Push
--- commit rows, the PR/about summary lines, section headings, and the async git fan-out that
--- gathers the values those builders format.
---
--- Reads the live status state and the conventional-commit/datetime/pr-overview/issues helpers
--- from session.lua and via direct requires, so the orchestrator owns state while this module owns the
--- head-line layout.

local ai_commit = require("diff_review.integrations.ai_commit")
local gh = require("diff_review.integrations.gh")
local git_backend = require("diff_review.git.git_backend")

local status_issues = require("diff_review.views.status.status_issues")
-- pr_overview edge kept lazy to avoid a load-time cycle.
local function pr_overview() return require("diff_review.views.pr.pr_overview") end
local fold_state = require("diff_review.views.status.fold_state")
local status_helpers = require("diff_review.views.status.status_helpers")
local conventional_commit = require("diff_review.integrations.conventional_commit")
local datetime = require("diff_review.integrations.datetime")
local session = require("diff_review.session")

local M = {}

--- Executes an asynchronous Git command and passes the trimmed first output line to a callback.
---@param cwd string Working directory path.
---@param args string[] Array of Git command line arguments.
---@param cb fun(line: string?) Callback receiving the trimmed line or nil on failure.
local function status_git_line_async(cwd, args, cb)
  local command = { "git", "-C", cwd }
  vim.list_extend(command, args)
  git_backend.systemlist_async(command, function(result, code)
    if code ~= 0 then
      cb(nil)
      return
    end
    cb(vim.trim(result[1] or ""))
  end)
end

--- Formats styled segments and subject column indices for a status head commit row.
---@param name string Label name (e.g. `"Head"`, `"Merge"`, `"Push"`).
---@param oid string Commit object identifier.
---@param ref string Reference branch or remote name.
---@param ref_hl string Highlight group name for the ref segment.
---@param subject string Commit subject text.
---@param date_text string? Optional formatted relative date string.
---@param ref_width integer? Column width padding for alignment.
---@return table[] segments Array of `[text, hl_group?]` tuples.
---@return integer subject_start_col Zero-based byte offset where subject starts.
---@return integer subject_end_col Zero-based byte offset where subject ends.
local function status_head_row(name, oid, ref, ref_hl, subject, date_text, ref_width)
  local segments = {}
  local byte_col = 0
  local function add_segment(text, hl_group)
    text = tostring(text or "")
    segments[#segments + 1] = { text, hl_group }
    byte_col = byte_col + #text
  end

  add_segment(("%-8s"):format(name .. ":"), "DiffReviewStatusLabel")
  add_segment(("%-7s"):format(oid or ""), "DiffReviewStatusObjectId")
  if date_text and date_text ~= "" then
    add_segment(" ")
    add_segment(date_text, "DiffReviewStatusDate")
  end
  local ref_text = ref or ""
  local ref_padding = ""
  if ref_width and ref_width > 0 then
    local padding_width = ref_width - vim.fn.strdisplaywidth(ref_text)
    if padding_width > 0 then ref_padding = string.rep(" ", padding_width) end
  end
  add_segment(" ")
  add_segment(ref_text, ref_hl)
  if ref_padding ~= "" then add_segment(ref_padding) end
  add_segment(" ")

  local subject_start_col = byte_col
  for _, segment in ipairs(conventional_commit.subject_segments(subject or "", nil)) do
    add_segment(segment[1], segment[2])
  end
  local subject_end_col = byte_col
  return segments, subject_start_col, subject_end_col
end

--- Constructs a commit message status entry descriptor for an interactive subject line.
---@param commit table? Commit record table.
---@param source string Label source identifier.
---@param start_col integer Zero-based byte column start of subject.
---@param end_col integer Zero-based byte column end of subject.
---@return DiffReviewStatusEntry? entry Status entry descriptor or nil.
function M._status_commit_message_entry(commit, source, start_col, end_col)
  if type(commit) ~= "table" then return nil end
  local oid = vim.trim(tostring(commit.oid or commit.sha or commit.id or ""))
  local subject = tostring(commit.subject or commit.messageHeadline or commit.message or "")
  if oid == "" and subject == "" then return nil end
  return {
    id = ("commit-message:%s:%s"):format(tostring(source or "commit"), oid ~= "" and oid or vim.fn.sha256(subject):sub(1, 12)),
    kind = "commit_message",
    commit = commit,
    commit_subject_start_col = start_col,
    commit_subject_end_col = end_col,
  }
end

--- Builds a structured status head commit line with formatted segments and interactive entry.
---@param name string Label name.
---@param oid string Commit object identifier.
---@param ref string Reference branch or remote name.
---@param ref_hl string Highlight group name for the ref segment.
---@param subject string Commit subject text.
---@param date_text string? Optional formatted relative date string.
---@param ref_width integer? Column width padding for alignment.
---@param commit table? Commit record table.
---@return DiffReviewStatusHeadLine line Structured status head line descriptor.
function M._status_head_commit_line(name, oid, ref, ref_hl, subject, date_text, ref_width, commit)
  local segments, subject_start_col, subject_end_col = status_head_row(name, oid, ref, ref_hl, subject, date_text, ref_width)
  return {
    segments = segments,
    entry = M._status_commit_message_entry(commit, name, subject_start_col, subject_end_col),
  }
end

--- Formats the interactive PR summary line for the status header.
---@param pr_state DiffReviewStatusPRState? PR status state record.
---@return DiffReviewStatusHeadLine line Structured PR head line descriptor.
local function status_pr_head_line(pr_state)
  local segments = {
    { ("%-8s"):format("PR:"), "DiffReviewStatusLabel" },
  }
  local entry = { id = "pr", kind = "pr" }

  if not pr_state or pr_state.state == "fetching" then
    segments[#segments + 1] = { "...fetching...", "DiffReviewStatusFetching" }
  elseif (pr_state.state == "ready" or pr_state.state == "closed") and pr_state.pr then
    entry.pr = pr_state.pr
    local subject = pr_state.pr.title ~= "" and pr_state.pr.title or ("PR #" .. tostring(pr_state.pr.number))
    vim.list_extend(segments, conventional_commit.subject_segments(subject, "DiffReviewStatusPR"))
    if pr_state.state == "closed" then segments[#segments + 1] = { " [closed]", "DiffReviewStatusClosed" } end
  elseif pr_state.state == "error" then
    segments[#segments + 1] = { "error", "ErrorMsg" }
  elseif pr_state.state == "unavailable" then
    segments[#segments + 1] = { "unavailable", "Comment" }
  else
    segments[#segments + 1] = { "none", "Comment" }
  end

  return { segments = segments, entry = entry }
end

--- Formats the interactive AI commit summary line for the status header.
---@param about_state DiffReviewAICommitState? AI commit generator state.
---@return DiffReviewStatusHeadLine line Structured About head line descriptor.
local function status_about_head_line(about_state)
  local segments = {
    { ("%-8s"):format("About:"), "DiffReviewStatusLabel" },
  }
  local entry = { id = "about", kind = "about", about = about_state }

  if not about_state or about_state.state == "none" then
    segments[#segments + 1] = { "none", "Comment" }
  elseif about_state.state == "generating" then
    segments[#segments + 1] = { "...generating...", "DiffReviewStatusFetching" }
  elseif about_state.state == "ready" and about_state.message then
    entry.about = about_state
    vim.list_extend(segments, conventional_commit.subject_segments(ai_commit.subject(about_state.message), "DiffReviewStatusPath"))
  elseif about_state.state == "error" then
    segments[#segments + 1] = { "error", "ErrorMsg" }
  else
    segments[#segments + 1] = { "none", "Comment" }
  end

  return { segments = segments, entry = entry }
end

--- Updates the rendered About line in-place within an active status buffer.
---@param buf integer Status buffer handle.
---@return boolean patched True if line was successfully replaced.
function M._status_patch_about_line(buf)
  local status = session.states and session.states[buf] or session.status
  if not status then return false end
  return status_helpers.status_patch_head_line(buf, "about", status_about_head_line(status.about))
end

--- Assembles all status header lines including branch refs, remote tracking, PR, and AI commit state.
---@param values table Git ref values gathered from background queries.
---@param pr_state DiffReviewStatusPRState? PR status state record.
---@param about_state DiffReviewAICommitState? AI commit generator state.
---@param issues_state table? Issue integration state.
---@return DiffReviewStatusHeadLine[] lines Array of structured status head lines.
local function status_build_head_lines(values, pr_state, about_state, issues_state)
  local remote_action = session.status and session.status.remote_action
  local ref_width = vim.fn.strdisplaywidth(values.branch or "(detached)")
  if values.upstream then ref_width = math.max(ref_width, vim.fn.strdisplaywidth(values.upstream)) end
  if values.push_ref then ref_width = math.max(ref_width, vim.fn.strdisplaywidth(values.push_ref)) end
  local lines = {}
  lines[#lines + 1] = M._status_head_commit_line(
    "Head",
    values.head_oid or "0000000",
    values.branch or "(detached)",
    "DiffReviewStatusBranch",
    values.subject or "(no commits)",
    nil,
    ref_width,
    {
      oid = "HEAD",
      short_oid = values.head_oid or "0000000",
      subject = values.subject or "(no commits)",
    }
  )

  if values.upstream then
    lines[#lines + 1] = M._status_head_commit_line(
      "Merge",
      values.upstream_oid or "",
      values.upstream,
      "DiffReviewStatusRemote",
      values.upstream_subject or "",
      nil,
      ref_width,
      {
        oid = values.upstream,
        short_oid = values.upstream_oid or "",
        subject = values.upstream_subject or "",
      }
    )
  end

  if remote_action and remote_action.action == "push" then
    lines[#lines + 1] = {
      segments = {
        { ("%-8s"):format("Push:"), "DiffReviewStatusLabel" },
        { remote_action.status or "Pushing...", "DiffReviewStatusFetching" },
      },
    }
  elseif values.push_ref then
    lines[#lines + 1] = M._status_head_commit_line(
      "Push",
      values.push_oid or "",
      values.push_ref,
      "DiffReviewStatusRemote",
      values.push_subject or "",
      nil,
      ref_width,
      {
        oid = values.push_ref,
        short_oid = values.push_oid or "",
        subject = values.push_subject or "",
      }
    )
  end

  lines[#lines + 1] = status_pr_head_line(pr_state)
  lines[#lines + 1] = status_about_head_line(about_state)
  lines[#lines + 1] = status_issues.head_line(issues_state)
  return lines
end

--- Splits markdown text into line arrays with fallback placeholder for empty strings.
---@param text string Source markdown text.
---@return string[] lines Array of markdown line strings.
local function status_markdown_lines(text)
  text = tostring(text or ""):gsub("\r\n", "\n")
  if text == "" then return { "_No description._" } end
  return vim.split(text, "\n", { plain = true })
end

--- Formats a section header label with item count string.
---@param title string Section title string.
---@param count integer Number of items in section.
---@return string text Formatted header text.
function M._status_section_heading_text(title, count)
  title = tostring(title or ""):gsub("%s*:%s*$", "")
  return ("%s (%d):"):format(title, math.max(0, math.floor(tonumber(count) or 0)))
end

--- Builds formatted highlight segment for a section header row.
---@param title string Section title string.
---@param count integer Number of items in section.
---@return table[] segments Highlight segment tuple array.
function M._status_section_heading_segments(title, count)
  return { { M._status_section_heading_text(title, count), "DiffReviewStatusHeader" } }
end

--- Calculates total item count within a status section across files, reviews, or commits.
---@param section DiffReviewStatusSection Status section descriptor.
---@return integer count Total count of elements.
function M._status_section_count(section)
  if type(section.reviews) == "table" then return #section.reviews end
  if type(section.issue_comments) == "table" then return #section.issue_comments end
  if type(section.commits) == "table" then return #section.commits end
  return #(section.files or {})
end

--- Returns a 7-character abbreviated object identifier with fallback for empty strings.
---@param oid string? Target object identifier string.
---@return string short_oid Abbreviated 7-character OID.
local function status_short_oid(oid)
  oid = tostring(oid or "")
  if oid == "" then return "0000000" end
  return oid:sub(1, 7)
end

--- Extracts headline subject, relative date string, and commit record for a PR's head commit.
---@param pr DiffReviewGhPR Pull request descriptor table.
---@return string subject Headline message or subject.
---@return string date_text Formatted relative date string.
---@return table? commit Matched commit record or nil.
local function status_pr_head_subject(pr)
  local commits = pr.commits or {}
  if type(commits) ~= "table" then return "", "", nil end
  local head_oid = vim.trim(tostring(pr.headRefOid or ""))
  local commit = nil
  if head_oid ~= "" then
    for commit_index = #commits, 1, -1 do
      local candidate = commits[commit_index]
      if type(candidate) == "table" then
        local oid = vim.trim(tostring(candidate.oid or candidate.sha or candidate.id or ""))
        if oid == head_oid then
          commit = candidate
          break
        end
      end
    end
  end
  commit = commit or commits[#commits]
  if type(commit) ~= "table" then return "", "", nil end
  local date_text = datetime.relative(
    commit.committedDate or commit.committed_date or commit.committed_at or commit.authoredDate or commit.authored_date or commit.authored_at,
    { yesterday = false }
  )
  return tostring(commit.messageHeadline or commit.subject or commit.message or ""), date_text, commit
end

--- Builds multi-line header display for PR detail views including metadata and description.
---@param pr DiffReviewGhPR Pull request descriptor table.
---@param status table? Active status session state table.
---@return DiffReviewStatusHeadLine[] lines Array of structured header line descriptors.
local function status_pr_detail_head_lines(pr, status)
  local description_section_id = "pr-head-section:description"
  local title = pr.title ~= "" and pr.title or ("PR #" .. tostring(pr.number))
  local lines = {
    { segments = { { "Title:  ", "DiffReviewStatusLabel" }, { title, "DiffReviewStatusPath" } } },
  }
  if pr.repo and pr.repo ~= "" then
    lines[#lines + 1] = { segments = { { "Repo:   ", "DiffReviewStatusLabel" }, { pr.repo, "DiffReviewStatusRemote" } } }
  end
  local head_subject, head_date, head_commit = status_pr_head_subject(pr)
  local status_commit = pr_overview().status_commit_from_pr_commit(head_commit, pr.headRefName)
    or {
      oid = pr.headRefOid,
      short_oid = status_short_oid(pr.headRefOid),
      subject = head_subject,
    }
  lines[#lines + 1] = M._status_head_commit_line(
    "Head",
    status_short_oid(pr.headRefOid),
    pr.headRefName or "",
    "DiffReviewStatusBranch",
    head_subject,
    head_date,
    nil,
    status_commit
  )
  if pr.url and pr.url ~= "" then
    lines[#lines + 1] = { segments = { { "URL:    ", "DiffReviewStatusLabel" }, { pr.url, "DiffReviewStatusPR" } } }
  end
  lines[#lines + 1] = {
    segments = {
      { "Release: ", "DiffReviewStatusLabel" },
      { pr_overview().milestone_text(pr), "DiffReviewStatusBranch" },
    },
  }
  local pending_review_text = pr_overview().pending_review_text(pr, status)
  lines[#lines + 1] = {
    segments = {
      { "Review: ", "DiffReviewStatusLabel" },
      { pending_review_text, "DiffReviewReviewPending" },
    },
  }
  lines[#lines + 1] = {
    segments = {
      { "Status: ", "DiffReviewStatusLabel" },
      { pr_overview().status_text(pr), pr_overview().status_highlight(pr) },
    },
  }
  lines[#lines + 1] = {
    segments = {
      { "Activity: ", "DiffReviewStatusLabel" },
      { pr_overview().activity_text(pr, status), "DiffReviewStatusDate" },
    },
  }
  lines[#lines + 1] = { segments = { { "" } } }
  lines[#lines + 1] = {
    segments = { { "Description:", "DiffReviewStatusHeader" } },
    entry = { id = description_section_id, kind = "pr_head_section", default_folded = false },
  }
  if not fold_state._status_folded(description_section_id, false, status) then
    for line_index, line in ipairs(status_markdown_lines(pr.body)) do
      lines[#lines + 1] = {
        segments = { { line } },
        parent_id = description_section_id,
        entry = {
          id = description_section_id .. ":line:" .. tostring(line_index),
          kind = "pr_head_line",
          fold_target_id = description_section_id,
        },
      }
    end
  end
  vim.list_extend(lines, pr_overview().check_head_lines(pr, status))
  return lines
end

--- Enables repository issue and user omnicompletion across active status buffers.
---@param cwd string? Repository working directory path.
---@param repo string? Repository identifier string (`"owner/repo"`).
function M._status_enable_repo_completion(cwd, repo)
  if not (cwd and repo and repo ~= "") then return end
  local cache = require("github.repo_cache")
  for buf, state in pairs(session.states or {}) do
    if state and state.cwd == cwd and vim.api.nvim_buf_is_valid(buf) then
      cache.enable_user_completion(buf, repo)
    end
  end
end

--- Asynchronously loads GitHub repository contributors, metadata, and issue cache.
---@param cwd string? Repository working directory path.
---@param repo string? Optional explicit repository name (`"owner/repo"`).
function M.github_load_repo_metadata(cwd, repo)
  local cache = require("github.repo_cache")
  local issue_index_ok, issue_index = pcall(require, "github.issue_index")
  if repo and repo ~= "" then
    M._status_enable_repo_completion(cwd, repo)
    cache.ensure_metadata(cwd, repo, function(done)
      gh.repo_contributors_async(cwd, repo, done)
    end, { remember_cwd = false })
    if issue_index_ok then issue_index.ensure_repo(cwd, repo, { manual = false }) end
    return
  end
  local cached_repo = cache.repo_for_cwd(cwd)
  if cached_repo then M._status_enable_repo_completion(cwd, cached_repo) end
  cache.ensure_metadata_for_cwd(cwd, function(done)
    gh.current_repo_async(cwd, function(result)
      if result and result.ok and result.repo then M._status_enable_repo_completion(cwd, result.repo) end
      done(result)
    end)
  end, function(resolved_repo, done)
    M._status_enable_repo_completion(cwd, resolved_repo)
    gh.repo_contributors_async(cwd, resolved_repo, done)
  end)
  if issue_index_ok then issue_index.ensure_current(cwd, { manual = false }) end
end

--- Gathers Git refs and status values concurrently and constructs status header lines.
---@param cwd string Repository working directory path.
---@param cb fun(lines: DiffReviewStatusHeadLine[], values: table) Callback receiving formatted lines and raw values.
local function status_head_lines_async(cwd, cb)
  local values = {}
  local pending = 9

  local function done(key, value)
    values[key] = value
    pending = pending - 1
    if pending > 0 then return end

    local status = session.status
    local pr_state = status and status.pr
    local about_state = status and status.about
    local issues_state = status_issues.ensure_state(status, cwd)
    cb(status_build_head_lines(values, pr_state, about_state, issues_state), values)
  end

  status_git_line_async(cwd, { "rev-parse", "--short", "HEAD" }, function(line) done("head_oid", line) end)
  status_git_line_async(cwd, { "rev-parse", "--abbrev-ref", "HEAD" }, function(line) done("branch", line) end)
  status_git_line_async(cwd, { "log", "-1", "--format=%s" }, function(line) done("subject", line) end)
  status_git_line_async(cwd, { "rev-parse", "--abbrev-ref", "@{upstream}" }, function(line) done("upstream", line) end)
  status_git_line_async(cwd, { "rev-parse", "--short", "@{upstream}" }, function(line) done("upstream_oid", line) end)
  status_git_line_async(cwd, { "log", "-1", "--format=%s", "@{upstream}" }, function(line) done("upstream_subject", line) end)
  status_git_line_async(cwd, { "rev-parse", "--abbrev-ref", "@{push}" }, function(line) done("push_ref", line) end)
  status_git_line_async(cwd, { "rev-parse", "--short", "@{push}" }, function(line) done("push_oid", line) end)
  status_git_line_async(cwd, { "log", "-1", "--format=%s", "@{push}" }, function(line) done("push_subject", line) end)
end

-- Expose the bare-local head builders that init and other modules call by name.
M._status_build_head_lines = status_build_head_lines
M._status_pr_detail_head_lines = status_pr_detail_head_lines
M._status_head_lines_async = status_head_lines_async
M._status_markdown_lines = status_markdown_lines
M._status_short_oid = status_short_oid
M._status_pr_head_line = status_pr_head_line

return M
