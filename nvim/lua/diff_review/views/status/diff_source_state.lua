--- Owns per-file diff source state for the status views: the diff-source registry and policies,
--- the commit source handles, per-file state creation with git text loaders, hunk/annotation
--- population, and the layout build that feeds the renderer.
---
--- Reads the source/loader/annotation/layout/syntax/hunk-index models, the size-gate and key
--- builders, pr_overview, and live status state through the init module via dr().

local git_backend = require("diff_review.git.git_backend")
local paths = require("diff_review.infra.paths")

--- Resolve the init module lazily so source-state building can reach the shared data-model seams
--- and orchestrator state without a load-time circular require.
local function dr()
  return require("diff_review")
end

local M = {}

function M._status_diff_source_kind(file, entry_kind, hunk_entry_kind)
  if entry_kind == "commit_file" or hunk_entry_kind == "commit_hunk" then return "commit" end
  if entry_kind == "pr_review_file" or hunk_entry_kind == "pr_review_hunk" then return "review" end
  if entry_kind == "pr_file" or hunk_entry_kind == "pr_hunk" then return "pr" end
  if dr()._status and dr()._status.view_kind == "diff" then return "branch" end
  if file and file.section_name == "staged" then return "staged" end
  return "unstaged"
end

---@param file DiffReviewStatusFile
---@param entry_kind? string
---@param hunk_entry_kind? string
---@return string
function M._status_diff_source_id(file, entry_kind, hunk_entry_kind)
  if file and file.diff_source_id then return file.diff_source_id end
  local section_name = file and file.section_name or nil
  if section_name and section_name ~= "" then return section_name end
  return dr()._status_diff_source_kind(file, entry_kind, hunk_entry_kind)
end

---@param status table
---@return DiffReviewDiffSourceRegistry
function M._status_diff_source_registry(status)
  status.diff_source_registry = status.diff_source_registry or dr()._diff_source_model.new_registry()
  return status.diff_source_registry
end

---@param registry DiffReviewDiffSourceRegistry
function M._status_configure_diff_source_policies(registry)
  if not registry then return end
  dr()._diff_source_model.set_kind_policy(registry, "unstaged", {
    stage = true,
    discard = true,
    jump = true,
  })
  dr()._diff_source_model.set_kind_policy(registry, "staged", {
    unstage = true,
    discard = true,
    jump = true,
  })
  dr()._diff_source_model.set_kind_policy(registry, "pr", {
    comment = true,
    jump = true,
  })
  dr()._diff_source_model.set_kind_policy(registry, "review", {
    comment = true,
    viewed = true,
    unviewed = true,
    jump = true,
  })
  dr()._diff_source_model.set_kind_policy(registry, "commit", {
    jump = true,
  })
  dr()._diff_source_model.set_kind_policy(registry, "branch", {
    jump = true,
  })
  dr()._diff_source_model.set_kind_policy(registry, "walkthrough", {
    jump = true,
  })
end

---@param commit DiffReviewStatusCommit
---@return string?
function M._status_commit_source_id(commit)
  if not (commit and commit.oid) then return nil end
  return "commit:" .. tostring(commit.oid)
end

---@param commit DiffReviewStatusCommit
---@param status table
---@return DiffReviewDiffSourceHandle?
function M._status_commit_source_handle(commit, status)
  local source_id = dr()._status_commit_source_id(commit)
  if not (source_id and status and status.cwd) then return nil end
  local cwd = status.cwd
  return {
    id = source_id,
    kind = "commit",
    label = commit.short_oid or commit.oid,
    cwd = cwd,
    commit_oid = commit.oid,
    lazy = true,
    metadata = {
      view_kind = status.view_kind,
      subject = commit.subject,
    },
    load = function(source_state, done)
      dr()._status_perf_event("source.commit.load.start", status.buf, {
        source_id = source_id,
        commit_oid = commit.oid,
      })
      git_backend.systemlist_async(dr()._git_show_diff_command(cwd, commit.oid), function(output, code, stderr)
        if code ~= 0 then
          local message = vim.trim(tostring(stderr or ""))
          done(false, message ~= "" and message or "Unable to load commit diff")
          return
        end
        local diff_text = table.concat(output or {}, "\n")
        local files = dr()._status_perf_span("source.commit.parse", status.buf, {
          source_id = source_id,
          diff_len = #diff_text,
        }, function()
          return dr()._status_commit_files_from_diff(cwd, commit, diff_text)
        end)
        dr()._status_populate_commit_source_files(source_state, commit, files, status)
        source_state.metadata = source_state.metadata or {}
        source_state.metadata.files = files
        source_state.metadata.diff_len = #diff_text
        dr()._status_perf_event("source.commit.load.done", status.buf, {
          source_id = source_id,
          file_count = #files,
        })
        done(true)
      end)
    end,
  }
end

---@param commit DiffReviewStatusCommit
function M._status_register_commit_source_handle(commit)
  local status = dr()._status
  if not (status and status.diff_source_registry and commit and commit.oid) then return end
  local handle = dr()._status_commit_source_handle(commit, status)
  if not handle then return end
  dr()._diff_source_model.ensure_handle(status.diff_source_registry, handle)
  commit.diff_source_id = handle.id
end

---@param commit DiffReviewStatusCommit
---@return DiffReviewDiffSourceState?
function M._status_ensure_commit_source_state(commit)
  local status = dr()._status
  if not (status and status.diff_source_registry and commit and commit.oid) then return nil end
  local handle = dr()._status_commit_source_handle(commit, status)
  if not handle then return nil end
  local loader = dr()._diff_source_loader_model.ensure(status.diff_source_registry, handle)
  commit.diff_source_id = handle.id
  return loader.source
end

---@param comment table
---@return string?
function M._status_annotation_id(comment)
  if not comment then return nil end
  local id = comment.local_id or comment.remote_node_id or comment.remote_id or comment.id
  if id then return tostring(id) end
  return nil
end

---@param comment table
---@return DiffReviewAnnotationState
function M._status_annotation_state(comment)
  if not comment then return "clean" end
  if comment.local_state == "deleted" then return "deleted" end
  if comment.local_state == "error" then return "error" end
  if comment.local_state == "new" then return "new" end
  if comment.local_state == "dirty" then return "dirty" end
  if comment.local_state == "syncing" then return "syncing" end
  if comment.dirty == true then return "dirty" end
  return "clean"
end

---@param file_state DiffReviewDiffFileState
---@param comment table
---@param kind string
function M._status_add_diff_file_annotation(file_state, comment, kind)
  local id = dr()._status_annotation_id(comment)
  if not id then return end
  local annotation = dr()._diff_annotation_model.upsert(file_state.annotation_index, {
    id = id,
    kind = kind,
    source_id = file_state.source_id,
    file_key = file_state.key,
    path = file_state.path,
    side = comment.side or comment.original_side or comment.start_side or "RIGHT",
    line = tonumber(comment.line or comment.original_line or comment.start_line) or 0,
    end_line = tonumber(comment.end_line) or nil,
    body = comment.body or "",
    base_body = comment.base_body or comment.body or "",
    remote_body = comment.remote_body or comment.body or "",
    author = comment.author,
    remote_id = comment.remote_node_id or comment.remote_id or comment.id,
    editable = comment.editable == true,
    state = dr()._status_annotation_state(comment),
    metadata = {
      comment = comment,
      renderer = kind,
    },
  })
  dr()._diff_source_model.add_annotation(file_state, annotation)
end

---@param file DiffReviewStatusFile
---@param source_kind DiffReviewDiffSourceKind
---@return DiffReviewDiffFileStageState
function M._status_diff_stage_state(file, source_kind)
  if source_kind == "review" then return "unviewed" end
  if source_kind == "commit" or source_kind == "pr" or source_kind == "branch" then return "readonly" end
  if file and file.section_name == "staged" then return "staged" end
  return "unstaged"
end

---@param file DiffReviewStatusFile
---@param status table?
---@return string
function M._status_diff_file_path(file, status)
  if file and status and status.cwd and file.filename then
    local relative = paths.repo_relative(file.filename, status.cwd)
    if relative and relative ~= "" then return relative:gsub("\\", "/") end
  end
  local relpath = file and file.relpath or nil
  if relpath and relpath ~= "" and not relpath:match("^%a:[/\\]") and not relpath:match("^/") then
    return relpath:gsub("\\", "/")
  end
  return tostring((file and (file.filename or file.relpath)) or ""):gsub("\\", "/")
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
---@param status table
---@param source_kind string
function M._status_set_diff_file_text_loaders(file_state, file, status, source_kind)
  if not (file_state and file and status and status.cwd) then return end
  local relpath = dr()._status_diff_file_path(file, status)
  if source_kind == "commit" and file.diff_source_id then
    local commit_oid = tostring(file.diff_source_id):gsub("^commit:", "")
    dr()._diff_source_model.set_text_loader(file_state, "new", function(done)
      git_backend.systemlist_async({ "git", "-C", status.cwd, "show", ("%s:%s"):format(commit_oid, relpath) }, function(lines, code, output)
        if code ~= 0 then
          done(false, nil, nil, vim.trim(tostring(output or "")))
          return
        end
        done(true, table.concat(lines or {}, "\n"), commit_oid)
      end)
    end)
    dr()._diff_source_model.set_text_loader(file_state, "old", function(done)
      git_backend.systemlist_async({ "git", "-C", status.cwd, "show", ("%s^:%s"):format(commit_oid, file.original_relpath or relpath) }, function(lines, code, output)
        if code ~= 0 then
          done(false, nil, nil, vim.trim(tostring(output or "")))
          return
        end
        done(true, table.concat(lines or {}, "\n"), commit_oid .. "^")
      end)
    end)
    return
  end
  dr()._diff_source_model.set_text_loader(file_state, "new", function(done)
    local ok, lines = pcall(vim.fn.readfile, file.filename)
    if not ok then
      done(false, nil, nil, tostring(lines))
      return
    end
    done(true, table.concat(lines or {}, "\n"), "worktree")
  end)
  local old_revision = nil
  if source_kind == "unstaged" then
    old_revision = ":0"
  elseif source_kind == "staged" then
    old_revision = "HEAD"
  elseif source_kind == "branch" and status.diff_branch then
    old_revision = status.diff_branch
  elseif source_kind == "commit" and file.diff_source_id then
    old_revision = tostring(file.diff_source_id):gsub("^commit:", "")
  end
  if not old_revision or old_revision == "" then return end
  dr()._diff_source_model.set_text_loader(file_state, "old", function(done)
    git_backend.systemlist_async({ "git", "-C", status.cwd, "show", ("%s:%s"):format(old_revision, relpath) }, function(lines, code, output)
      if code ~= 0 then
        done(false, nil, nil, vim.trim(tostring(output or "")))
        return
      end
      done(true, table.concat(lines or {}, "\n"), old_revision)
    end)
  end)
end

---@param file DiffReviewStatusFile
---@param entry_kind? string
---@param hunk_entry_kind? string
---@param file_key string
---@return DiffReviewDiffFileState?
function M._status_ensure_diff_file_state(file, entry_kind, hunk_entry_kind, file_key)
  local status = dr()._status
  if not (status and file) then return nil end
  local source_id = dr()._status_diff_source_id(file, entry_kind, hunk_entry_kind)
  local source_kind = dr()._status_diff_source_kind(file, entry_kind, hunk_entry_kind)
  local source_handle = {
    id = source_id,
    kind = source_kind,
    label = file.section_name,
    cwd = status.cwd,
    lazy = false,
    metadata = {
      view_kind = status.view_kind,
      section_name = file.section_name,
    },
  }
  if (source_kind == "unstaged" or source_kind == "staged") and dr()._status_source_reload_paths_loader then
    source_handle.reload_paths = dr()._status_source_reload_paths_loader(source_kind, source_id, status)
  end
  local registry = dr()._status_diff_source_registry(status)
  local file_path = dr()._status_diff_file_path(file, status)
  local file_state = dr()._diff_source_loader_model.ensure_file(registry, source_handle, file_path, {
    original_path = file.original_relpath,
    status = file.git_status or file.status,
    stage_state = dr()._status_diff_stage_state(file, source_kind),
    added = file.added,
    removed = file.removed,
    expanded = dr()._status_folded(file_key, false) == false,
    metadata = {
      status_file = file,
      file_key = file_key,
      entry_kind = entry_kind,
      hunk_entry_kind = hunk_entry_kind,
    },
  })
  file_state.source_id = source_id
  file_state.status = file.git_status or file.status
  file_state.stage_state = dr()._status_diff_stage_state(file, source_kind)
  file_state.added = file.added
  file_state.removed = file.removed
  file_state.expanded = dr()._status_folded(file_key, false) == false
  file_state.metadata = file_state.metadata or {}
  file_state.metadata.status_file = file
  file_state.metadata.file_key = file_key
  file_state.metadata.entry_kind = entry_kind
  file_state.metadata.hunk_entry_kind = hunk_entry_kind
  file.diff_source_id = source_id
  file.diff_file_key = file_state.key
  file_state.syntax_context = dr()._diff_syntax_context_model.ensure_context(file_state.syntax_context, file_state.key)
  dr()._status_set_diff_file_text_loaders(file_state, file, status, source_kind)
  return file_state
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
---@param hunks DiffReviewHunk[]
function M._status_populate_diff_file_hunks(file_state, file, hunks)
  local file_path = dr()._status_diff_file_path(file, dr()._status)
  for hunk_index, hunk in ipairs(hunks or {}) do
    hunk.source_id = file_state.source_id
    hunk.file_key = file_state.key
    hunk.raw_hunks = hunk.raw_hunks or dr()._diff_source_model.raw_hunks_from_diff(hunk.diff or "", {
      id_prefix = ("%s:%s:%d"):format(file_state.source_id, file_path, hunk_index),
      source_id = file_state.source_id,
      file_key = file_state.key,
      staged = hunk.staged,
      metadata = {
        status_hunk = hunk,
      },
    })
    for _, raw_hunk in ipairs(hunk.raw_hunks or {}) do
      dr()._diff_source_model.add_raw_hunk(file_state, raw_hunk)
    end
  end
end

---@param source_state DiffReviewDiffSourceState
---@param commit DiffReviewStatusCommit
---@param files DiffReviewStatusFile[]
---@param status table
function M._status_populate_commit_source_files(source_state, commit, files, status)
  if not (source_state and commit and status) then return end
  source_state.file_by_key = {}
  source_state.file_order = {}
  source_state.revision = (source_state.revision or 0) + 1
  for _, file in ipairs(files or {}) do
    file.diff_source_id = source_state.handle.id
    local file_key = dr()._status_keys.commit_file_key(commit.oid, file.filename)
    local file_path = dr()._status_diff_file_path(file, status)
    local file_state = dr()._diff_source_model.ensure_file(source_state, file_path, {
      original_path = file.original_relpath,
      status = file.git_status or file.status,
      stage_state = "readonly",
      added = file.added,
      removed = file.removed,
      expanded = dr()._status_folded(file_key, true) == false,
      metadata = {
        status_file = file,
        file_key = file_key,
        entry_kind = "commit_file",
        hunk_entry_kind = "commit_hunk",
        commit_oid = commit.oid,
      },
    })
    file.diff_file_key = file_state.key
    file_state.hunks = {}
    file_state.hunk_index_by_id = {}
    file_state.annotations = {}
    file_state.annotation_index = dr()._diff_annotation_model.new_index()
    file_state.layout = nil
    file_state.body_layout = nil
    file_state.layout_dirty = true
    file_state.status = file.git_status or file.status
    file_state.stage_state = "readonly"
    file_state.added = file.added
    file_state.removed = file.removed
    file_state.expanded = dr()._status_folded(file_key, true) == false
    file_state.metadata = file_state.metadata or {}
    file_state.metadata.status_file = file
    file_state.metadata.file_key = file_key
    file_state.metadata.entry_kind = "commit_file"
    file_state.metadata.hunk_entry_kind = "commit_hunk"
    file_state.metadata.commit_oid = commit.oid
    file_state.syntax_context = dr()._diff_syntax_context_model.ensure_context(file_state.syntax_context, file_state.key)
    dr()._status_set_diff_file_text_loaders(file_state, file, status, "commit")
    dr()._status_populate_diff_file_hunks(file_state, file, file.hunks or {})
  end
end

---@param source_state DiffReviewDiffSourceState
---@param files DiffReviewStatusFile[]
---@param status table
---@param source_kind DiffReviewDiffSourceKind
---@param source_id string
function M._status_populate_reloaded_source_files(source_state, files, status, source_kind, source_id)
  if not (source_state and status and source_kind and source_id) then return end
  for _, file in ipairs(files or {}) do
    file.diff_source_id = source_id
    local file_key = dr()._status_keys.file_key(source_kind, file.filename)
    local file_path = dr()._status_diff_file_path(file, status)
    local file_state = dr()._diff_source_model.ensure_file(source_state, file_path, {
      original_path = file.original_relpath,
      status = file.git_status or file.status,
      stage_state = dr()._status_diff_stage_state(file, source_kind),
      added = file.added,
      removed = file.removed,
      expanded = dr()._status_folded(file_key, true) == false,
      metadata = {
        status_file = file,
        file_key = file_key,
        entry_kind = "file",
        hunk_entry_kind = "hunk",
      },
    })
    file.diff_file_key = file_state.key
    file_state.hunks = {}
    file_state.hunk_index_by_id = {}
    file_state.annotations = {}
    file_state.annotation_index = dr()._diff_annotation_model.new_index()
    file_state.layout = nil
    file_state.body_layout = nil
    file_state.layout_dirty = true
    file_state.pending = false
    file_state.stale = false
    file_state.status = file.git_status or file.status
    file_state.stage_state = dr()._status_diff_stage_state(file, source_kind)
    file_state.added = file.added
    file_state.removed = file.removed
    file_state.expanded = dr()._status_folded(file_key, true) == false
    file_state.metadata = file_state.metadata or {}
    file_state.metadata.status_file = file
    file_state.metadata.file_key = file_key
    file_state.metadata.entry_kind = "file"
    file_state.metadata.hunk_entry_kind = "hunk"
    file_state.syntax_context = dr()._diff_syntax_context_model.ensure_context(file_state.syntax_context, file_state.key)
    dr()._status_set_diff_file_text_loaders(file_state, file, status, source_kind)
    dr()._status_populate_diff_file_hunks(file_state, file, file.hunks or {})
  end
end

---@param source_kind DiffReviewDiffSourceKind
---@param source_id string
---@param status table
---@return fun(source_state: DiffReviewDiffSourceState, paths: string[], done: fun(ok: boolean, err?: string))
function M._status_source_reload_paths_loader(source_kind, source_id, status)
  return function(source_state, paths, done)
    local cwd = status and status.cwd
    if not cwd then
      done(false, "missing git root")
      return
    end
    local extra_args = {}
    if source_kind == "staged" then extra_args[#extra_args + 1] = "--cached" end
    if #(paths or {}) > 0 then
      extra_args[#extra_args + 1] = "--"
      for _, path in ipairs(paths or {}) do
        extra_args[#extra_args + 1] = path
      end
    end
    dr()._status_perf_event("source.reload_paths.start", status.buf, {
      source_id = source_id,
      source_kind = source_kind,
      paths = paths,
    })
    git_backend.systemlist_async(dr()._git_diff_command(cwd, extra_args), function(output, code, stderr)
      if code ~= 0 then
        local message = vim.trim(tostring(stderr or ""))
        done(false, message ~= "" and message or "git diff failed")
        return
      end
      for _, path in ipairs(paths or {}) do
        dr()._diff_source_model.remove_file(source_state, path)
      end
      local diff_text = table.concat(output or {}, "\n")
      local files = dr()._status_files_from_diff_provider(cwd, {
        section_name = source_id,
        default_status = "modified",
      }, diff_text)
      dr()._status_populate_reloaded_source_files(source_state, files, status, source_kind, source_id)
      source_state.metadata = source_state.metadata or {}
      source_state.metadata.last_reload_paths = vim.deepcopy(paths or {})
      dr()._status_perf_event("source.reload_paths.done", status.buf, {
        source_id = source_id,
        source_kind = source_kind,
        path_count = #(paths or {}),
        file_count = #files,
      })
      done(true)
    end)
  end
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
function M._status_populate_diff_file_annotations(file_state, file)
  local status = dr()._status
  for _, comment in ipairs(type(file.pr_comments) == "table" and file.pr_comments or {}) do
    dr()._status_add_diff_file_annotation(file_state, comment, "pr_comment")
  end
  for _, comment in ipairs(type(file.pr_review_comments) == "table" and file.pr_review_comments or {}) do
    dr()._status_add_diff_file_annotation(file_state, comment, "pr_review_comment")
  end
  for _, comment in ipairs(type(status and status.review_comments) == "table" and status.review_comments or {}) do
    if dr()._status_lazy_comment_matches_file(file, comment) then
      dr()._status_add_diff_file_annotation(file_state, comment, "draft_comment")
    end
  end
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return string[]
function M._status_diff_hunk_annotation_ids(file_state, file, hunk)
  local ids = {}
  for _, annotation in ipairs(file_state.annotations or {}) do
    local comment = annotation.metadata and annotation.metadata.comment or nil
    if comment and dr()._status_lazy_comment_matches_file(file, comment) and dr()._pr_overview.hunk_contains_comment(hunk, comment) then
      ids[#ids + 1] = annotation.id
    end
  end
  return ids
end

---@param file_state DiffReviewDiffFileState
---@param file DiffReviewStatusFile
---@param display_hunks DiffReviewHunk[]
function M._status_build_diff_file_layout(file_state, file, display_hunks)
  local key_by_item = {}
  local body_blocks = {}
  for hunk_index, hunk in ipairs(display_hunks or {}) do
    local key = hunk.display_id or ("%s:display:%d"):format(file_state.key, hunk_index)
    key_by_item[#key_by_item + 1] = key
    local height = dr()._status_lazy_hunk_estimate(file, hunk)
    body_blocks[#body_blocks + 1] = {
      key = key,
      kind = "hunk",
      height = height,
      metadata = {
        kind = "hunk",
        hunk = hunk,
        raw_hunks = hunk.raw_hunks or {},
        annotation_ids = dr()._status_diff_hunk_annotation_ids(file_state, file, hunk),
      },
    }
  end

  file_state.body_layout = dr()._diff_layout_model.new_file_body_layout(body_blocks)
  file_state.layout = file_state.body_layout.view
  for hunk_index, _ in ipairs(display_hunks or {}) do
    local key = key_by_item[hunk_index]
    local span = file_state.layout.span_by_key[key]
    local block = file_state.body_layout.block_by_key[key]
    if span then
      span.metadata = block and block.metadata or span.metadata
    end
  end
  file_state.layout_dirty = false
  file_state.layout_revision = (file_state.layout_revision or 0) + 1
end

---@param file DiffReviewStatusFile
---@param entry_kind? string
---@param hunk_entry_kind? string
---@param file_key string
---@return DiffReviewDiffFileState?
function M._status_record_diff_file_header_state(file, entry_kind, hunk_entry_kind, file_key)
  local file_state = dr()._status_ensure_diff_file_state(file, entry_kind, hunk_entry_kind, file_key)
  if not file_state then return nil end
  file_state.hunks = {}
  file_state.hunk_index_by_id = {}
  file_state.annotations = {}
  file_state.annotation_index = dr()._diff_annotation_model.new_index()
  file_state.layout = nil
  file_state.layout_dirty = true
  dr()._status_populate_diff_file_hunks(file_state, file, file.hunks or {})
  dr()._status_populate_diff_file_annotations(file_state, file)
  return file_state
end

---@param file DiffReviewStatusFile
---@param hunks DiffReviewHunk[]
---@param display_hunks DiffReviewHunk[]
---@param entry_kind? string
---@param hunk_entry_kind? string
---@param file_key string
---@return DiffReviewDiffFileState?
function M._status_record_diff_file_state(file, hunks, display_hunks, entry_kind, hunk_entry_kind, file_key)
  local file_state = dr()._status_ensure_diff_file_state(file, entry_kind, hunk_entry_kind, file_key)
  if not file_state then return nil end
  file_state.hunks = {}
  file_state.hunk_index_by_id = {}
  file_state.annotations = {}
  file_state.annotation_index = dr()._diff_annotation_model.new_index()
  file_state.layout_dirty = true
  dr()._status_populate_diff_file_hunks(file_state, file, hunks)
  for hunk_index, hunk in ipairs(display_hunks or {}) do
    hunk.source_id = file_state.source_id
    hunk.file_key = file_state.key
    hunk.display_id = hunk.display_id or ("%s:display:%d"):format(file_state.key, hunk_index)
    hunk.diff_review_hunk_index = hunk.diff_review_hunk_index or dr()._diff_hunk_index_model.from_hunk(hunk)
    file_state.hunk_index_by_id = file_state.hunk_index_by_id or {}
    file_state.hunk_index_by_id[hunk.display_id] = hunk.diff_review_hunk_index
  end
  dr()._status_populate_diff_file_annotations(file_state, file)
  dr()._status_build_diff_file_layout(file_state, file, display_hunks)
  return file_state
end

return M
