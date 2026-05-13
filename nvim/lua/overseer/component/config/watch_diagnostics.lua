local util = require("overseer.util")

local type_to_severity = {
  e = vim.diagnostic.severity.ERROR,
  E = vim.diagnostic.severity.ERROR,
  w = vim.diagnostic.severity.WARN,
  W = vim.diagnostic.severity.WARN,
  n = vim.diagnostic.severity.INFO,
  N = vim.diagnostic.severity.INFO,
  i = vim.diagnostic.severity.INFO,
  I = vim.diagnostic.severity.INFO,
}

local ts_lsp_names = {
  tsgo = true,
  tsserver = true,
  vtsls = true,
  ["typescript-tools"] = true,
}

local function has_diagnostics(result)
  return result.diagnostics and not vim.tbl_isempty(result.diagnostics)
end

local function normalize_filename(filename)
  local absolute = vim.fn.fnamemodify(filename, ":p")
  local realpath = (vim.uv or vim.loop).fs_realpath(absolute)
  return vim.fs.normalize(realpath or absolute)
end

local function filename_for_buf(bufnr)
  if not bufnr or not vim.api.nvim_buf_is_valid(bufnr) then
    return nil
  end

  local filename = vim.api.nvim_buf_get_name(bufnr)
  if filename == "" then
    return nil
  end

  return normalize_filename(filename)
end

local function diagnostic_col(col)
  if not col or col <= 0 then
    return 0
  end

  return col - 1
end

local function has_ts_lsp(bufnr)
  if not vim.api.nvim_buf_is_loaded(bufnr) then
    return false
  end

  local clients = vim.lsp.get_clients and vim.lsp.get_clients({ bufnr = bufnr })
      or vim.lsp.get_active_clients({ bufnr = bufnr })
  for _, client in ipairs(clients) do
    if ts_lsp_names[client.name] then
      return true
    end
  end

  return false
end

local function diagnostics_match(left, right)
  return left.lnum == right.lnum
      and left.col == right.col
      and left.message == right.message
      and left.severity == right.severity
      and tostring(left.code or "") == tostring(right.code or "")
end

local function diagnostics_equal(left, right)
  if #left ~= #right then
    return false
  end

  for i, diagnostic in ipairs(left) do
    local other = right[i]
    if not diagnostics_match(diagnostic, other)
        or diagnostic.end_lnum ~= other.end_lnum
        or diagnostic.end_col ~= other.end_col then
      return false
    end
  end

  return true
end

local function has_equivalent_external_diagnostic(self, bufnr, diagnostic)
  if not vim.api.nvim_buf_is_loaded(bufnr) then
    return false
  end

  for namespace in pairs(vim.diagnostic.get_namespaces()) do
    if namespace ~= self.ns then
      for _, other in ipairs(vim.diagnostic.get(bufnr, { namespace = namespace })) do
        if diagnostics_match(diagnostic, other) then
          return true
        end
      end
    end
  end

  return false
end

local function to_diagnostic(task, item)
  local lnum_idx = (item.lnum or 1) - 1
  local end_lnum_idx
  if item.end_lnum and item.end_lnum > 0 then
    end_lnum_idx = item.end_lnum - 1
  else
    end_lnum_idx = lnum_idx + 1
  end

  return {
    message = item.text,
    severity = type_to_severity[item.type] or vim.diagnostic.severity.ERROR,
    lnum = lnum_idx,
    end_lnum = end_lnum_idx,
    col = diagnostic_col(item.col),
    end_col = item.end_col and diagnostic_col(item.end_col) or nil,
    source = task.name,
    code = item.code,
  }
end

---@type overseer.ComponentFileDefinition
return {
  desc = "Display watch diagnostics without clearing until the next complete result",
  params = {
    virtual_text = {
      desc = "Override the default diagnostics.virtual_text setting",
      type = "boolean",
      optional = true,
    },
    signs = {
      desc = "Override the default diagnostics.signs setting",
      type = "boolean",
      optional = true,
    },
    underline = {
      desc = "Override the default diagnostics.underline setting",
      type = "boolean",
      optional = true,
    },
  },
  constructor = function(params)
    local function reset_buf(self, bufnr)
      if vim.tbl_isempty(vim.diagnostic.get(bufnr, { namespace = self.ns })) then
        self.bufnrs[bufnr] = nil
        return
      end

      self.updating = true
      vim.diagnostic.reset(self.ns, bufnr)
      self.updating = false
      self.bufnrs[bufnr] = nil
    end

    local function apply_diagnostics(self, bufnr, diagnostics)
      local current = vim.diagnostic.get(bufnr, { namespace = self.ns })
      if diagnostics_equal(current, diagnostics) then
        if vim.tbl_isempty(diagnostics) then
          self.bufnrs[bufnr] = nil
        else
          self.bufnrs[bufnr] = true
        end
        return
      end

      if vim.tbl_isempty(diagnostics) then
        reset_buf(self, bufnr)
        return
      end

      self.updating = true
      vim.diagnostic.set(self.ns, bufnr, diagnostics, {
        virtual_text = params.virtual_text,
        signs = params.signs,
        underline = params.underline,
      })
      self.updating = false
      self.bufnrs[bufnr] = true
    end

    local function remove_diagnostics(self)
      for bufnr in pairs(self.bufnrs) do
        reset_buf(self, bufnr)
      end
      self.bufnrs = {}
    end

    local function refresh_file(self, task, filename)
      local items = self.items_by_filename[filename]
      if not items then
        local bufnr = vim.fn.bufnr(filename)
        if bufnr > 0 then
          reset_buf(self, bufnr)
        end
        return
      end

      local bufnr = vim.fn.bufadd(filename)
      local diagnostics = {}
      local suppress_for_lsp = has_ts_lsp(bufnr)

      for _, item in ipairs(items) do
        local diagnostic = to_diagnostic(task, item)
        if not suppress_for_lsp and not has_equivalent_external_diagnostic(self, bufnr, diagnostic) then
          table.insert(diagnostics, diagnostic)
        end
      end

      apply_diagnostics(self, bufnr, diagnostics)
      if not vim.api.nvim_buf_is_loaded(bufnr) then
        util.set_bufenter_callback(bufnr, "diagnostics_show", function()
          vim.diagnostic.show(self.ns, bufnr)
        end)
      end
    end

    local function set_diagnostics(self, task, result)
      local grouped = {}
      for _, diag in ipairs(result.diagnostics) do
        if not diag.filename and diag.bufnr and diag.bufnr ~= 0 then
          diag.filename = vim.api.nvim_buf_get_name(diag.bufnr)
        end

        if diag.filename then
          diag.filename = normalize_filename(diag.filename)
          grouped[diag.filename] = grouped[diag.filename] or {}
          table.insert(grouped[diag.filename], diag)
        end
      end

      remove_diagnostics(self)
      self.items_by_filename = grouped

      for filename in pairs(grouped) do
        refresh_file(self, task, filename)
      end
    end

    return {
      bufnrs = {},
      items_by_filename = {},
      pending_empty_result = false,
      updating = false,
      disposed = false,
      on_init = function(self, task)
        self.ns = vim.api.nvim_create_namespace(task.name)
        self.group = vim.api.nvim_create_augroup("overseer_watch_diagnostics_" .. self.ns, { clear = true })
        vim.api.nvim_create_autocmd({ "BufEnter", "DiagnosticChanged", "LspAttach", "LspDetach" }, {
          group = self.group,
          callback = function(event)
            if self.disposed or self.updating then
              return
            end

            local filename = filename_for_buf(event.buf)
            if not filename or not self.items_by_filename[filename] then
              return
            end

            vim.schedule(function()
              if self.disposed or self.updating then
                return
              end

              refresh_file(self, task, filename)
            end)
          end,
        })
      end,
      on_result = function(self, task, result)
        if has_diagnostics(result) then
          self.pending_empty_result = false
          set_diagnostics(self, task, result)
          return
        end

        if not vim.tbl_isempty(self.items_by_filename) and not self.pending_empty_result then
          self.pending_empty_result = true
          return
        end

        self.pending_empty_result = false
        self.items_by_filename = {}
        remove_diagnostics(self)
      end,
      on_dispose = function(self, task)
        self.disposed = true
        if self.group then
          vim.api.nvim_del_augroup_by_id(self.group)
        end
        remove_diagnostics(self)
      end,
    }
  end,
}
