local function get_position_before_postfix()
  local pos = vim.api.nvim_win_get_cursor(0) -- get current cursor position
  local row = pos[1] - 1 -- must convert to 0 indexed
  local line = vim.api.nvim_get_current_line()
  local dot_position = line:find("%.") 

  if dot_position then 
    return { row, dot_position - 2 } -- Before dot + 1 based index
  end

  return nil
end

local function type_node()
  local node = vim.treesitter.get_node({
    pos = get_position_before_postfix()
  })

  local function is_type(ty)
    if 
      ty == "primitive_type" or 
      ty == "scoped_type_identifier" or 
      ty == "generic_type" or 
      ty == "type_identifier"  then
      return true
    end 

    return false
  end

  while node do 
    local ty = node:type()

    local parent = node:parent(); 
    local parent_ty = parent and parent:type()

    if is_type(ty) and not is_type(parent_ty) then
      return node
    end

    node = parent
  end 

  return nil
end 

local function expr_node()
  local node = vim.treesitter.get_node({
    pos = get_position_before_postfix()
  })

  while node do 
    local ty = node:type()
    if 
      ty == "call_expression" or 
      ty == "integer_literal" or 
      ty == "string_literal" or 
      ty == "float_literal" then 
      return node
    end 

    node = node:parent()
  end 

  return nil
end

function postfix(options)
  return { 
    trigger = options.trigger, 
    execute = function ()
      local node = options.node()
      if node == nil then
        return nil
      end

      local row, col = node:start()
      local text = vim.treesitter.get_node_text(node, 0):gsub('%.%w*$', '')  -- remove postfix
      local pos = vim.api.nvim_win_get_cursor(0)
      return {
        body = options.body(text), 
        clear_region = {
          from = { row, col },
          to = { pos[1] - 1, pos[2] } 
        }
      } 
    end, 
  }  
end

local global_snippets = {
  -- { 
  -- trigger = 'fun', body = 'function ${1:name}(${2:args}) $0 end',
  -- }
}

local snippets_by_filetype = {
  rust = {
    postfix({ 
      trigger = 'rcl', 
      node = type_node, 
      body = function (text)
        return "Rc<RefCell<" .. text .. ">>" 
      end 
    }),
    postfix({ 
      trigger = 'rcl', 
      node = expr_node, 
      body = function (text)
        return "Rc::new(RefCell::new(" .. text .. "))" 
      end 
    }),
    postfix({ 
      trigger = 'rfl', 
      node = type_node, 
      body = function (text)
        return "RefCell<" .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'rfl', 
      node = expr_node, 
      body = function (text)
        return "RefCell::new(" .. text .. ")" 
      end 
    }),
    postfix({ 
      trigger = 'arc', 
      node = type_node, 
      body = function (text)
        return "Arc<" .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'rf', 
      node = type_node, 
      body = function (text)
        return "RefCell<" .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'rf', 
      node = expr_node, 
      body = function (text)
        return "RefCell::new(" .. text .. ")" 
      end 
    }),
    postfix({ 
      trigger = 'rl', 
      node = type_node, 
      body = function (text)
        return "RwLock<" .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'rl', 
      node = expr_node, 
      body = function (text)
        return "RwLock::new(" .. text .. ")" 
      end 
    }),
    postfix({ 
      trigger = 'mut', 
      node = type_node, 
      body = function (text)
        return "Mutex<" .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'mut', 
      node = expr_node, 
      body = function (text)
        return "Mutex::new(" .. text .. ")" 
      end 
    }),
    postfix({ 
      trigger = 'amut', 
      node = type_node, 
      body = function (text)
        return "Arc<Mutex<" .. text .. ">>" 
      end 
    }),
    postfix({ 
      trigger = 'amut', 
      node = expr_node, 
      body = function (text)
        return "Arc::new(Mutex::new(" .. text .. "))" 
      end 
    }),
    postfix({ 
      trigger = 'lboxfut', 
      node = type_node, 
      body = function (text)
        return "LocalBoxFuture<'static, " .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'boxfut', 
      node = type_node, 
      body = function (text)
        return "BoxFuture<'static, " .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'ifut', 
      node = type_node, 
      body = function (text)
        return "impl Future<Output = " .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'opt', 
      node = type_node, 
      body = function (text)
        return "Option<" .. text .. ">" 
      end 
    }),
    postfix({ 
      trigger = 'vec', 
      node = type_node, 
      body = function (text)
        return "Vec<" .. text .. ">" 
      end 
    }),
  }
}

local function get_buf_snips()
  local ft = vim.bo.filetype
  local snips = vim.list_slice(global_snippets)

  if ft and snippets_by_filetype[ft] then
    vim.list_extend(snips, snippets_by_filetype[ft])
  end

  return snips
end

local source = {}

---Return whether this source is available in the current context or not (optional).
---@return boolean
function source:is_available()
  return true
end

---Return the debug name of this source (optional).
---@return string
function source:get_debug_name()
  return 'debug name'
end

---Return LSP's PositionEncodingKind.
---@NOTE: If this method is omitted, the default value will be `utf-16`.
---@return lsp.PositionEncodingKind
function source:get_position_encoding_kind()
  return 'utf-16'
end

---Return the keyword pattern for triggering completion (optional).
---If this is omitted, nvim-cmp will use a default keyword pattern. See |cmp-config.completion.keyword_pattern|.
---@return string
function source:get_keyword_pattern()
  return [[\k\+]]
end

---Return trigger characters for triggering completion (optional).
function source:get_trigger_characters()
  return { '.' }
end

---Invoke completion (required).
---@param params cmp.SourceCompletionApiParams
---@param callback fun(response: lsp.CompletionResponse|nil)
function source:complete(params, callback)
  local bufnr = vim.api.nvim_get_current_buf()
  local completion_items = {}

  vim.tbl_map(function(s)
    local executed = s.execute();
    if executed ~= nil then 
      ---@type lsp.CompletionItem
      local item = {
        word = s.trigger,
        label = s.trigger,
        kind = vim.lsp.protocol.CompletionItemKind.Snippet,
        insertText = executed.body,
        insertTextFormat = vim.lsp.protocol.InsertTextFormat.Snippet,
        clear_region = executed.clear_region
      }
      table.insert(completion_items, item)
    end
  end, get_buf_snips())

  callback(completion_items)
end

---Resolve completion item (optional). This is called right before the completion is about to be displayed.
---Useful for setting the text shown in the documentation window (`completion_item.documentation`).
---@param completion_item lsp.CompletionItem
---@param callback fun(completion_item: lsp.CompletionItem|nil)
function source:resolve(completion_item, callback)
  callback(completion_item)
end

---Executed after the item was selected.
---@param completion_item lsp.CompletionItem
---@param callback fun(completion_item: lsp.CompletionItem|nil)
function source:execute(completion_item, callback)
  local clear = completion_item.clear_region 
  if clear ~= nil then
    local buf = vim.api.nvim_get_current_buf() 

    vim.api.nvim_buf_set_text(buf, clear.from[1], clear.from[2], clear.to[1], clear.to[2], {})
  end 

  callback(completion_item)
end

return source
---Register your source to nvim-cmp.
-- require('cmp').register_source('snp', source)

