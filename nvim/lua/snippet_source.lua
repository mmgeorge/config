local utils = require("utils")
local type_node = utils.type_node
local expr_node = utils.expr_node
local postfix = utils.postfix

local global_snippets = {}
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

local function get_snippets()
  local ft = vim.bo.filetype
  local snips = vim.list_slice(global_snippets)

  if ft and snippets_by_filetype[ft] then
    vim.list_extend(snips, snippets_by_filetype[ft])
  end

  return snips
end

--- @module 'blink.cmp'
--- @class blink.cmp.Source
local source = {}

-- `opts` table comes from `sources.providers.your_provider.opts`
-- You may also accept a second argument `config`, to get the full
-- `sources.providers.your_provider` table
function source.new(opts)
  -- vim.validate('your_source.opts.some_option', opts.some_option, { 'string' })
  -- vim.validate('your_source.opts.optional_option', opts.optional_option, { 'string' }, true)

  local self = setmetatable({}, { __index = source })
  self.opts = opts
  return self
end


-- (Optional) Enable the source in specific contexts only
function source:enabled() return true end

-- (Optional) Non-alphanumeric characters that trigger the source
function source:get_trigger_characters() return { '.' } end

function source:get_completions(ctx, callback)
  local bufnr = vim.api.nvim_get_current_buf()
  local items = {}
  local insert_item = function(snippet)
    local executed = snippet.execute();
    if executed ~= nil then 
      ---@type lsp.CompletionItem
      local item = {
        word = snippet.trigger,
        label = snippet.trigger,
        kind = vim.lsp.protocol.CompletionItemKind.Snippet,
        insertText = executed.body,
        insertTextFormat = vim.lsp.protocol.InsertTextFormat.Snippet,
        clear_region = executed.clear_region
      }
      table.insert(items, item)
    end
  end 

  vim.tbl_map(insert_item, get_snippets())

  callback({ 
    items = items, 
    -- Whether blink.cmp should request items when deleting characters
    -- from the keyword (i.e. "foo|" -> "fo|")
    -- Note that any non-alphanumeric characters will always request
    -- new items (excluding `-` and `_`)
    is_incomplete_backward = false,
    -- Whether blink.cmp should request items when adding characters
    -- to the keyword (i.e. "fo|" -> "foo|")
    -- Note that any non-alphanumeric characters will always request
    -- new items (excluding `-` and `_`)
    is_incomplete_forward = false,
  })
end

-- Called immediately after applying the item's textEdit/insertText
function source:execute(ctx, item, callback, default_implementation)
  local clear = item.clear_region 
  if clear ~= nil then
    local buf = vim.api.nvim_get_current_buf() 

    vim.api.nvim_buf_set_text(buf, clear.from[1], clear.from[2], clear.to[1], clear.to[2], {})
  end 

  default_implementation()
  callback()
end

return source
