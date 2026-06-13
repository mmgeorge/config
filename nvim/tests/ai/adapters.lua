vim.loader.enable(false)

local original_getenv = os.getenv
package.loaded["ai.adapters"] = nil

local function assert_eq(actual, expected, message)
  if actual ~= expected then
    error(("%s: expected %s, got %s"):format(message, tostring(expected), tostring(actual)), 2)
  end
end

local function load_with_gemini_key(value)
  os.getenv = function(name)
    if name == "GEMINI_API_KEY" then return value end
    return original_getenv(name)
  end
  package.loaded["ai.adapters"] = nil
  return require("ai.adapters").get()
end

local ok, err = xpcall(function()
  local gemini = load_with_gemini_key("test-key")
  assert_eq(gemini.commit, "gemini3,thinking=minimal", "gemini commit default")
  assert_eq(gemini.pr_create, "gemini3,thinking=minimal", "gemini PR create default")

  local fallback = load_with_gemini_key(nil)
  assert_eq(fallback.commit, "copilot,model=gpt-4.1", "fallback commit default")
  assert_eq(fallback.pr_create, "copilot,model=gpt-4.1", "fallback PR create default")
end, debug.traceback)

os.getenv = original_getenv
package.loaded["ai.adapters"] = nil

if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end

print("ai_adapters: ok")
vim.cmd("qa!")
