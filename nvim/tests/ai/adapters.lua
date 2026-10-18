vim.loader.enable(false)

local original_getenv = os.getenv
package.loaded["ai.adapters"] = nil

local function assert_eq(actual, expected, message)
  if actual ~= expected then
    error(("%s: expected %s, got %s"):format(message, tostring(expected), tostring(actual)), 2)
  end
end

local function load_with_gemini_key(value, enabled)
  os.getenv = function(name)
    if name == "GEMINI_API_KEY" then return value end
    return original_getenv(name)
  end
  package.loaded["ai.adapters"] = nil
  local adapters = require("ai.adapters")
  if enabled ~= nil then adapters.use_gemini_for_commits = enabled end
  return adapters.get()
end

local ok, err = xpcall(function()
  local gemini = load_with_gemini_key("test-key")
  assert_eq(gemini.commit, "harness", "Gemini commit generation must default to disabled")
  assert_eq(gemini.pr_create, "gemini3,thinking=low", "gemini PR create default")
  assert_eq(gemini.chat.name, "gemini", "commit flag changed the chat adapter")

  assert_eq(load_with_gemini_key("test-key", true).commit,
    "gemini3-lite,thinking=minimal", "enabled Gemini commit adapter")
  assert_eq(load_with_gemini_key("test-key", false).commit,
    "harness", "disabled Gemini commit adapter")
  assert_eq(load_with_gemini_key(nil, true).commit,
    "harness", "Gemini commit flag bypassed missing credentials")

  local fallback = load_with_gemini_key(nil)
  assert_eq(fallback.commit, "harness", "fallback commit default")
  assert_eq(fallback.pr_create, "copilot-cli,model=gpt-5.6-luna,thinking=low", "fallback PR create default")
end, debug.traceback)

os.getenv = original_getenv
package.loaded["ai.adapters"] = nil

if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end

print("ai_adapters: ok")
vim.cmd("qa!")
