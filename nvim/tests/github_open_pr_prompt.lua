vim.loader.enable(false)

local open_pr = require("github.open_pr")

local function assert_contains(text, expected, message)
  if not text:find(expected, 1, true) then
    error(message .. ": " .. expected, 2)
  end
end

local system_prompt = open_pr._pr_system_prompt_for_test
assert_contains(
  system_prompt,
  "Lead with the feature, fix, or capability and its reviewer-visible outcome.",
  "system prompt should lead with the outcome"
)
assert_contains(
  system_prompt,
  "Describe the operative mechanism through accurate ownership, data flow, contracts, or lifecycle boundaries.",
  "system prompt should require concrete mechanisms"
)
assert_contains(
  system_prompt,
  "State the concrete consequence that follows from that mechanism",
  "system prompt should require the warrant or consequence"
)
assert_contains(
  system_prompt,
  "Use a before/now contrast only when the diff proves a change to existing behavior.",
  "system prompt should avoid unsupported comparisons"
)
assert_contains(
  system_prompt,
  "Do not invent motivations, guarantees, architectural terms, or user benefits.",
  "system prompt should keep claims grounded"
)

local context = open_pr._build_context_for_test(
  "feature/copilot-cli",
  "origin/main",
  " nvim/lua/ai/adapters.lua | 2 +-\n 1 file changed, 1 insertion(+), 1 deletion(-)",
  table.concat({
    "diff --git a/nvim/lua/ai/adapters.lua b/nvim/lua/ai/adapters.lua",
    "--- a/nvim/lua/ai/adapters.lua",
    "+++ b/nvim/lua/ai/adapters.lua",
    "@@ -1 +1 @@",
    "-commit = \"copilot\"",
    "+commit = \"copilot-cli\"",
  }, "\n")
)
assert_contains(
  context,
  "Write the draft PR description from the repository evidence below.",
  "context should state the writing task"
)
assert_contains(context, "Base branch: main", "context should identify the base branch")
assert_contains(context, "Compact diff context:", "context should include diff evidence")
assert_contains(
  context,
  "Synthesize the complete change into one coherent operating story",
  "context should request synthesis instead of enumeration"
)

print("github_open_pr_prompt: ok")
vim.cmd("qa!")
