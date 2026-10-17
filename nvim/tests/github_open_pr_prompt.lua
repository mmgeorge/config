vim.loader.enable(false)

local open_pr = require("github.open_pr")
local compact = "Changed files:\n  moved.rs (Renamed from original.rs)\n  source.rs (+1 -1)\n\nsource.rs (+1 -1)\n\n  fn calculate() {\n- old();\n+ new();\n"
local head = string.rep("1", 40)
local context = open_pr._build_context_for_test("feature/change", "refs/remotes/origin/develop", head, compact)
assert(context:find("Base branch: develop", 1, true))
assert(context:find("Head commit: " .. head, 1, true))
assert(context:find(compact, 1, true), "native compact context was rewritten")
assert(not context:find("conventional commit", 1, true), "PR context inherited commit instructions")
assert(not open_pr._pr_system_prompt_for_test:find("Conventional Commit", 1, true))
print("github_open_pr_prompt: native context and PR instruction isolation passed")
vim.cmd("qa!")
