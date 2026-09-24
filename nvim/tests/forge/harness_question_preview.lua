vim.loader.enable(false)
local renderer = require("forge.render.harness.interaction_tree")
local question = { id = "set", questions = {
  { id = "scope", header = "Scope", question = "Which scope?", options = { { label = "Rust", description = "CLI" } } },
} }
local exchange = {
  id = "exchange", ordinal = 1, state = "running", prompt = "Plan it", created_at_ms = 0,
  turn = { { id = "turn", state = "completed", message = {
    { id = "commentary", kind = "assistant", delivery = "commentary", text = "Checking the scope" },
    { id = "answer", kind = "assistant", delivery = "final", text = "I mean a Rust CLI" },
  }, tool = { order = { "ask" }, item = { ask = {
    id = "ask", title = "harness_question_ask", output = "Question set accepted", status = "completed", failed = false,
  } } } } },
  node_list = {
    { kind = "turn_content", id = "commentary-node", turn_id = "turn", item = { kind = "message", id = "commentary" } },
    { kind = "turn_content", id = "tool-node", turn_id = "turn", item = { kind = "tool", id = "ask" } },
    { kind = "question_presented", id = "question", question = question },
    { kind = "exchange_input", prompt = { id = "ask-input", intent = "clarification", text = "What do you mean?" } },
    { kind = "turn_content", id = "answer-node", turn_id = "turn", item = { kind = "message", id = "answer" } },
    { kind = "exchange_input", prompt = { id = "answer-input", intent = "answer", text = "Planning feedback:\n- Scope: Rust" } },
  },
}
for _, plan in ipairs({ false, true }) do
  if plan then
    exchange.node_list[3] = { kind = "plan_event", event = {
      id = "question", content = { kind = "lifecycle", lifecycle = { kind = "question_asked", question = question } },
    } }
  end
  local rendered = renderer.build({ { kind = "exchange", exchange = exchange } }, { content_width = 100 })
  local text = table.concat(rendered.lines, "\n")
  assert(text:find("Question presented: Scope", 1, true), text)
  assert(text:find("○ You asked: What do you mean?", 1, true), text)
  assert(text:find("○ You answered: Scope: Rust", 1, true), text)
  assert(text:find("↳ Checking the scope", 1, true), text)
  assert(not text:find("↳ I mean", 1, true), text)
  assert(not text:find("Question set accepted", 1, true), text)
end
exchange.state = "cancelled"
local collapsed = renderer.build({ { kind = "exchange", exchange = exchange } }, { content_width = 100, expanded = {} })
assert(not table.concat(collapsed.lines, "\n"):find("I mean a Rust CLI", 1, true),
  "collapsed preview exposed an earlier clarification without its user input")
local expanded = renderer.build({ { kind = "exchange", exchange = exchange } }, {
  content_width = 100, expanded = { ["exchange:exchange"] = true },
})
local text = table.concat(expanded.lines, "\n")
assert(text:find("I mean a Rust CLI", 1, true) < text:find("You answered:", 1, true), text)
print("harness_question_preview: passed")
