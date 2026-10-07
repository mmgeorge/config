vim.loader.enable(false)

local ok, failure = pcall(function()
  local renderer = require("forge.render.harness.tool")
  local tool = { kind = "command", title = "cargo test --lib", status = "running", started_at_ms = 1000 }
  for _, sample in ipairs({ { 1500, "0s" }, { 3000, "2s" }, { 6500, "5s" } }) do
    local heading = renderer.heading_lines(tool, 80, "  ", sample[1])[1]
    assert(heading.text == "  • " .. sample[2] .. " cargo test --lib", heading.text)
    assert(heading.text:sub(heading.command_offset + 1) == "cargo test --lib")
    local chunks = renderer.foldtext_chunks(tool, "  ", heading.text, sample[1])
    local text = {}
    for _, chunk in ipairs(chunks) do text[#text + 1] = chunk[1] end
    assert(table.concat(text) == heading.text)
  end
  tool.status = "completed"
  tool.completed_at_ms = 3500
  assert(renderer.heading(tool, 90000) == "• 2s cargo test --lib")
  tool.started_at_ms = nil
  assert(renderer.heading(tool, 90000) == "• — cargo test --lib")
  tool.kind = "tool_call"
  tool.title = "sem_entities({})"
  tool.started_at_ms = 1000
  assert(renderer.heading_lines(tool, 80, "  ", 90000)[1].text == "  • 2s sem_entities({})")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("harness_tool_timing: passed")
  vim.cmd("qa!")
end
