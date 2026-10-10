vim.loader.enable(false)

local ok, failure = pcall(function()
  local renderer = require("forge.render.harness.tool")
  local tool = { kind = "command", title = "cargo test --lib", status = "running", started_at_ms = 1000 }
  for _, sample in ipairs({ { 1000, "0s" }, { 1002, "0s" }, { 1049, "0s" }, { 1050, "0.1s" },
    { 1149, "0.1s" }, { 1150, "0.2s" }, { 1439, "0.4s" }, { 1950, "1s" },
    { 1999, "1s" }, { 2000, "1s" }, { 2050, "1.1s" }, { 3000, "2s" }, { 6500, "5.5s" },
    { 100949, "99.9s" }, { 100950, "1.7m" }, { 101050, "1.7m" }, { 1001050, "16.7m" } }) do
    local heading = renderer.heading_lines(tool, 80, "  ", sample[1])[1]
    assert(heading.text == ("  • %5s cargo test --lib"):format(sample[2]), heading.text)
    assert(vim.fn.strdisplaywidth(heading.text:sub(1, heading.command_offset)) == 10)
    assert(heading.text:sub(heading.command_offset + 1) == "cargo test --lib")
    local chunks = renderer.foldtext_chunks(tool, "  ", heading.text, sample[1])
    local text = {}
    for _, chunk in ipairs(chunks) do text[#text + 1] = chunk[1] end
    assert(table.concat(text) == heading.text)
  end
  tool.status = "completed"
  tool.completed_at_ms = 3500
  assert(renderer.heading(tool, 90000) == "•  2.5s cargo test --lib")
  tool.started_at_ms = nil
  assert(renderer.heading(tool, 90000) == "•     — cargo test --lib")
  tool.kind = "tool_call"
  tool.title = "sem_entities({})"
  tool.started_at_ms = 1000
  assert(renderer.heading_lines(tool, 80, "  ", 90000)[1].text == "  •  2.5s sem_entities({})")
  for _, kind in ipairs({ "command", "tool_call" }) do
    tool.kind = kind
    tool.title = "inspect " .. string.rep("界 very long argument ", 20) .. "\nnext command"
    for _, width in ipairs({ 12, 40, 90 }) do
      local rows = renderer.heading_lines(tool, width, "  ", 90000)
      assert(#rows == 1)
      assert(not rows[1].text:find("\n", 1, true))
      assert(vim.fn.strdisplaywidth(rows[1].text) <= width)
    end
  end

end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("harness_tool_timing: passed")
  vim.cmd("qa!")
end
