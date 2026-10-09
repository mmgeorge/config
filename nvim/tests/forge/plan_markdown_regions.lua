vim.loader.enable(false)

local markdown = require("forge.views.plan_review.markdown")
local source = {
  { text = "Decisions:", metadata = {} },
  { text = "- **Use fixed coins.**", metadata = { markdown = true } },
  { text = "  Preserve deterministic tests.", metadata = { markdown = true } },
  { text = "- **Keep presentation optional.**", metadata = { markdown = true } },
  { text = "Changes:", metadata = {} },
  { text = "/// **Literal declaration documentation**", metadata = {} },
  { text = "Manual verification:", metadata = {} },
  { text = "- Check `restart`.", metadata = { markdown = true } },
  { text = "Automated verification:", metadata = {} },
  { text = "    printf '**literal**'", metadata = {} },
}
local records = {}
for index = 1, #source do records[index] = { source_index = index, row = index - 1 } end
assert(vim.deep_equal(markdown.ranges(source, { source_record_list = records }), {
  { first0 = 1, after0 = 4 }, { first0 = 7, after0 = 8 },
}), "Only section bodies may render Markdown")

for index = 4, #records do records[index].row = records[index].row + 3 end
assert(vim.deep_equal(markdown.ranges(source, { source_record_list = records }), {
  { first0 = 1, after0 = 3 }, { first0 = 6, after0 = 7 }, { first0 = 10, after0 = 11 },
}), "Inserted review comments must split and shift Markdown regions")

io.write("plan_markdown_regions OK\n")
vim.cmd("qa!")
