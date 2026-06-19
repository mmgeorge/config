vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local root = vim.fs.normalize(original_cwd .. "/.diffreview-virtual-hunk-merge-test")
local calls = {}

local source_lines = {}
for line_number = 1, 100 do
  source_lines[line_number] = ("// filler %d"):format(line_number)
end
source_lines[1] = "mod gpu { pub struct Context; }"
source_lines[2] = "mod shaders { pub mod model { pub mod render { pub struct BindGroup1Descriptor; } pub mod particle_render { pub struct BindGroup1Descriptor; } } }"
source_lines[3] = "struct ModelStore {"
source_lines[4] = "  primitives: (),"
source_lines[5] = "  materials: (),"
source_lines[6] = "  bind_group: (),"
source_lines[7] = "}"
source_lines[8] = "impl ModelStore {"
source_lines[70] = "  pub fn new(context: &gpu::Context) -> Self {"
source_lines[71] = "    let primitives = ();"
source_lines[72] = "    let materials = ();"
source_lines[73] = "    let bind_group = ();"
source_lines[74] = "    let particle_bind_group ="
source_lines[75] = "      context.create_bind_group(shaders::model::particle_render::BindGroup1Descriptor {"
source_lines[76] = "        color_texture: color,"
source_lines[77] = "        color_sampler: color_sampler.binding(),"
source_lines[78] = "        normal_texture: normal,"
source_lines[79] = "        roughness_metallic_texture: metallic_roughness,"
source_lines[80] = "        primitive_material: materials.materials.binding(),"
source_lines[81] = "        primitive_transform: primitive_transforms.binding(),"
source_lines[82] = "      });"
source_lines[83] = ""
source_lines[84] = "    // TODO: Initial size is not correct, primitive_ge.tangents is larger"
source_lines[85] = "    Self {"
source_lines[86] = "      primitives,"
source_lines[87] = "      materials,"
source_lines[88] = "      bind_group,"
source_lines[89] = "      particle_bind_group,"
source_lines[90] = "    }"
source_lines[91] = "  }"
source_lines[92] = ""
source_lines[93] = "  pub fn insert(&mut self, model: &Model, repository: &ModelRepository) {"
source_lines[94] = "    for mesh in model.mesh_iter() {}"
source_lines[95] = "  }"
source_lines[96] = "}"

local diff_text = table.concat({
  "diff --git a/src/model_store.rs b/src/model_store.rs",
  "index 1111111..2222222 100644",
  "--- a/src/model_store.rs",
  "+++ b/src/model_store.rs",
  "@@ -73,0 +74,9 @@",
  "+    let particle_bind_group =",
  "+      context.create_bind_group(shaders::model::particle_render::BindGroup1Descriptor {",
  "+        color_texture: color,",
  "+        color_sampler: color_sampler.binding(),",
  "+        normal_texture: normal,",
  "+        roughness_metallic_texture: metallic_roughness,",
  "+        primitive_material: materials.materials.binding(),",
  "+        primitive_transform: primitive_transforms.binding(),",
  "+      });",
  "@@ -88,0 +89 @@",
  "+      particle_bind_group,",
  "@@ -74,9 +93,0 @@",
  "-    let particle_bind_group =",
  "-      context.create_bind_group(shaders::model::particle_render::BindGroup1Descriptor {",
  "-        color_texture: color,",
  "-        color_sampler: color_sampler.binding(),",
  "-        normal_texture: normal,",
  "-        roughness_metallic_texture: metallic_roughness,",
  "-        primitive_material: materials.materials.binding(),",
  "-        primitive_transform: primitive_transforms.binding(),",
  "-      });",
}, "\n")

---@type DiffReviewGitBackend
local backend = {}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

function backend.systemlist(command)
  local key = command_key(command)
  calls[#calls + 1] = key
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "virtual hunk merge" }, 0 end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return { "M\tsrc/model_store.rs" }, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return vim.split(diff_text, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t--no-color\t--format=%H%x09%h%x09%cI%x09%s\t-20" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tshow\t:0:src/model_store.rs" then return source_lines, 0 end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  local output, code = backend.systemlist(command)
  cb(output, code, "")
end

function backend.system()
  return "", 0
end

function backend.system_async(_, _, cb)
  cb({ code = 0, stdout = "", stderr = "", output = "" })
end

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local function buffer_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function buffer_dump(buf)
  return table.concat(buffer_lines(buf), "\n")
end

local function buffer_contains(buf, pattern)
  for _, line in ipairs(buffer_lines(buf)) do
    if line:find(pattern, 1, true) then return true end
  end
  return false
end

local function find_row(buf, pattern)
  for index, line in ipairs(buffer_lines(buf)) do
    if line:find(pattern, 1, true) then return index end
  end
  error("missing row: " .. pattern .. "\n" .. buffer_dump(buf), 2)
end

local function count_rows(buf, pattern)
  local count = 0
  for _, line in ipairs(buffer_lines(buf)) do
    if line:find(pattern, 1, true) then count = count + 1 end
  end
  return count
end

local function gutter_text(buf, row)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.virt_text_pos == "inline" and details.virt_text then
      local parts = {}
      for _, chunk in ipairs(details.virt_text) do
        parts[#parts + 1] = chunk[1]
      end
      return table.concat(parts)
    end
  end
  return ""
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function run()
  vim.fn.delete(root, "rf")
  assert_true(vim.fn.mkdir(root .. "/src", "p") == 1, "mkdir failed")
  assert_true(vim.fn.writefile(source_lines, root .. "/src/model_store.rs") == 0, "writefile failed")

  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })
  diff_review.open()

  local buf = vim.api.nvim_get_current_buf()
  wait_for(function()
    return buffer_contains(buf, "model_store.rs +10 -9")
  end, "status did not render file\n" .. buffer_dump(buf)
    .. "\n\ncalls:\n" .. table.concat(calls, "\n")
    .. "\n\nmessages:\n" .. vim.fn.execute("messages"))

  trigger_normal_mapping("<Tab>", find_row(buf, "model_store.rs +10 -9"))
  wait_for(function()
    return buffer_contains(buf, "particle_bind_group,")
  end, "expanded hunk did not render\n" .. buffer_dump(buf))

  assert_true(count_rows(buf, "@@ ") == 1, "adjacent virtual hunks should render under one header\n" .. buffer_dump(buf))
  assert_true(count_rows(buf, "Self {") == 1, "semantic bridge context should not be duplicated\n" .. buffer_dump(buf))
  local self_row = find_row(buf, "Self {")
  local self_gutter = gutter_text(buf, self_row)
  assert_true(self_gutter:find("^%s*76%s+85%s+$", 1) ~= nil, "semantic context gutter should map old/new line numbers: " .. self_gutter)
  local insert_old_line = diff_review._hunk_old_line_for_new_line({
    { old_start = 73, old_count = 0, new_start = 74, new_count = 9 },
    { old_start = 88, old_count = 0, new_start = 89, new_count = 1 },
    { old_start = 74, old_count = 9, new_start = 93, new_count = 0 },
  }, 93)
  assert_true(
    insert_old_line == 73,
    "pure deletion parent context should use old_start - 1 with current new line, got " .. tostring(insert_old_line)
  )
  assert_true(
    diff_review._hunk_render_coords_adjacent(73, 93, 74, nil),
    "boundary parent and first deletion row should be adjacent on the old side"
  )
  local for_mesh_old_line = diff_review._hunk_old_line_for_new_line({
    { old_start = 73, old_count = 0, new_start = 74, new_count = 9 },
    { old_start = 88, old_count = 0, new_start = 89, new_count = 1 },
    { old_start = 74, old_count = 9, new_start = 93, new_count = 0 },
  }, 94)
  local enumerate_old_line = diff_review._hunk_old_line_for_new_line({
    { old_start = 73, old_count = 0, new_start = 74, new_count = 9 },
    { old_start = 88, old_count = 0, new_start = 89, new_count = 1 },
    { old_start = 74, old_count = 9, new_start = 93, new_count = 0 },
  }, 95)
  local material_old_line = diff_review._hunk_old_line_for_new_line({
    { old_start = 73, old_count = 0, new_start = 74, new_count = 9 },
    { old_start = 88, old_count = 0, new_start = 89, new_count = 1 },
    { old_start = 74, old_count = 9, new_start = 93, new_count = 0 },
  }, 96)
  assert_true(for_mesh_old_line == 83, "post-deletion context line 94 should map to old line 83, got " .. tostring(for_mesh_old_line))
  assert_true(enumerate_old_line == 84, "post-deletion context line 95 should map to old line 84, got " .. tostring(enumerate_old_line))
  assert_true(material_old_line == 85, "post-deletion context line 96 should map to old line 85, got " .. tostring(material_old_line))
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
vim.fn.delete(root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
