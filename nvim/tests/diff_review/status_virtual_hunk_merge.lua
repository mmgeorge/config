vim.loader.enable(false)

local diff_review = require("diff_review")
local hunk_model = require("diff_review.render.hunk_model")
local ui = require("diff_review.infra.ui")
local gh = require("diff_review.integrations.gh")

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
source_lines[8] = "// pre-impl filler"
source_lines[69] = "impl ModelStore {"
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

local repeated_context_lines = {}
for line_number = 1, 100 do
  repeated_context_lines[line_number] = ("// repeated filler %d"):format(line_number)
end
repeated_context_lines[1] = "struct RepeatedContext;"
repeated_context_lines[2] = "impl RepeatedContext {"
repeated_context_lines[30] = "  pub fn build_repeated_context(context: &gpu::Context) -> Self {"
repeated_context_lines[66] = "    let bind_group = context.create_bind_group(shaders::model::render::BindGroup1Descriptor {"
repeated_context_lines[67] = "      color_texture: color.clone(),"
repeated_context_lines[68] = "      color_sampler: color_sampler.binding(),"
repeated_context_lines[69] = "      normal_texture: normal.clone(),"
repeated_context_lines[70] = "      roughness_metallic_texture: metallic_roughness.clone(),"
repeated_context_lines[71] = "      primitive_material: materials.materials.binding(),"
repeated_context_lines[72] = "      primitive_transform: primitive_transforms.binding(),"
repeated_context_lines[73] = "    });"
repeated_context_lines[84] = "    // TODO: Initial size is not correct, primitive_ge.tangents is larger"
repeated_context_lines[85] = "    let output = Self {"
repeated_context_lines[86] = "      primitives,"
repeated_context_lines[87] = "      materials,"
repeated_context_lines[88] = "      bind_group,"
repeated_context_lines[89] = "      particle_bind_group,"
repeated_context_lines[90] = "    };"
repeated_context_lines[91] = "    // repeated spacer 91"
repeated_context_lines[92] = "    // repeated spacer 92"
repeated_context_lines[93] = "    // repeated spacer 93"
repeated_context_lines[94] = "    // repeated spacer 94"
repeated_context_lines[95] = "    // repeated spacer 95"
repeated_context_lines[96] = "    // repeated spacer 96"
repeated_context_lines[97] = "    let tail_marker = ();"
repeated_context_lines[98] = "    output"
repeated_context_lines[99] = "  }"
repeated_context_lines[100] = "}"

local compact_neighbor_lines = {}
for line_number = 1, 120 do
  compact_neighbor_lines[line_number] = ("// compact filler %d"):format(line_number)
end
compact_neighbor_lines[1] = "mod gpu { pub struct Context; }"
compact_neighbor_lines[2] = "mod shaders { pub mod model { pub mod render { pub struct BindGroup1Descriptor; } pub mod particle_render { pub struct BindGroup1Descriptor; } } }"
compact_neighbor_lines[8] = "impl ModelStore {"
compact_neighbor_lines[30] = "  pub fn compact_neighbor(context: &gpu::Context) -> Self {"
compact_neighbor_lines[66] = "    let bind_group = context.create_bind_group(shaders::model::render::BindGroup1Descriptor {"
compact_neighbor_lines[67] = "      color_texture: color.clone(),"
compact_neighbor_lines[68] = "      color_sampler: color_sampler.binding(),"
compact_neighbor_lines[69] = "      normal_texture: normal.clone(),"
compact_neighbor_lines[70] = "      roughness_metallic_texture: metallic_roughness.clone(),"
compact_neighbor_lines[71] = "      primitive_material: materials.materials.binding(),"
compact_neighbor_lines[72] = "      primitive_transform: primitive_transforms.binding(),"
compact_neighbor_lines[73] = "    });"
compact_neighbor_lines[74] = "    let particle_bind_group ="
compact_neighbor_lines[75] = "      context.create_bind_group(shaders::model::particle_render::BindGroup1Descriptor {"
compact_neighbor_lines[76] = "        color_texture: color,"
compact_neighbor_lines[77] = "        color_sampler: color_sampler.binding(),"
compact_neighbor_lines[78] = "        normal_texture: normal,"
compact_neighbor_lines[79] = "        roughness_metallic_texture: metallic_roughness,"
compact_neighbor_lines[80] = "        primitive_material: materials.materials.binding(),"
compact_neighbor_lines[81] = "        primitive_transform: primitive_transforms.binding(),"
compact_neighbor_lines[82] = "      });"
compact_neighbor_lines[83] = ""
compact_neighbor_lines[84] = "    // TODO: Initial size is not correct, primitive_ge.tangents is larger"
compact_neighbor_lines[85] = "    Self {"
compact_neighbor_lines[86] = "      primitives,"
compact_neighbor_lines[87] = "      materials,"
compact_neighbor_lines[88] = "      bind_group,"
compact_neighbor_lines[89] = "      particle_bind_group,"
compact_neighbor_lines[90] = "    }"
compact_neighbor_lines[91] = "  }"
compact_neighbor_lines[92] = ""
compact_neighbor_lines[93] = "  pub fn insert(&mut self, model: &Model, repository: &ModelRepository) {"
compact_neighbor_lines[94] = "    for mesh in model.mesh_iter() {}"
compact_neighbor_lines[95] = "  }"
compact_neighbor_lines[96] = "}"

local typescript_lines = {
  "class Widget {",
  "  render() {",
  "    const value = 2;",
  "    return value;",
  "  }",
  "}",
}

local multi_function_lines = {
  "impl MultiFunction {",
  "  fn first(&self) {",
  "    let first_value = 2;",
  "  }",
  "",
  "  fn second(&self) {",
  "    let second_value = 2;",
  "  }",
  "",
  "  fn third(&self) {",
  "    let third_value = 2;",
  "  }",
  "} // impl MultiFunction",
}

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
  "diff --git a/src/repeated_context.rs b/src/repeated_context.rs",
  "index 3333333..4444444 100644",
  "--- a/src/repeated_context.rs",
  "+++ b/src/repeated_context.rs",
  "@@ -67 +67 @@",
  "-      color_texture: color,",
  "+      color_texture: color.clone(),",
  "@@ -69,2 +69,2 @@",
  "-      normal_texture: normal,",
  "-      roughness_metallic_texture: metallic_roughness,",
  "+      normal_texture: normal.clone(),",
  "+      roughness_metallic_texture: metallic_roughness.clone(),",
  "@@ -88,0 +89 @@",
  "+      particle_bind_group,",
  "@@ -96,0 +97 @@",
  "+    let tail_marker = ();",
  "diff --git a/src/compact_neighbor.rs b/src/compact_neighbor.rs",
  "index 5555555..6666666 100644",
  "--- a/src/compact_neighbor.rs",
  "+++ b/src/compact_neighbor.rs",
  "@@ -57 +67 @@",
  "-      color_texture: color,",
  "+      color_texture: color.clone(),",
  "@@ -59,2 +69,2 @@",
  "-      normal_texture: normal,",
  "-      roughness_metallic_texture: metallic_roughness,",
  "+      normal_texture: normal.clone(),",
  "+      roughness_metallic_texture: metallic_roughness.clone(),",
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
  "diff --git a/src/widget.ts b/src/widget.ts",
  "index 7777777..8888888 100644",
  "--- a/src/widget.ts",
  "+++ b/src/widget.ts",
  "@@ -3 +3 @@",
  "-    const value = 1;",
  "+    const value = 2;",
  "diff --git a/src/multi_function.rs b/src/multi_function.rs",
  "index 9999999..aaaaaaa 100644",
  "--- a/src/multi_function.rs",
  "+++ b/src/multi_function.rs",
  "@@ -3 +3 @@",
  "-    let first_value = 1;",
  "+    let first_value = 2;",
  "@@ -7 +7 @@",
  "-    let second_value = 1;",
  "+    let second_value = 2;",
  "@@ -11 +11 @@",
  "-    let third_value = 1;",
  "+    let third_value = 2;",
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
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return { "M\tsrc/model_store.rs", "M\tsrc/repeated_context.rs", "M\tsrc/compact_neighbor.rs", "M\tsrc/widget.ts", "M\tsrc/multi_function.rs" }, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return vim.split(diff_text, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t--no-color\t--format=%H%x09%h%x09%cI%x09%s\t-20" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tshow\t:0:src/model_store.rs" then return source_lines, 0 end
  if key == "git\t-C\t" .. root .. "\tshow\t:0:src/repeated_context.rs" then return repeated_context_lines, 0 end
  if key == "git\t-C\t" .. root .. "\tshow\t:0:src/compact_neighbor.rs" then return compact_neighbor_lines, 0 end
  if key == "git\t-C\t" .. root .. "\tshow\t:0:src/widget.ts" then return typescript_lines, 0 end
  if key == "git\t-C\t" .. root .. "\tshow\t:0:src/multi_function.rs" then return multi_function_lines, 0 end
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

local function count_rows_in_file_section(buf, file_pattern, pattern)
  local lines = buffer_lines(buf)
  local start_row = nil
  for index, line in ipairs(lines) do
    if line:find(file_pattern, 1, true) then
      start_row = index
      break
    end
  end
  assert_true(start_row ~= nil, "missing file section: " .. file_pattern .. "\n" .. table.concat(lines, "\n"))
  local count = 0
  for index = start_row + 1, #lines do
    local line = lines[index]
    if line:find("^Modified ") and not line:find(file_pattern, 1, true) then break end
    if line:find(pattern, 1, true) then count = count + 1 end
  end
  return count
end

local function find_row_in_file_section_after(buf, file_pattern, pattern, after_row)
  local lines = buffer_lines(buf)
  local start_row = nil
  for index, line in ipairs(lines) do
    if line:find(file_pattern, 1, true) then
      start_row = index
      break
    end
  end
  assert_true(start_row ~= nil, "missing file section: " .. file_pattern .. "\n" .. table.concat(lines, "\n"))
  if after_row and after_row > start_row then start_row = after_row end
  for index = start_row + 1, #lines do
    local line = lines[index]
    if line:find("^Modified ") and not line:find(file_pattern, 1, true) then break end
    if line:find(pattern, 1, true) then return index end
  end
  error("missing row in " .. file_pattern .. ": " .. pattern .. "\n" .. table.concat(lines, "\n"), 2)
end

local function section_has_pattern_between(buf, file_pattern, pattern, start_row, end_row)
  local lines = buffer_lines(buf)
  assert_true(start_row < end_row, "invalid row range for " .. file_pattern)
  for index = start_row + 1, end_row - 1 do
    local line = lines[index] or ""
    if line:find("^Modified ") and not line:find(file_pattern, 1, true) then break end
    if line:find(pattern, 1, true) then return true end
  end
  return false
end

local function gutter_text(buf, row)
  local marks = vim.api.nvim_buf_get_extmarks(buf, ui.status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
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
  assert_true(vim.fn.writefile(repeated_context_lines, root .. "/src/repeated_context.rs") == 0, "writefile failed")
  assert_true(vim.fn.writefile(compact_neighbor_lines, root .. "/src/compact_neighbor.rs") == 0, "writefile failed")
  assert_true(vim.fn.writefile(typescript_lines, root .. "/src/widget.ts") == 0, "writefile failed")
  assert_true(vim.fn.writefile(multi_function_lines, root .. "/src/multi_function.rs") == 0, "writefile failed")

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

  assert_true(count_rows(buf, "    Self {") == 1, "semantic bridge context should not be duplicated\n" .. buffer_dump(buf))
  local self_row = find_row(buf, "    Self {")
  local self_gutter = gutter_text(buf, self_row)
  assert_true(self_gutter:find("^%s*76%s+85%s+$", 1) ~= nil, "semantic context gutter should map old/new line numbers: " .. self_gutter)
  local insert_old_line = hunk_model.old_line_for_new_line({
    { old_start = 73, old_count = 0, new_start = 74, new_count = 9 },
    { old_start = 88, old_count = 0, new_start = 89, new_count = 1 },
    { old_start = 74, old_count = 9, new_start = 93, new_count = 0 },
  }, 93)
  assert_true(
    insert_old_line == 73,
    "pure deletion parent context should use old_start - 1 with current new line, got " .. tostring(insert_old_line)
  )
  assert_true(
    hunk_model.render_coords_adjacent(73, 93, 74, nil),
    "boundary parent and first deletion row should be adjacent on the old side"
  )
  local for_mesh_old_line = hunk_model.old_line_for_new_line({
    { old_start = 73, old_count = 0, new_start = 74, new_count = 9 },
    { old_start = 88, old_count = 0, new_start = 89, new_count = 1 },
    { old_start = 74, old_count = 9, new_start = 93, new_count = 0 },
  }, 94)
  local enumerate_old_line = hunk_model.old_line_for_new_line({
    { old_start = 73, old_count = 0, new_start = 74, new_count = 9 },
    { old_start = 88, old_count = 0, new_start = 89, new_count = 1 },
    { old_start = 74, old_count = 9, new_start = 93, new_count = 0 },
  }, 95)
  local material_old_line = hunk_model.old_line_for_new_line({
    { old_start = 73, old_count = 0, new_start = 74, new_count = 9 },
    { old_start = 88, old_count = 0, new_start = 89, new_count = 1 },
    { old_start = 74, old_count = 9, new_start = 93, new_count = 0 },
  }, 96)
  assert_true(for_mesh_old_line == 83, "post-deletion context line 94 should map to old line 83, got " .. tostring(for_mesh_old_line))
  assert_true(enumerate_old_line == 84, "post-deletion context line 95 should map to old line 84, got " .. tostring(enumerate_old_line))
  assert_true(material_old_line == 85, "post-deletion context line 96 should map to old line 85, got " .. tostring(material_old_line))
  wait_for(function()
    return count_rows(buf, "pub fn new(context: &gpu::Context) -> Self {") == 1
  end, "same-scope neighboring display groups should render the function opener once\n" .. buffer_dump(buf))
  local model_impl_row = find_row_in_file_section_after(buf, "model_store.rs", "impl ModelStore {", nil)
  local model_new_row = find_row_in_file_section_after(buf, "model_store.rs", "pub fn new(context: &gpu::Context) -> Self {", model_impl_row)
  assert_true(
    model_new_row == model_impl_row + 1,
    "adjacent Rust impl/function ancestors should render without an ellipsis\n" .. buffer_dump(buf)
  )
  assert_true(
    count_rows_in_file_section(buf, "model_store.rs", "impl ModelStore {") == 1,
    "Rust impl ancestor should render once for neighboring hunks\n" .. buffer_dump(buf)
  )
  assert_true(
    count_rows(buf, "@@ +1 -0") == 0,
    "nested Self insertion should not render as a standalone +1 hunk\n" .. buffer_dump(buf)
  )

  trigger_normal_mapping("<Tab>", find_row(buf, "repeated_context.rs"))
  wait_for(function()
    return buffer_contains(buf, "particle_bind_group,")
      and buffer_contains(buf, "let tail_marker = ();")
      and buffer_contains(buf, "build_repeated_context")
  end, "repeated context file did not render\n" .. buffer_dump(buf))
  assert_true(
    count_rows(buf, "build_repeated_context") == 1,
    "same-scope display groups should not repeat the function opener\n" .. buffer_dump(buf)
  )
  local repeated_impl_row = find_row_in_file_section_after(buf, "repeated_context.rs", "impl RepeatedContext {", nil)
  local repeated_fn_row = find_row_in_file_section_after(buf, "repeated_context.rs", "pub fn build_repeated_context", repeated_impl_row)
  assert_true(
    section_has_pattern_between(buf, "repeated_context.rs", "...", repeated_impl_row, repeated_fn_row),
    "distant Rust impl/function ancestors should render an ellipsis between boundary rows\n" .. buffer_dump(buf)
  )
  assert_true(
    count_rows_in_file_section(buf, "repeated_context.rs", "impl RepeatedContext {") == 1,
    "distant Rust impl ancestor should render once\n" .. buffer_dump(buf)
  )
  assert_true(
    count_rows_in_file_section(buf, "repeated_context.rs", "@@ +1 -0") >= 1,
    "long same-scope gaps should remain split into separate display hunks\n" .. buffer_dump(buf)
  )

  trigger_normal_mapping("<Tab>", find_row(buf, "compact_neighbor.rs"))
  wait_for(function()
    return pcall(find_row_in_file_section_after, buf, "compact_neighbor.rs", "let particle_bind_group =", nil)
      and pcall(find_row_in_file_section_after, buf, "compact_neighbor.rs", "normal_texture: normal.clone()", nil)
      and pcall(find_row_in_file_section_after, buf, "compact_neighbor.rs", "particle_bind_group,", nil)
  end, "compact neighbor file did not render\n" .. buffer_dump(buf))
  assert_true(
    count_rows_in_file_section(buf, "compact_neighbor.rs", "@@ ") <= 2,
    "compact replacement plus neighboring insert should not split into extra display hunks\n" .. buffer_dump(buf)
  )
  assert_true(
    count_rows_in_file_section(buf, "compact_neighbor.rs", "@@ +1 -0") == 0,
    "neighboring Self insertion should not render as a standalone +1 hunk\n" .. buffer_dump(buf)
  )
  local particle_binding_row = find_row_in_file_section_after(buf, "compact_neighbor.rs", "let particle_bind_group =", nil)
  local particle_close_row = find_row_in_file_section_after(buf, "compact_neighbor.rs", "      });", particle_binding_row)
  local todo_row = find_row_in_file_section_after(buf, "compact_neighbor.rs", "// TODO: Initial size is not correct", particle_close_row)
  local compact_self_row = find_row_in_file_section_after(buf, "compact_neighbor.rs", "    Self {", todo_row)
  assert_true(
    particle_close_row < todo_row and todo_row < compact_self_row,
    "small-gap merged hunks should render bridge context instead of jumping from the add block to Self\n" .. buffer_dump(buf)
  )

  trigger_normal_mapping("<Tab>", find_row(buf, "widget.ts"))
  wait_for(function()
    return pcall(find_row_in_file_section_after, buf, "widget.ts", "class Widget {", nil)
      and pcall(find_row_in_file_section_after, buf, "widget.ts", "render() {", nil)
      and pcall(find_row_in_file_section_after, buf, "widget.ts", "const value = 2;", nil)
  end, "TypeScript class/method ancestor context did not render\n" .. buffer_dump(buf))
  local class_row = find_row_in_file_section_after(buf, "widget.ts", "class Widget {", nil)
  local method_row = find_row_in_file_section_after(buf, "widget.ts", "render() {", class_row)
  assert_true(
    method_row == class_row + 1,
    "adjacent TypeScript class/method ancestors should render without an ellipsis\n" .. buffer_dump(buf)
  )

  trigger_normal_mapping("<Tab>", find_row(buf, "multi_function.rs"))
  wait_for(function()
    return pcall(find_row_in_file_section_after, buf, "multi_function.rs", "impl MultiFunction {", nil)
      and pcall(find_row_in_file_section_after, buf, "multi_function.rs", "let first_value = 2;", nil)
      and pcall(find_row_in_file_section_after, buf, "multi_function.rs", "let third_value = 2;", nil)
      and pcall(find_row_in_file_section_after, buf, "multi_function.rs", "} // impl MultiFunction", nil)
  end, "multi-function ancestor context did not render\n" .. buffer_dump(buf))
  assert_true(
    count_rows_in_file_section(buf, "multi_function.rs", "impl MultiFunction {") == 1,
    "same-impl hunks should render the ancestor opener only on the first displayed hunk\n" .. buffer_dump(buf)
  )
  assert_true(
    count_rows_in_file_section(buf, "multi_function.rs", "} // impl MultiFunction") == 1,
    "same-impl hunks should render the ancestor closer only on the last displayed hunk\n" .. buffer_dump(buf)
  )
  local multi_impl_row = find_row_in_file_section_after(buf, "multi_function.rs", "impl MultiFunction {", nil)
  local multi_first_row = find_row_in_file_section_after(buf, "multi_function.rs", "fn first", multi_impl_row)
  local multi_third_row = find_row_in_file_section_after(buf, "multi_function.rs", "let third_value = 2;", multi_first_row)
  local multi_close_row = find_row_in_file_section_after(buf, "multi_function.rs", "} // impl MultiFunction", multi_third_row)
  assert_true(
    multi_impl_row < multi_first_row and multi_third_row < multi_close_row,
    "same-impl ancestor opener/closer should bracket the displayed hunk run\n" .. buffer_dump(buf)
  )
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
