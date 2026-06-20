vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local root = vim.fs.normalize(original_cwd .. "/.diffreview-boundary-context-test")
local calls = {}
local original_compute_diff_syntax_async = diff_review.compute_diff_syntax_async
local original_compute_file_syntax_async = diff_review.compute_file_syntax_async

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local diff_text = table.concat({
  "diff --git a/src/engine.rs b/src/engine.rs",
  "index 1111111..2222222 100644",
  "--- a/src/engine.rs",
  "+++ b/src/engine.rs",
  "@@ -11 +11 @@",
  "-    let stderr_layer = tracing_subscriber::fmt::layer().with_writer(std::io::stderr);",
  "+    let stderr_laye = tracing_subscriber::fmt::layer().with_writer(std::io::stderr);",
  "@@ -12 +12 @@",
  "-    let writer = LOG_WRITER.get_or_init(|| SwappableWriter::new(open_log()));",
  "+    let writer = LOG_WRITER.get_or_int(|| SwappableWriter::new(open_log()));",
  "diff --git a/src/tangent.rs b/src/tangent.rs",
  "index 3333333..4444444 100644",
  "--- a/src/tangent.rs",
  "+++ b/src/tangent.rs",
  "@@ -4,0 +5,4 @@",
  "+pub struct TangentBasis {",
  "+  pub tangent0: Vec3,",
  "+  pub tangent1: Vec3,",
  "+}",
  "diff --git a/src/material.rs b/src/material.rs",
  "index 7777777..8888888 100644",
  "--- a/src/material.rs",
  "+++ b/src/material.rs",
  "@@ -21 +21 @@",
  "-      color_texture: color,",
  "+      color_texture: color.clone(),",
  "diff --git a/src/model_store.rs b/src/model_store.rs",
  "index 8888888..9999999 100644",
  "--- a/src/model_store.rs",
  "+++ b/src/model_store.rs",
  "@@ -16,0 +17 @@",
  "+  pub particle_bind_group: gpu::BindGroup<shaders::model::particle_render::BindGroup1Descriptor>,",
  "@@ -17,0 +19,9 @@",
  "+",
  "+#[derive(Debug)]",
  "+pub struct ParticleModelDrawCommand {",
  "+  pub first_index: u32,",
  "+  pub index_count: u32,",
  "+  pub base_vertex: u32,",
  "+  pub primitive_instance: u32,",
  "+  pub debug_info: String,",
  "+}",
  "diff --git a/src/render_system.rs b/src/render_system.rs",
  "index 9999999..aaaaaaa 100644",
  "--- a/src/render_system.rs",
  "+++ b/src/render_system.rs",
  "@@ -5 +5,2 @@",
  "-      CameraBuffer, ModelStore, TransformBuffer, model_store::InstructionBundleBuilder,",
  "+      CameraBuffer, ModelStore, ParticleModelDrawParams, ParticlePbRenderPipeline, TransformBuffer,",
  "+      model_store::{InstructionBundleBuilder, ParticleModelDrawCommand},",
  "@@ -11 +12 @@",
  "-  render::GpuState,",
  "+  render::{GpuState, ParticleInstanceSources},",
  "diff --git a/src/struct_gap.rs b/src/struct_gap.rs",
  "index bbbbbbb..ccccccc 100644",
  "--- a/src/struct_gap.rs",
  "+++ b/src/struct_gap.rs",
  "@@ -17 +17 @@",
  "-  transforms: (),",
  "+  transforms: TransformBuffer,",
  "diff --git a/src/render_barrier.rs b/src/render_barrier.rs",
  "index ddddddd..eeeeeee 100644",
  "--- a/src/render_barrier.rs",
  "+++ b/src/render_barrier.rs",
  "@@ -164,0 +165,6 @@",
  "+    let particle_source = engine.get::<ParticleInstanceSources>().get();",
  "+    if let Some(source) = particle_source",
  "+      && source.count > 0",
  "+      && source.model_asset.is_some()",
  "+      && !self.particle_commands.is_empty()",
  "+    {",
  "diff --git a/src/notes.txt b/src/notes.txt",
  "index 5555555..6666666 100644",
  "--- a/src/notes.txt",
  "+++ b/src/notes.txt",
  "@@ -5 +5 @@",
  "-old target",
  "+new target",
}, "\n")

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  calls[#calls + 1] = { kind = "systemlist", key = command_key(command) }
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "boundary test" }, 0 end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return { "M\tsrc/engine.rs", "M\tsrc/tangent.rs", "M\tsrc/material.rs", "M\tsrc/model_store.rs", "M\tsrc/render_system.rs", "M\tsrc/struct_gap.rs", "M\tsrc/render_barrier.rs", "M\tsrc/notes.txt" }, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return vim.split(diff_text, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    return {}, 0
  end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 5)
end

function backend.system(command, input)
  calls[#calls + 1] = { kind = "system", key = command_key(command), input = input }
  return "", 0
end

function backend.system_async(command, input, cb)
  vim.defer_fn(function()
    local output, code = backend.system(command, input)
    cb({ code = code, stdout = output, stderr = "", output = output })
  end, 5)
end

local function contains_line(lines, pattern)
  for _, line in ipairs(lines) do
    if line:find(pattern, 1, true) then return true end
  end
  return false
end

local function buffer_contains(buf, pattern)
  return contains_line(vim.api.nvim_buf_get_lines(buf, 0, -1, false), pattern)
end

local function buffer_dump(buf)
  return table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
end

local function find_row(buf, pattern)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:find(pattern, 1, true) then return index end
  end
  error("missing row: " .. pattern .. "\n" .. table.concat(lines, "\n"), 2)
end

local function find_row_in_file_section(buf, file_pattern, pattern)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local start_row = nil
  for index, line in ipairs(lines) do
    if line:find(file_pattern, 1, true) then
      start_row = index
      break
    end
  end
  if not start_row then return nil end
  for index = start_row + 1, #lines do
    local line = lines[index]
    if line:find("^Modified ") and not line:find(file_pattern, 1, true) then break end
    if line:find(pattern, 1, true) then return index end
  end
  return nil
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
  return nil
end

local function line_has_highlight(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.hl_group == hl_group or tostring(details.hl_group):find("^" .. vim.pesc(hl_group) .. "%.") then return true end
    if type(details.hl_group) == "table" then
      for _, group in ipairs(details.hl_group) do
        if group == hl_group or tostring(group):find("^" .. vim.pesc(hl_group) .. "%.") then return true end
      end
    end
  end
  return false
end

local function line_highlights(buf, row)
  local groups = {}
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if type(details.hl_group) == "table" then
      for _, group in ipairs(details.hl_group) do
        groups[#groups + 1] = tostring(group)
      end
    elseif details.hl_group then
      groups[#groups + 1] = tostring(details.hl_group)
    end
  end
  return table.concat(groups, ",")
end

local function count_boundary_rows(buf, pattern)
  local count = 0
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local boundary_lines = (diff_review._status and diff_review._status.boundary_lines) or {}
  for index, line in ipairs(lines) do
    if boundary_lines[index] and line:find(pattern, 1, true) then count = count + 1 end
  end
  return count
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

local function assert_padding_omits_trailing_delimiters()
  assert_true(diff_review._hunk_single_hidden_new_line(15, 17) == 16, "single hidden line should be detected")
  assert_true(diff_review._hunk_single_hidden_new_line(15, 18) == nil, "multi-line gaps should stay collapsed")

  local padding_source_lines = {}
  for line_number = 1, 90 do
    padding_source_lines[line_number] = ("// filler %d"):format(line_number)
  end
  padding_source_lines[72] = "        }"
  padding_source_lines[73] = "      }"
  padding_source_lines[74] = ""
  padding_source_lines[75] = "      if let Some(source) = &particle_source"

  local hunk = {
    lines = {
      {
        prefix = "+",
        new_line = 75,
        code = padding_source_lines[75],
      },
    },
  }
  local context = {
    start_row = 56,
    end_row = 89,
    path_start_rows = {},
    path_end_rows = {},
    sibling_before_rows = {},
    sibling_after_rows = {},
  }

  local padding = diff_review._hunk_context_padding_lines(
    padding_source_lines,
    hunk,
    context,
    "before",
    { [75] = true },
    nil,
    { changed_line = 75, after_line = 83 }
  )
  assert_true(#padding == 0, "trailing delimiter padding should be omitted")
  assert_true(not diff_review._hunk_context_padding_line_is_useful("        }"), "closing delimiter padding should not be useful")
  assert_true(not diff_review._hunk_context_padding_line_is_useful(""), "blank padding should not be useful")

  padding_source_lines[74] = "      let previous_particle = particle_source.clone();"
  padding = diff_review._hunk_context_padding_lines(
    padding_source_lines,
    hunk,
    context,
    "before",
    { [75] = true },
    nil,
    { changed_line = 75, after_line = 83 }
  )
  assert_true(
    #padding == 1 and padding[1].line_number == 74,
    "useful same-scope padding should still render before a changed line"
  )

  for line_number = 91, 180 do
    padding_source_lines[line_number] = ("// filler %d"):format(line_number)
  end
  padding_source_lines[162] = "      binding.draw_indexed(indices, command.base_vertex, instances)"
  padding_source_lines[163] = "    }"
  padding_source_lines[164] = ""
  padding_source_lines[165] = "    let particle_source = engine.get::<ParticleInstanceSources>().get();"
  hunk = {
    lines = {
      {
        prefix = "+",
        new_line = 165,
        code = padding_source_lines[165],
      },
    },
  }
  context = {
    start_row = 114,
    end_row = 179,
    path_start_rows = {},
    path_end_rows = {},
    sibling_before_rows = {},
    sibling_after_rows = {},
  }
  padding = diff_review._hunk_context_padding_lines(
    padding_source_lines,
    hunk,
    context,
    "before",
    { [165] = true },
    nil,
    { changed_line = 165, after_line = 171 }
  )
  assert_true(#padding == 0, "fallback padding should not skip over blank or delimiter barriers")
end

local function assert_current_file_jump(expected_file, expected_line, expected_text)
  local actual_file = vim.fs.normalize(vim.api.nvim_buf_get_name(0))
  assert_true(actual_file == vim.fs.normalize(expected_file), "expected current file " .. expected_file .. ", got " .. actual_file)
  local cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(cursor[1] == expected_line, "expected line " .. expected_line .. ", got " .. cursor[1])
  local line = vim.api.nvim_buf_get_lines(0, expected_line - 1, expected_line, false)[1] or ""
  assert_true(line == expected_text, "expected line text " .. expected_text .. ", got " .. line)
end

local function system_call_count()
  local count = 0
  for _, call in ipairs(calls) do
    if call.kind == "system" then count = count + 1 end
  end
  return count
end

local function run()
  assert_padding_omits_trailing_delimiters()

  vim.fn.delete(root, "rf")
  assert_true(vim.fn.mkdir(root .. "/src", "p") == 1, "mkdir failed")
  local diff_syntax_batches = {}
  local file_syntax_requests = {}
  diff_review.compute_diff_syntax_async = function(filename, lines_for_syntax, cb)
    diff_syntax_batches[#diff_syntax_batches + 1] = {
      filename = filename,
      lines = vim.deepcopy(lines_for_syntax),
    }
    original_compute_diff_syntax_async(filename, lines_for_syntax, cb)
  end
  diff_review.compute_file_syntax_async = function(filename, cb)
    file_syntax_requests[filename] = (file_syntax_requests[filename] or 0) + 1
    original_compute_file_syntax_async(filename, cb)
  end
  assert_true(vim.fn.writefile({
    "pub struct Engine {",
    "  world: ecs::World,",
    "  start: time::Instant,",
    "  last_draw: time::Instant,",
    "  since_start: time::Duration,",
    "  since_last_draw: time::Duration,",
    "}",
    "",
    "impl Engine {",
    "  pub fn new(bridge: Bridge) -> Self {",
    "    let stderr_laye = tracing_subscriber::fmt::layer().with_writer(std::io::stderr);",
    "    let writer = LOG_WRITER.get_or_int(|| SwappableWriter::new(open_log()));",
    "  }",
    "}",
  }, root .. "/src/engine.rs") == 0, "writefile failed")
  assert_true(vim.fn.writefile({
    "pub fn normalize(value: f32, length: f32) -> f32 {",
    "  return value / length;",
    "}",
    "",
    "pub struct TangentBasis {",
    "  pub tangent0: Vec3,",
    "  pub tangent1: Vec3,",
    "}",
  }, root .. "/src/tangent.rs") == 0, "writefile failed")
  assert_true(vim.fn.writefile({
    "use {",
    "  crate::{Model, ModelRepository},",
    "  engine::sys::gpu,",
    "};",
    "",
    "pub struct ModelStore {",
    "  pub bind_group: gpu::BindGroup<shaders::model::render::BindGroup1Descriptor>,",
    "}",
    "",
    "impl ModelStore {",
    "  pub fn new(context: &gpu::Context) -> Self {",
    "    let color = context.color();",
    "    let normal = context.normal();",
    "    let metallic_roughness = context.metallic_roughness();",
    "",
    "    self.materials.insert(shaders::model::render::Material {",
    "      color_texture: color,",
    "    });",
    "",
    "    let bind_group = context.create_bind_group(shaders::model::render::BindGroup1Descriptor {",
    "      color_texture: color.clone(),",
    "      color_sampler: color_sampler.binding(),",
    "      normal_texture: normal.clone(),",
    "      roughness_metallic_texture: metallic_roughness.clone(),",
    "    });",
    "  }",
    "}",
  }, root .. "/src/material.rs") == 0, "writefile failed")
  assert_true(vim.fn.writefile({
    "use {",
    "  crate::{",
    "    ModelComponent, ModelRepository,",
    "    pbr::{",
    "      CameraBuffer, ModelStore, ParticleModelDrawParams, ParticlePbRenderPipeline, TransformBuffer,",
    "      model_store::{InstructionBundleBuilder, ParticleModelDrawCommand},",
    "      pb_render_instructions::InstructionBundle, pb_render_pipeline::PbRenderPipeline,",
    "    },",
    "  },",
    "  base::{CameraComponent, TransformComponent},",
    "  engine::{Engine, ecs, sys::gpu},",
    "  render::{GpuState, ParticleInstanceSources},",
    "};",
    "",
    "pub struct ModelRenderSystem {",
    "}",
  }, root .. "/src/render_system.rs") == 0, "writefile failed")
  assert_true(vim.fn.writefile({
    "use {",
    "  crate::{",
    "    Model, ModelRepository, Primitive,",
    "    pbr::{",
    "      pb_materials::PbMaterials, pb_render_instructions::EntityIndex,",
    "      primitive_mesh_store::PrimitiveMeshStore,",
    "    },",
    "  },",
    "  engine::sys::gpu,",
    "  std::collections::HashMap,",
    "};",
    "",
    "pub struct ModelStore {",
    "  pub primitives: PrimitiveMeshStore,",
    "  pub materials: PbMaterials,",
    "  pub bind_group: gpu::BindGroup<shaders::model::render::BindGroup1Descriptor>,",
    "  pub particle_bind_group: gpu::BindGroup<shaders::model::particle_render::BindGroup1Descriptor>,",
    "}",
    "",
    "#[derive(Debug)]",
    "pub struct ParticleModelDrawCommand {",
    "  pub first_index: u32,",
    "  pub index_count: u32,",
    "  pub base_vertex: u32,",
    "  pub primitive_instance: u32,",
    "  pub debug_info: String,",
    "}",
    "",
    "impl ModelStore {",
    "}",
  }, root .. "/src/model_store.rs") == 0, "writefile failed")
  assert_true(vim.fn.writefile({
    "// 1",
    "// 2",
    "// 3",
    "// 4",
    "// 5",
    "// 6",
    "// 7",
    "// 8",
    "// 9",
    "// 10",
    "// 11",
    "// 12",
    "// 13",
    "// 14",
    "pub struct ModelRenderSystem {",
    "  camera: CameraBuffer,",
    "  transforms: TransformBuffer,",
    "}",
  }, root .. "/src/struct_gap.rs") == 0, "writefile failed")
  local render_barrier_lines = {}
  for line_number = 1, 180 do
    render_barrier_lines[line_number] = ("// filler %d"):format(line_number)
  end
  render_barrier_lines[1] = "struct Engine;"
  render_barrier_lines[2] = "struct Renderer { particle_commands: Vec<()> }"
  render_barrier_lines[3] = "struct ParticleInstanceSources;"
  render_barrier_lines[114] = "impl Renderer {"
  render_barrier_lines[115] = "  fn render(&mut self, engine: &Engine) {"
  render_barrier_lines[160] = "    for command in commands {"
  render_barrier_lines[161] = "      binding.insert_debug_marker(command_debug);"
  render_barrier_lines[162] = "      binding.draw_indexed(indices, command.base_vertex, instances)"
  render_barrier_lines[163] = "    }"
  render_barrier_lines[164] = ""
  render_barrier_lines[165] = "    let particle_source = engine.get::<ParticleInstanceSources>().get();"
  render_barrier_lines[166] = "    if let Some(source) = particle_source"
  render_barrier_lines[167] = "      && source.count > 0"
  render_barrier_lines[168] = "      && source.model_asset.is_some()"
  render_barrier_lines[169] = "      && !self.particle_commands.is_empty()"
  render_barrier_lines[170] = "    {"
  render_barrier_lines[171] = "    }"
  render_barrier_lines[172] = "  }"
  render_barrier_lines[173] = "}"
  assert_true(vim.fn.writefile(render_barrier_lines, root .. "/src/render_barrier.rs") == 0, "writefile failed")
  assert_true(vim.fn.writefile({
    "alpha",
    "beta",
    "gamma",
    "delta",
    "new target",
    "zeta",
    "eta",
    "theta",
    "iota",
  }, root .. "/src/notes.txt") == 0, "writefile failed")
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)

  diff_review.setup({ about_auto_generate = false })
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(buf, "engine.rs +2 -2") end, "status did not render file\n" .. buffer_dump(buf))

  trigger_normal_mapping("<Tab>", find_row(buf, "engine.rs"))
  local engine_header = "@@ +2 -2"
  wait_for(function()
    return buffer_contains(buf, engine_header)
  end, "compact hunk did not render\n" .. buffer_dump(buf))
  local delete_row = find_row(buf, "let stderr_layer = tracing_subscriber")
  local delete_gutter = gutter_text(buf, delete_row) or ""
  assert_true(delete_gutter:find("^%s*11%s+11%s+%~", 1) ~= nil, "compact row did not use composite local gutter: " .. delete_gutter)
  assert_true(line_has_highlight(buf, delete_row, "DiffReviewInlineDeleteBg"), "compact row did not highlight the deleted span")
  wait_for(function()
    local row = find_row(buf, "let stderr_layer = tracing_subscriber")
    return line_has_highlight(buf, row, "@keyword")
  end, "delete hunk body row did not get Tree-sitter keyword highlight: " .. line_highlights(buf, delete_row))
  wait_for(function()
    return file_syntax_requests[root .. "/src/engine.rs"] ~= nil or #diff_syntax_batches > 0
  end, "syntax renderer did not request any file or diff syntax")
  for _, batch in ipairs(diff_syntax_batches) do
    local batch_text = table.concat(batch.lines or {}, "\n")
    assert_true(
      not (batch_text:find("stderr_layer =", 1, true) and batch_text:find("stderr_laye =", 1, true)),
      "diff syntax batch mixed old and new engine lines:\n" .. batch_text
    )
    assert_true(
      not (batch_text:find("get_or_init(", 1, true) and batch_text:find("get_or_int(", 1, true)),
      "diff syntax batch mixed old and new writer lines:\n" .. batch_text
    )
  end
  wait_for(function()
    for _, value in pairs(diff_review._ts_context_cache or {}) do
      if type(value) == "table" and value.label == "Engine.new" then return true end
    end
    return false
  end, "Tree-sitter context was not cached\n" .. buffer_dump(buf))
  wait_for(function() return buffer_contains(buf, "pub fn new(bridge: Bridge) -> Self {") end, "opening boundary did not render\n" .. buffer_dump(buf))
  local boundary_row = find_row(buf, "pub fn new(bridge: Bridge) -> Self {")
  local first_header_row = find_row(buf, engine_header)
  local updated_delete_row = find_row(buf, "let stderr_layer = tracing_subscriber")
  assert_true(
    first_header_row < boundary_row and boundary_row < updated_delete_row,
    "opening boundary should render after hunk header and before hunk body\n" .. buffer_dump(buf)
  )
  local boundary_gutter = gutter_text(buf, boundary_row) or ""
  assert_true(boundary_gutter:find("^%s*10%s+10%s+$", 1) ~= nil, "boundary line did not use diff gutter: " .. boundary_gutter)
  assert_true(line_has_highlight(buf, boundary_row, "@keyword"), "boundary row did not get keyword syntax highlight: " .. line_highlights(buf, boundary_row))
  assert_true(line_has_highlight(buf, boundary_row, "@type"), "boundary row did not get type syntax highlight: " .. line_highlights(buf, boundary_row))
  local current_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index = boundary_row + 1, updated_delete_row - 1 do
    assert_true(
      not current_lines[index]:find("...", 1, true),
      "adjacent boundary and changed row should not render an ellipsis\n" .. buffer_dump(buf)
    )
  end
  wait_for(function() return buffer_contains(buf, "}") end, "closing boundary did not render\n" .. buffer_dump(buf))
  wait_for(function()
    return count_boundary_rows(buf, "pub fn new(bridge: Bridge) -> Self {") == 1
  end, "neighboring same-scope hunks repeated boundary rows\n" .. buffer_dump(buf))
  local closing_gutter = gutter_text(buf, find_row(buf, "  }")) or ""
  assert_true(closing_gutter:find("^%s*13%s+13%s+$", 1) ~= nil, "closing boundary did not use diff gutter: " .. closing_gutter)
  wait_for(function() return buffer_contains(buf, engine_header) end, "compact hunk header did not render\n" .. buffer_dump(buf))
  local header_row = find_row(buf, engine_header)
  local header_line = vim.api.nvim_buf_get_lines(buf, header_row - 1, header_row, false)[1]
  assert_true(header_line:find("^@@ %+2 %-2", 1) ~= nil, "hunk header unexpectedly included context or gutter: " .. header_line)

  local before = system_call_count()
  trigger_normal_mapping("S", boundary_row)
  vim.wait(50)
  assert_true(system_call_count() == before, "boundary row triggered a stage action")
  trigger_normal_mapping("<CR>", boundary_row)
  assert_current_file_jump(root .. "/src/engine.rs", 10, "  pub fn new(bridge: Bridge) -> Self {")
  vim.api.nvim_win_set_buf(0, buf)

  trigger_normal_mapping("<Tab>", find_row(buf, engine_header))
  local collapsed_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(contains_line(collapsed_lines, engine_header), "collapsed hunk header missing")
  assert_true(not contains_line(collapsed_lines, "let stderr_layer = tracing_subscriber"), "collapsed hunk showed body")

  trigger_normal_mapping("<Tab>", find_row(buf, "tangent.rs"))
  wait_for(function()
    return buffer_contains(buf, "@@ +4 -0")
  end, "struct hunk header did not render\n" .. buffer_dump(buf))
  wait_for(function()
    return buffer_contains(buf, "pub struct TangentBasis {")
  end, "struct hunk did not render added scope start\n" .. buffer_dump(buf))
  wait_for(function()
    local tangent_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    return not contains_line(tangent_lines, "return value / length;")
  end, "struct hunk showed context from previous sibling scope\n" .. buffer_dump(buf))

  trigger_normal_mapping("<Tab>", find_row(buf, "material.rs"))
  wait_for(function()
    local ok_bind, bind_group_row = pcall(find_row, buf, "let bind_group = context.create_bind_group")
    local ok_add, material_add_row = pcall(find_row, buf, "color_texture: color.clone()")
    local ok_close, material_close_row = pcall(find_row, buf, "    });")
    if not (ok_bind and ok_add and ok_close) then return false end
    local material_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    return bind_group_row < material_add_row
      and material_add_row < material_close_row
      and not contains_line(material_lines, "self.materials.insert")
  end, "material hunk did not render ancestor-path padding\n" .. buffer_dump(buf))
  local bind_group_row = find_row(buf, "let bind_group = context.create_bind_group")
  local material_add_row = find_row(buf, "color_texture: color.clone()")
  local material_close_row = find_row(buf, "    });")
  assert_true(bind_group_row < material_add_row, "ancestor opener should render before the changed line\n" .. buffer_dump(buf))
  assert_true(material_add_row < material_close_row, "first rendered closer should belong to the changed line ancestor path\n" .. buffer_dump(buf))
  local material_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(not contains_line(material_lines, "self.materials.insert"), "ancestor-path padding included a previous sibling statement")
  assert_true(not contains_line(material_lines, "use {"), "ancestor-path padding included the file root opener")
  local material_impl_row = find_row(buf, "impl ModelStore {")
  local material_fn_row = find_row(buf, "pub fn new(context: &gpu::Context) -> Self {")
  assert_true(
    material_impl_row < material_fn_row and material_fn_row < bind_group_row,
    "nearest semantic parent should render before selected function context\n" .. buffer_dump(buf)
  )
  trigger_normal_mapping("<CR>", bind_group_row)
  assert_current_file_jump(root .. "/src/material.rs", 20, "    let bind_group = context.create_bind_group(shaders::model::render::BindGroup1Descriptor {")
  vim.api.nvim_win_set_buf(0, buf)
  trigger_normal_mapping("S", bind_group_row)
  vim.wait(50)
  assert_true(system_call_count() == before, "ancestor opener padding row triggered a stage action")
  trigger_normal_mapping("S", material_close_row)
  vim.wait(50)
  assert_true(system_call_count() == before, "ancestor closer padding row triggered a stage action")

  trigger_normal_mapping("<Tab>", find_row(buf, "model_store.rs"))
  wait_for(function()
    return buffer_contains(buf, "@@ +10 -0")
      and buffer_contains(buf, "pub struct ParticleModelDrawCommand")
  end, "model store hunk did not render\n" .. buffer_dump(buf))
  local model_store_header_row = find_row(buf, "@@ +10 -0")
  local particle_command_row = find_row(buf, "pub struct ParticleModelDrawCommand")
  local model_store_impl_row = find_row_in_file_section(buf, "model_store.rs", "impl ModelStore")
  assert_true(
    model_store_header_row < particle_command_row,
    "new sibling struct should render inside the model store display hunk\n" .. buffer_dump(buf)
  )
  assert_true(
    model_store_impl_row == nil,
    "trailing context crossed into the next top-level sibling scope\n" .. buffer_dump(buf)
  )

  trigger_normal_mapping("<Tab>", find_row(buf, "render_system.rs"))
  wait_for(function()
    return buffer_contains(buf, "@@ +3 -2")
      and buffer_contains(buf, "render::{GpuState, ParticleInstanceSources}")
      and buffer_contains(buf, "};")
      and not buffer_contains(buf, "pub struct ModelRenderSystem")
  end, "rust use hunk did not render with scoped import context\n" .. buffer_dump(buf))

  trigger_normal_mapping("<Tab>", find_row(buf, "struct_gap.rs"))
  wait_for(function()
    return buffer_contains(buf, "pub struct ModelRenderSystem")
      and buffer_contains(buf, "camera: CameraBuffer")
      and buffer_contains(buf, "transforms: TransformBuffer")
  end, "single-line boundary gap did not render concrete context\n" .. buffer_dump(buf))
  local struct_row = find_row(buf, "pub struct ModelRenderSystem")
  local camera_row = find_row(buf, "camera: CameraBuffer")
  local transforms_row = find_row(buf, "transforms: TransformBuffer")
  assert_true(struct_row < camera_row and camera_row < transforms_row, "single hidden context line should replace ellipsis\n" .. buffer_dump(buf))
  local struct_gap_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for row = struct_row + 1, transforms_row - 1 do
    assert_true(not struct_gap_lines[row]:find("...", 1, true), "single-line gap should not render ellipsis\n" .. buffer_dump(buf))
  end

  trigger_normal_mapping("<Tab>", find_row(buf, "render_barrier.rs"))
  wait_for(function()
    return buffer_contains(buf, "let particle_source = engine.get::<ParticleInstanceSources>().get()")
      and not buffer_contains(buf, "binding.draw_indexed")
  end, "fallback padding crossed a closed block boundary\n" .. buffer_dump(buf))

  trigger_normal_mapping("<Tab>", find_row(buf, "notes.txt"))
  wait_for(function()
    return buffer_contains(buf, "gamma")
      and buffer_contains(buf, "delta")
      and buffer_contains(buf, "zeta")
      and buffer_contains(buf, "theta")
  end, "plain text hunk did not render fallback padding\n" .. buffer_dump(buf))
  local notes_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(not contains_line(notes_lines, "alpha"), "fallback padding rendered more than three lines before the hunk")
  assert_true(not contains_line(notes_lines, "iota"), "fallback padding rendered more than three lines after the hunk")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
diff_review.compute_diff_syntax_async = original_compute_diff_syntax_async
diff_review.compute_file_syntax_async = original_compute_file_syntax_async
vim.fn.delete(root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
