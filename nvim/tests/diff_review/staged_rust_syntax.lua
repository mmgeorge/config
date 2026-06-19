vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local root = vim.fs.normalize(original_cwd .. "/.diffreview-staged-rust-syntax-test")
local calls = {}

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

local particle_system_diff = table.concat({
  "diff --git a/blue/engine/plugins/physics/src/particle_system.rs b/blue/engine/plugins/physics/src/particle_system.rs",
  "index dca15ea2..5fa4d134 100644",
  "--- a/blue/engine/plugins/physics/src/particle_system.rs",
  "+++ b/blue/engine/plugins/physics/src/particle_system.rs",
  "@@ -205,11 +116,22 @@ impl ecs::System for ParticleSystem {",
  " }",
  " ",
  " impl ecs::System for ParticleSystem {",
  "+  fn order(&self) -> u32 {",
  "+    50",
  "+  }",
  "+",
  "   fn run(&mut self, engine: &Engine) {",
  "     self.process_spawns(engine);",
  "     self.process_events(engine);",
  " ",
  "-    if self.particle_count == 0 {",
  "+    let storage = engine.get::<ParticleStorage>();",
  "+    let particle_count = storage.particle_count();",
  "+    let instance_sources = engine.get::<ParticleInstanceSources>();",
  "+    let render_mode = active_particle_render_mode(engine);",
  "+    instance_sources.set(Some(storage.instance_source(render_mode.model_asset())));",
  "+",
  "+    if particle_count == 0 {",
  "+      instance_sources.set(None);",
  "       return;",
  "     }",
  " ",
  "@@ -217,47 +139,35 @@ impl ecs::System for ParticleSystem {",
  "     let context = &state.context;",
  " ",
  "     let events = engine.get::<Events<ParticleEvent>>();",
  "-    let camera = engine.world().execute_query::<&CameraComponent>();",
  "-    let camera = camera.iter().next().unwrap();",
  "-    let window = engine.window();",
  "-",
  "-    let projection = camera.projection_mat4(window.as_ref());",
  "-    let view = camera.view_mat4();",
  "-    self.render_locals.camera.clear();",
  "-    self",
  "-      .render_locals",
  "-      .camera",
  "-      .push(shaders::Camera { projection, view });",
  " ",
  "     let solver_iteration_count = 10;",
  "     let fixed_dt = 1.0 / 60.0_f32;",
  "     let substep_count = 1u32;",
  " ",
  "-    self.compute_locals.scene.clear();",
  "-    self.compute_locals.scene.extend(&[shaders::Scene {",
  "-      count: self.particle_count,",
  "+    storage.compute_locals.scene.clear();",
  "+    storage.compute_locals.scene.extend(&[shaders::Scene {",
  "+      count: particle_count,",
  "       dt: fixed_dt,",
  "       _padding: [0; 8],",
  "     }]);",
  "-    self.compute_locals.scene.upload(context);",
  "-    self.render_locals.camera.upload(context);",
  "+    storage.compute_locals.scene.upload(context);",
  " ",
  "     let encoder = context.create_command_encoder(&gpu::CommandEncoderDescriptor {",
  "       label: Some(gpu::label!(\"encoder\")),",
}, "\n")

local sparse_fragment_diff = table.concat({
  "diff --git a/src/fragment.rs b/src/fragment.rs",
  "index 1111111..2222222 100644",
  "--- a/src/fragment.rs",
  "+++ b/src/fragment.rs",
  "@@ -8,1 +8,1 @@",
  "-      Widget::new(CameraComponent { value })",
  "+      Widget::new(CameraComponent { value: value + 1 })",
}, "\n")

local shifted_old_line_diff = table.concat({
  "diff --git a/src/multi_hunk.rs b/src/multi_hunk.rs",
  "index 3333333..4444444 100644",
  "--- a/src/multi_hunk.rs",
  "+++ b/src/multi_hunk.rs",
  "@@ -6,6 +6,3 @@ impl Builder {",
  "   fn setup(&self) {",
  "-    let removed_a = CameraComponent { value: 1 };",
  "-    let removed_b = CameraComponent { value: 2 };",
  "-    let removed_c = CameraComponent { value: 3 };",
  "     let keep = true;",
  "   }",
  " ",
  "@@ -20,1 +17,1 @@ impl Builder {",
  "-      Widget::new(CameraComponent { value })",
  "+      Widget::new(CameraComponent { value: value + 1 })",
}, "\n")

local unstaged_shifted_old_line_diff = shifted_old_line_diff:gsub("src/multi_hunk%.rs", "src/unstaged_multi_hunk.rs")

local ferrous_model_store_diff = table.concat({
  "diff --git a/blue/engine/plugins/model/src/pbr/model_store.rs b/blue/engine/plugins/model/src/pbr/model_store.rs",
  "index 861a6248..9c10261a 100644",
  "--- a/blue/engine/plugins/model/src/pbr/model_store.rs",
  "+++ b/blue/engine/plugins/model/src/pbr/model_store.rs",
  "@@ -14,6 +14,16 @@ pub struct ModelStore {",
  "   pub primitives: PrimitiveMeshStore,",
  "   pub materials: PbMaterials,",
  "   pub bind_group: gpu::BindGroup<shaders::model::render::BindGroup1Descriptor>,",
  "+  pub particle_bind_group: gpu::BindGroup<shaders::model::particle_render::BindGroup1Descriptor>,",
  "+}",
  "+",
  "+#[derive(Debug)]",
  "+pub struct ParticleModelDrawCommand {",
  "+  pub first_index: u32,",
  "+  pub index_count: u32,",
  "+  pub base_vertex: u32,",
  "+  pub primitive_instance: u32,",
  "+  pub debug_info: String,",
  " }",
  " ",
  " impl ModelStore {",
  "@@ -54,19 +64,29 @@ impl ModelStore {",
  "         });",
  " ",
  "     let bind_group = context.create_bind_group(shaders::model::render::BindGroup1Descriptor {",
  "-      color_texture: color,",
  "+      color_texture: color.clone(),",
  "       color_sampler: color_sampler.binding(),",
  "-      normal_texture: normal,",
  "-      roughness_metallic_texture: metallic_roughness,",
  "+      normal_texture: normal.clone(),",
  "+      roughness_metallic_texture: metallic_roughness.clone(),",
  "       primitive_material: materials.materials.binding(),",
  "       primitive_transform: primitive_transforms.binding(),",
  "     });",
  "+    let particle_bind_group =",
  "+      context.create_bind_group(shaders::model::particle_render::BindGroup1Descriptor {",
  "+        color_texture: color,",
  "+        color_sampler: color_sampler.binding(),",
  "+        normal_texture: normal,",
  "+        roughness_metallic_texture: metallic_roughness,",
  "+        primitive_material: materials.materials.binding(),",
  "+        primitive_transform: primitive_transforms.binding(),",
  "+      });",
  " ",
  "     // TODO: Initial size is not correct, primitive_ge.tangents is larger",
  "     Self {",
  "       primitives,",
  "       materials,",
  "       bind_group,",
  "+      particle_bind_group,",
  "     }",
  "   }",
  " ",
  "@@ -82,6 +102,38 @@ impl ModelStore {",
  "     }",
  "   }",
  " ",
  "+  pub fn insert_particle_model(",
  "+    &mut self,",
  "+    model: &Model,",
  "+    repository: &ModelRepository,",
  "+  ) -> Vec<ParticleModelDrawCommand> {",
  "+    let mut commands = Vec::new();",
  "+",
  "+    for mesh in model.mesh_iter() {",
  "+      for (primitive_index, primitive) in mesh.primitives.enumerate() {",
  "+        let material_id = self.materials.insert(&primitive.material(), repository);",
  "+        let debug_info = format!(\"Particle PrimitiveIndex: {}\", primitive_index);",
  "+        let primitive_instance = self.primitives.insert_primitive(",
  "+          &primitive,",
  "+          mesh.transform,",
  "+          material_id,",
  "+          model,",
  "+          debug_info.clone(),",
  "+        );",
  "+        let entry = self.primitives.entry(&primitive).unwrap();",
  "+        commands.push(ParticleModelDrawCommand {",
  "+          first_index: entry.index_start,",
  "+          index_count: entry.index_count,",
  "+          base_vertex: entry.vertex_start,",
  "+          primitive_instance,",
  "+          debug_info,",
  "+        });",
  "+      }",
  "+    }",
  "+",
  "+    commands",
  "+  }",
  "+",
  "   pub fn clear_instances(&mut self) {",
  "     self.primitives.clear_instances();",
  "     self.materials.clear();",
}, "\n")

local staged_diff = particle_system_diff .. "\n" .. sparse_fragment_diff .. "\n" .. shifted_old_line_diff
local unstaged_diff = ferrous_model_store_diff .. "\n" .. unstaged_shifted_old_line_diff

local ferrous_name_status_lines = {
  "M\tblue/apps/app-browser/src/physics/drop3d.rs",
  "M\tblue/engine/codegen/shaders/src/model/mod.rs",
  "M\tblue/engine/codegen/shaders/src/model/render.rs",
  "M\tblue/engine/codegen/shaders/src/physics/compute.rs",
  "M\tblue/engine/codegen/shaders/src/physics/compute_initialize.rs",
  "M\tblue/engine/codegen/shaders/src/physics/render.rs",
  "M\tblue/engine/codegen/shaders/src/types.rs",
  "M\tblue/engine/plugins/model/src/pbr.rs",
  "M\tblue/engine/plugins/model/src/pbr/model_render_system.rs",
  "M\tblue/engine/plugins/model/src/pbr/model_store.rs",
  "M\tblue/engine/plugins/model/src/pbr/primitive_mesh_store.rs",
  "M\tblue/engine/plugins/physics/src/lib.rs",
  "M\tblue/engine/plugins/physics/src/particle_events.rs",
  "M\tblue/engine/plugins/physics/src/particle_spawn.rs",
  "M\tblue/engine/plugins/physics/src/particle_system.rs",
  "M\tblue/engine/plugins/render/Cargo.toml",
  "M\tblue/engine/plugins/render/src/lib.rs",
  "M\tblue/engine/shaders/model/bindings.slang",
  "M\tblue/engine/shaders/model/lighting/material.slang",
  "M\tblue/engine/shaders/model/render.entry.slang",
  "M\tblue/engine/shaders/physics/particle.slang",
}

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "staged rust syntax" }, 0 end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    return {
      "M\tblue/engine/plugins/physics/src/particle_system.rs",
      "M\tsrc/fragment.rs",
      "M\tsrc/multi_hunk.rs",
    }, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    local lines = vim.deepcopy(ferrous_name_status_lines)
    lines[#lines + 1] = "M\tsrc/unstaged_multi_hunk.rs"
    return lines, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then
    return vim.split(unstaged_diff, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--cached" then
    return vim.split(staged_diff, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t--no-color\t--format=%H%x09%h%x09%cI%x09%s\t-20" then return {}, 0 end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  calls[#calls + 1] = command_key(command)
  local output, code = backend.systemlist(command)
  cb(output, code, "")
end

function backend.system()
  return "", 0
end

function backend.system_async(_, _, cb)
  cb({ code = 0, stdout = "", stderr = "", output = "" })
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

local function line_has_treesitter_highlight(buf, row)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if type(details.hl_group) == "string" and details.hl_group:sub(1, 1) == "@" then return true end
    if type(details.hl_group) == "table" then
      for _, group in ipairs(details.hl_group) do
        if tostring(group):sub(1, 1) == "@" then return true end
      end
    end
  end
  return false
end

local function line_has_line_highlight(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.line_hl_group == hl_group then return true end
  end
  return false
end

local function line_has_background_highlight(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.hl_group == hl_group and details.hl_eol == true then return true end
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

local function cell_at_text(buf, row, text)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local text_start = line:find(text, 1, true)
  assert_true(text_start ~= nil, ("missing text %q on row %d: %s"):format(text, row, line))
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_win_set_cursor(0, { row, text_start - 1 })
  vim.cmd("normal! zt")
  vim.cmd("redraw!")
  local screen_row = vim.fn.winline() - 1
  local screen_col = vim.fn.wincol() - 1
  local ok, cell = pcall(vim.api.nvim__inspect_cell, 1, screen_row, screen_col)
  assert_true(ok and type(cell) == "table" and type(cell[2]) == "table", "unable to inspect rendered screen cell")
  return cell[2]
end

local function normal_foreground()
  local normal = vim.api.nvim_get_hl(0, { name = "Normal", link = false })
  return normal.fg
end

local function assert_syntax_cell(buf, row, text)
  local cell = cell_at_text(buf, row, text)
  assert_true(
    cell.foreground and cell.foreground ~= normal_foreground(),
    ("%s rendered with normal foreground on row %d: %s"):format(text, row, vim.inspect(cell))
  )
end

local function assert_diff_background_cell(buf, row, text)
  local cell = cell_at_text(buf, row, text)
  assert_true(
    cell.background ~= nil,
    ("%s rendered without diff background on row %d: %s"):format(text, row, vim.inspect(cell))
  )
end

local function wait_for(condition, message)
  local ok = vim.wait(3000, condition, 10)
  if ok then return end
  if type(message) == "function" then
    error(message(), 2)
  end
  error(message, 2)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function normalized_path(value)
  return tostring(value or ""):gsub("\\", "/")
end

local function path_ends_with(path, suffix)
  local normalized = normalized_path(path)
  return normalized:sub(-#suffix) == suffix
end

local function diff_rows_for_file(file_suffix)
  local rows = {}
  for row_number, entry in pairs(diff_review._status.entries or {}) do
    local diff_line = entry and entry.diff_line
    if diff_line and path_ends_with(diff_line.file, file_suffix) then
      rows[#rows + 1] = {
        row = row_number,
        diff_line = diff_line,
      }
    end
  end
  table.sort(rows, function(left, right)
    return left.row < right.row
  end)
  return rows
end

local function ferrous_model_store_failures(buf)
  local file_suffix = "blue/engine/plugins/model/src/pbr/model_store.rs"
  local failures = {}
  local rows = diff_rows_for_file(file_suffix)
  if #rows < 60 then
    failures[#failures + 1] = "expected full model_store.rs diff rows, found " .. tostring(#rows)
    return failures
  end

  for _, row_info in ipairs(rows) do
    local diff_line = row_info.diff_line
    local row = row_info.row
    local code = diff_line.code or ""
    if code:match("%S") and not line_has_treesitter_highlight(buf, row) then
      failures[#failures + 1] = ("%d missing Tree-sitter syntax for %s%s [%s]"):format(
        row,
        diff_line.prefix or " ",
        code,
        line_highlights(buf, row)
      )
    end

    if diff_line.prefix == "+" then
      if not line_has_background_highlight(buf, row, "DiffReviewAddBg") then
        failures[#failures + 1] = ("%d missing add background for %s"):format(row, code)
      end
      if line_has_line_highlight(buf, row, "DiffReviewAddBg") then
        failures[#failures + 1] = ("%d uses add line_hl_group for %s"):format(row, code)
      end
    elseif diff_line.prefix == "-" then
      if not line_has_background_highlight(buf, row, "DiffReviewDeleteBg") then
        failures[#failures + 1] = ("%d missing delete background for %s"):format(row, code)
      end
      if line_has_line_highlight(buf, row, "DiffReviewDeleteBg") then
        failures[#failures + 1] = ("%d uses delete line_hl_group for %s"):format(row, code)
      end
    end
  end

  return failures
end

local function run()
  vim.fn.delete(root, "rf")
  assert_true(vim.fn.mkdir(root .. "/blue/engine/plugins/physics/src", "p") == 1, "mkdir failed")
  assert_true(vim.fn.mkdir(root .. "/blue/engine/plugins/model/src/pbr", "p") == 1, "mkdir model_store failed")
  assert_true(vim.fn.mkdir(root .. "/src", "p") == 1, "mkdir src failed")
  assert_true(vim.fn.writefile({
    "use {",
    "  crate::{",
    "    active_particle_render_mode, DeletePosition, DeleteRadius, ParticleEvent, ParticleSpawn,",
    "    ParticleStorage,",
    "  },",
    "  base::CameraComponent,",
    "  engine::{sys::gpu, *},",
    "  render::{GpuState, ParticleInstanceSources},",
    "  shaders::physics,",
    "};",
    "",
    "pub struct ParticleSystem {",
    "  compute: physics::compute::Pipelines,",
    "  compute_initialize: physics::compute_initialize::Pipelines,",
    "  pending_deletions: Vec<PendingDeletion>,",
    "  ready_deletions: Vec<PendingDeletion>,",
    "}",
    "",
    "impl ecs::System for ParticleSystem {",
    "  fn order(&self) -> u32 {",
    "    50",
    "  }",
    "",
    "  fn run(&mut self, engine: &Engine) {",
    "    self.process_spawns(engine);",
    "    self.process_events(engine);",
    "",
    "    let storage = engine.get::<ParticleStorage>();",
    "    let particle_count = storage.particle_count();",
    "    let instance_sources = engine.get::<ParticleInstanceSources>();",
    "    let render_mode = active_particle_render_mode(engine);",
    "    instance_sources.set(Some(storage.instance_source(render_mode.model_asset())));",
    "",
    "    if particle_count == 0 {",
    "      instance_sources.set(None);",
    "      return;",
    "    }",
    "",
    "    let state = engine.get::<GpuState>();",
    "    let context = &state.context;",
    "",
    "    let events = engine.get::<Events<ParticleEvent>>();",
    "",
    "    let solver_iteration_count = 10;",
    "    let fixed_dt = 1.0 / 60.0_f32;",
    "    let substep_count = 1u32;",
    "",
    "    storage.compute_locals.scene.clear();",
    "    storage.compute_locals.scene.extend(&[shaders::Scene {",
    "      count: particle_count,",
    "      dt: fixed_dt,",
    "      _padding: [0; 8],",
    "    }]);",
    "    storage.compute_locals.scene.upload(context);",
    "  }",
    "}",
  }, root .. "/blue/engine/plugins/physics/src/particle_system.rs") == 0, "writefile failed")
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
    "  pub fn new(context: &gpu::Context) -> Self {",
    "    let primitives =",
    "      PrimitiveMeshStore::new(context, constants::gpu::INITIAL_PRIMITIVE_GEOMETRY_BYTE_SIZE);",
    "",
    "    let materials = PbMaterials::new(context);",
    "",
    "    let color_sampler = context.create_sampler(gpu::SamplerDescriptor {",
    "      label: gpu::label!(\"color_sampler\"),",
    "      // We apply repeat in the shader for our texture atlas",
    "      address_mode_u: gpu::AddressMode::ClampToEdge,",
    "      address_mode_v: gpu::AddressMode::ClampToEdge,",
    "      address_mode_w: gpu::AddressMode::ClampToEdge,",
    "      mag_filter: gpu::FilterMode::Linear,",
    "      min_filter: gpu::FilterMode::Linear,",
    "      mipmap_filter: gpu::MipmapFilterMode::Linear,",
    "      anisotropy_clamp: 1,",
    "      ..Default::default()",
    "    });",
    "",
    "    let primitive_transforms = &primitives.primitive_instance_transforms;",
    "    let color = materials.color.create_view(&gpu::TextureViewDescriptor {",
    "      label: gpu::label!(\"color_texture\"),",
    "      ..Default::default()",
    "    });",
    "    let normal = materials.normal.create_view(&gpu::TextureViewDescriptor {",
    "      label: gpu::label!(\"normal_texture\"),",
    "      ..Default::default()",
    "    });",
    "    let metallic_roughness =",
    "      materials",
    "        .metallic_roughness",
    "        .create_view(&gpu::TextureViewDescriptor {",
    "          label: gpu::label!(\"roughness_metallic_texture\"),",
    "          ..Default::default()",
    "        });",
    "",
    "    let bind_group = context.create_bind_group(shaders::model::render::BindGroup1Descriptor {",
    "      color_texture: color.clone(),",
    "      color_sampler: color_sampler.binding(),",
    "      normal_texture: normal.clone(),",
    "      roughness_metallic_texture: metallic_roughness.clone(),",
    "      primitive_material: materials.materials.binding(),",
    "      primitive_transform: primitive_transforms.binding(),",
    "    });",
    "    let particle_bind_group =",
    "      context.create_bind_group(shaders::model::particle_render::BindGroup1Descriptor {",
    "        color_texture: color,",
    "        color_sampler: color_sampler.binding(),",
    "        normal_texture: normal,",
    "        roughness_metallic_texture: metallic_roughness,",
    "        primitive_material: materials.materials.binding(),",
    "        primitive_transform: primitive_transforms.binding(),",
    "      });",
    "",
    "    // TODO: Initial size is not correct, primitive_ge.tangents is larger",
    "    Self {",
    "      primitives,",
    "      materials,",
    "      bind_group,",
    "      particle_bind_group,",
    "    }",
    "  }",
    "",
    "  pub fn insert(&mut self, model: &Model, repository: &ModelRepository) {",
    "    for mesh in model.mesh_iter() {",
    "      for (i, primitive) in mesh.primitives.enumerate() {",
    "        let material_id = self.materials.insert(&primitive.material(), repository);",
    "        let debug = format!(\"PrimitiveIndex: {}\", i);",
    "        self",
    "          .primitives",
    "          .insert_primitive(&primitive, mesh.transform, material_id, model, debug);",
    "      }",
    "    }",
    "  }",
    "",
    "  pub fn insert_particle_model(",
    "    &mut self,",
    "    model: &Model,",
    "    repository: &ModelRepository,",
    "  ) -> Vec<ParticleModelDrawCommand> {",
    "    let mut commands = Vec::new();",
    "",
    "    for mesh in model.mesh_iter() {",
    "      for (primitive_index, primitive) in mesh.primitives.enumerate() {",
    "        let material_id = self.materials.insert(&primitive.material(), repository);",
    "        let debug_info = format!(\"Particle PrimitiveIndex: {}\", primitive_index);",
    "        let primitive_instance = self.primitives.insert_primitive(",
    "          &primitive,",
    "          mesh.transform,",
    "          material_id,",
    "          model,",
    "          debug_info.clone(),",
    "        );",
    "        let entry = self.primitives.entry(&primitive).unwrap();",
    "        commands.push(ParticleModelDrawCommand {",
    "          first_index: entry.index_start,",
    "          index_count: entry.index_count,",
    "          base_vertex: entry.vertex_start,",
    "          primitive_instance,",
    "          debug_info,",
    "        });",
    "      }",
    "    }",
    "",
    "    commands",
    "  }",
    "",
    "  pub fn clear_instances(&mut self) {",
    "    self.primitives.clear_instances();",
    "    self.materials.clear();",
    "  }",
    "",
    "  pub fn upload(&mut self, context: &gpu::Context) {",
    "    self.primitives.upload(context);",
    "    self.materials.upload(context);",
    "  }",
    "",
    "  fn clear(&mut self) {",
    "    self.materials.clear();",
    "    self.primitives.clear();",
    "  }",
    "}",
    "pub struct InstructionBundleBuilder {",
    "  pub primitive_to_entites: HashMap<Primitive, Vec<EntityIndex>>,",
    "}",
    "",
    "impl InstructionBundleBuilder {",
    "  pub fn new() -> Self {",
    "    Self {",
    "      primitive_to_entites: Default::default(),",
    "    }",
    "  }",
    "",
    "  pub fn insert(&mut self, entity: EntityIndex, model: &Model) {",
    "    for mesh in model.mesh_iter() {",
    "      for primitive in mesh.primitives {",
    "        let entry = self.primitive_to_entites.entry(primitive).or_default();",
    "        entry.push(entity);",
    "      }",
    "    }",
    "  }",
    "}",
  }, root .. "/blue/engine/plugins/model/src/pbr/model_store.rs") == 0, "write model_store failed")
  assert_true(vim.fn.writefile({
    "pub struct CameraComponent {",
    "  value: u32,",
    "}",
    "",
    "impl Builder {",
    "  fn build(&self, value: u32) -> Widget {",
    "    if value > 10 {",
    "      Widget::new(CameraComponent { value: value + 1 })",
    "    } else {",
    "      Widget::empty()",
    "    }",
    "  }",
    "}",
  }, root .. "/src/fragment.rs") == 0, "write fragment failed")
  assert_true(vim.fn.writefile({
    "pub struct CameraComponent {",
    "  value: u32,",
    "}",
    "",
    "impl Builder {",
    "  fn setup(&self) {",
    "    let keep = true;",
    "  }",
    "",
    "  fn build(&self, value: u32) -> Widget {",
    "    let before = value + 1;",
    "    let middle = before + 1;",
    "    let after = middle + 1;",
    "    if after > 10 {",
    "      Widget::new(CameraComponent { value: value + 1 })",
    "    } else {",
    "      Widget::empty()",
    "    }",
    "  }",
    "}",
  }, root .. "/src/multi_hunk.rs") == 0, "write multi_hunk failed")
  assert_true(vim.fn.writefile({
    "pub struct CameraComponent {",
    "  value: u32,",
    "}",
    "",
    "impl Builder {",
    "  fn setup(&self) {",
    "    let keep = true;",
    "  }",
    "",
    "  fn build(&self, value: u32) -> Widget {",
    "    let before = value + 1;",
    "    let middle = before + 1;",
    "    let after = middle + 1;",
    "    if after > 10 {",
    "      Widget::new(CameraComponent { value: value + 1 })",
    "    } else {",
    "      Widget::empty()",
    "    }",
    "  }",
    "}",
  }, root .. "/src/unstaged_multi_hunk.rs") == 0, "write unstaged_multi_hunk failed")

  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  assert_true(diff_review._git_backend == backend, "git backend was not installed")
  diff_review.setup({ about_auto_generate = false })
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  wait_for(function()
    return buffer_contains(buf, "particle_system.rs +16 -17")
  end, "status did not render staged file\n" .. buffer_dump(buf)
    .. "\n\nmessages:\n" .. vim.fn.execute("messages")
    .. "\n\ncalls:\n" .. table.concat(calls, "\n"))

  trigger_normal_mapping("<Tab>", find_row(buf, "particle_system.rs"))
  wait_for(function()
    return buffer_contains(buf, "let camera = engine.world().execute_query")
  end, "staged hunk did not render\n" .. buffer_dump(buf))
  local camera_row = find_row(buf, "let camera = engine.world().execute_query")
  wait_for(function()
    return line_has_highlight(buf, camera_row, "@keyword")
  end, "deleted staged Rust row did not get keyword syntax highlight: " .. line_highlights(buf, camera_row))
  assert_true(
    line_has_highlight(buf, camera_row, "@type"),
    "deleted staged Rust row did not get type syntax highlight: " .. line_highlights(buf, camera_row)
  )

  trigger_normal_mapping("<Tab>", find_row(buf, "particle_system.rs"))
  trigger_normal_mapping("<Tab>", find_row(buf, "fragment.rs"))
  wait_for(function()
    return buffer_contains(buf, "Widget::new(CameraComponent")
  end, "sparse staged Rust hunk did not render\n" .. buffer_dump(buf))
  local fragment_row = find_row(buf, "Widget::new(CameraComponent")
  wait_for(function()
    local row = find_row(buf, "Widget::new(CameraComponent")
    return line_has_highlight(buf, row, "@type")
  end, "sparse staged Rust row did not get type syntax highlight: " .. line_highlights(buf, fragment_row))

  trigger_normal_mapping("<Tab>", find_row(buf, "fragment.rs"))
  trigger_normal_mapping("<Tab>", find_row(buf, "multi_hunk.rs"))
  wait_for(function()
    return buffer_contains(buf, "let removed_a = CameraComponent")
  end, "multi-hunk staged Rust file did not render first hunk\n" .. buffer_dump(buf))
  wait_for(function()
    return buffer_contains(buf, "Widget::new(CameraComponent { value })")
  end, "multi-hunk staged Rust file did not render shifted old row\n" .. buffer_dump(buf))
  local shifted_row = find_row(buf, "Widget::new(CameraComponent { value })")
  wait_for(function()
    local row = find_row(buf, "Widget::new(CameraComponent { value })")
    return line_has_highlight(buf, row, "@type")
  end, "shifted old-line staged Rust row did not get type syntax highlight: " .. line_highlights(buf, shifted_row))

  trigger_normal_mapping("<Tab>", find_row(buf, "multi_hunk.rs"))
  trigger_normal_mapping("<Tab>", find_row(buf, "model_store.rs"))
  wait_for(function()
    return buffer_contains(buf, "pub particle_bind_group")
      and buffer_contains(buf, "context.create_bind_group(shaders::model::particle_render::BindGroup1Descriptor")
      and buffer_contains(buf, "commands.push(ParticleModelDrawCommand")
  end, "Ferrous model_store.rs diff did not render\n" .. buffer_dump(buf))
  wait_for(function()
    return #ferrous_model_store_failures(buf) == 0
  end, function()
    return "Ferrous model_store.rs syntax/background regression:\n"
      .. table.concat(ferrous_model_store_failures(buf), "\n")
      .. "\n\n"
      .. buffer_dump(buf)
  end)

  local particle_bind_group_row = find_row(buf, "pub particle_bind_group")
  assert_true(
    line_has_highlight(buf, particle_bind_group_row, "@type"),
    "Ferrous particle_bind_group row did not get type syntax highlight: " .. line_highlights(buf, particle_bind_group_row)
  )
  assert_syntax_cell(buf, particle_bind_group_row, "pub")
  assert_syntax_cell(buf, particle_bind_group_row, "BindGroup")
  assert_diff_background_cell(buf, particle_bind_group_row, "pub")
  local particle_bind_descriptor_row = find_row(buf, "context.create_bind_group(shaders::model::particle_render")
  assert_true(
    line_has_highlight(buf, particle_bind_descriptor_row, "@function"),
    "Ferrous create_bind_group row did not get function syntax highlight: " .. line_highlights(buf, particle_bind_descriptor_row)
  )
  assert_true(
    line_has_highlight(buf, particle_bind_descriptor_row, "@type"),
    "Ferrous create_bind_group row did not get type syntax highlight: " .. line_highlights(buf, particle_bind_descriptor_row)
  )
  assert_syntax_cell(buf, particle_bind_descriptor_row, "create_bind_group")
  assert_syntax_cell(buf, particle_bind_descriptor_row, "BindGroup1Descriptor")
  assert_diff_background_cell(buf, particle_bind_descriptor_row, "create_bind_group")
  local clone_row = find_row(buf, "color_texture: color.clone()")
  assert_true(
    line_has_highlight(buf, clone_row, "@function"),
    "Ferrous clone row did not get function syntax highlight: " .. line_highlights(buf, clone_row)
  )
  assert_syntax_cell(buf, clone_row, "clone")
  assert_diff_background_cell(buf, clone_row, "clone")

  trigger_normal_mapping("<Tab>", find_row(buf, "model_store.rs"))
  trigger_normal_mapping("<Tab>", find_row(buf, "unstaged_multi_hunk.rs"))
  wait_for(function()
    return buffer_contains(buf, "Widget::new(CameraComponent { value })")
  end, "multi-hunk unstaged Rust file did not render shifted old row\n" .. buffer_dump(buf))
  local unstaged_shifted_row = find_row(buf, "Widget::new(CameraComponent { value })")
  wait_for(function()
    local row = find_row(buf, "Widget::new(CameraComponent { value })")
    return line_has_highlight(buf, row, "@type")
  end, "shifted old-line unstaged Rust row did not get type syntax highlight: " .. line_highlights(buf, unstaged_shifted_row))
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
