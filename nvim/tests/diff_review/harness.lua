vim.loader.enable(false)

local diff_review = require("diff_review")
local builder = require("diff_review.harness.builder")
local client = require("diff_review.harness.client")
local controller = require("diff_review.views.harness.controller")
local raw_interaction_renderer = require("diff_review.render.harness.interaction_tree")
local session = require("diff_review.session")
local interaction_state = require("diff_review.views.harness.interaction_state")
local harness_snapshot = require("diff_review.views.harness.snapshot")

local function normalize_interaction(interaction)
  if interaction.node_list then return interaction end
  local node_list = {}
  if interaction.thought or interaction.active or interaction.response then
    node_list[#node_list + 1] = {
      kind = "main_segment",
      segment = {
        id = interaction.id .. ":segment:1",
        state = interaction.state == "running" and "running" or "complete",
        started_at_ms = interaction.created_at_ms or 0,
        completed_at_ms = interaction.completed_at_ms,
        duration_ms = interaction.duration_ms or 0,
        token_count = interaction.token_count,
        spawned_agent_count = #(interaction.agent or {}),
        thought = interaction.thought or {},
        active = interaction.active,
        response = interaction.response,
      },
    }
  end
  for _, steering in ipairs(interaction.steering_input or {}) do
    node_list[#node_list + 1] = { kind = "steering_prompt", prompt = steering }
  end
  interaction.node_list = node_list
  interaction.thought = nil
  interaction.active = nil
  interaction.response = nil
  interaction.steering_input = nil
  return interaction
end

local function normalize_entry(entry)
  if entry.interaction and entry.kind == "interaction" then
    entry.interaction = normalize_interaction(entry.interaction)
    entry.agent_by_id = entry.agent_by_id or {}
    for _, agent in ipairs(entry.agent or {}) do
      local run_id = agent.run and agent.run.id or agent.id
      entry.agent_by_id[run_id] = agent
      entry.interaction.node_list[#entry.interaction.node_list + 1] = {
        kind = "agent_reference",
        agent = {
          id = entry.interaction.id .. ":agent:" .. run_id,
          agent_run_id = run_id,
          created_at_ms = agent.run and agent.run.created_at_ms or 0,
        },
      }
      for _, node in ipairs(entry.interaction.node_list) do
        if node.kind == "main_segment" then
          node.segment.spawned_agent_count = (node.segment.spawned_agent_count or 0) + 1
          break
        end
      end
    end
    entry.agent = nil
  elseif entry.kind == "agent_lifecycle" then
    for _, child_interaction in ipairs(entry.interaction or {}) do normalize_interaction(child_interaction) end
  elseif entry.kind == "plan_execution" then
    for _, execution_interaction in ipairs(entry.interaction or {}) do normalize_interaction(execution_interaction) end
  elseif entry.id and entry.prompt then
    normalize_interaction(entry)
  end
  return entry
end

local interaction_renderer = {
  build = function(entry_list, options)
    local normalized = vim.deepcopy(entry_list)
    for _, entry in ipairs(normalized) do normalize_entry(entry) end
    return raw_interaction_renderer.build(normalized, options)
  end,
  foldtext = raw_interaction_renderer.foldtext,
}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equals(actual, expected, message)
  if actual ~= expected then error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual), 2) end
end

local function has_buffer_keymap(buf, mode, lhs)
  for _, keymap in ipairs(vim.api.nvim_buf_get_keymap(buf, mode)) do
    if keymap.lhs == lhs then return true end
  end
  return false
end

local function line_number(line_list, expected)
  for index, line in ipairs(line_list) do
    if line == expected then return index end
  end
  return nil
end

local function interaction_response(interaction)
  for _, node in ipairs(interaction.node_list or {}) do
    if node.segment and type(node.segment.response) == "string" then return node.segment.response end
  end
  return nil
end

local test_root = vim.fn.tempname()
local crate_root = vim.fs.joinpath(test_root, "crate")
local plan_path = vim.fs.joinpath(test_root, "working.md")
vim.fn.mkdir(vim.fs.joinpath(crate_root, "src"), "p")
vim.fn.writefile({ "[package]", 'name = "diff-review-harness"', 'version = "0.1.0"' }, vim.fs.joinpath(crate_root, "Cargo.toml"))
vim.fn.writefile({ "fn main() {}" }, vim.fs.joinpath(crate_root, "src", "main.rs"))
vim.fn.writefile({ "# Plan", "", "1. Review the implementation." }, plan_path)

local active_session = {
  id = "session-one",
  name = "Harness test",
  workspace = vim.fn.getcwd(),
  backend = "mock",
  native_compact = true,
  context_usage = { used = 275000, size = 353000, remaining_percent = 22 },
  backend_session_id = "provider-one",
  model = "mock-model",
  provider_label = "Mock CLI",
  resolved_model = "mock-model-resolved",
  effort = "low",
  execution_mode = "read",
  native_fork = false,
}

local interaction = {
  id = "interaction-one",
  session_id = active_session.id,
  ordinal = 1,
  prompt = "Change the greeting",
  state = "complete",
  checkpoint_before = "before",
  checkpoint_after = "after",
  diff_text = table.concat({
    "diff --git a/demo.lua b/demo.lua",
    "--- a/demo.lua",
    "+++ b/demo.lua",
    "@@ -1,1 +1,1 @@",
    "-return 'before'",
    "+return 'after'",
  }, "\n"),
  comment = {
    {
      id = "comment-one",
      interaction_id = "interaction-one",
      file_path = "demo.lua",
      new_line = 1,
      body = "Persisted review note",
      created_at_ms = 1,
    },
  },
}

local request_by_method = {}
local goal_continue_count = 0
local prompt_count = 0
local steer_should_fail = false
local steered_text_list = {}
local launched_binary = nil
local active_plan = nil
local active_elicitation = nil
local prompt_history = { "most recent prompt", "older prompt" }
local retract_request = nil

local function planning_question_set()
  return {
    id = "question-set",
    questions = {
      {
        id = "question-one",
        header = "Migration",
        question = "Which migration strategy should the plan use?",
        options = {
          { label = "Staged", description = "Support both formats temporarily." },
          { label = "Immediate", description = "Remove the old format now." },
          { label = "Compatible", description = "Keep a compatibility layer." },
        },
        allow_freeform = true,
      },
      {
        id = "question-two",
        header = "Storage",
        question = "Which storage boundary should own the state?",
        options = {
          { label = "SQLite", description = "Persist it through the broker." },
          { label = "Memory", description = "Keep it editor-local." },
        },
        allow_freeform = true,
      },
      {
        id = "question-three",
        header = "Testing",
        question = "Which verification depth should the plan require?",
        options = {
          { label = "Focused", description = "Run focused coverage." },
          { label = "Full", description = "Run the complete suite." },
        },
        allow_freeform = true,
      },
    },
  }
end

local function emit(options, message)
  vim.schedule(function() options.stdout(nil, vim.json.encode(message) .. "\n") end)
end

local function mock_segment(interaction_id, state, active, thought, response, duration_ms)
  return {
    kind = "main_segment",
    segment = {
      id = interaction_id .. ":segment:1",
      state = state,
      started_at_ms = 0,
      completed_at_ms = state == "complete" and (duration_ms or 0) or nil,
      duration_ms = duration_ms or 0,
      token_count = state == "complete" and 2700 or nil,
      spawned_agent_count = 0,
      thought = thought or {},
      active = active,
      response = response,
    },
  }
end

local function fake_launcher(command, options, _)
  launched_binary = command[1]
  local process = {}
  function process:write(payload)
    local request = vim.json.decode(vim.trim(payload))
    request_by_method[request.method] = request
    if request.method == "initialize" then
      active_session.backend = request.params.backend.kind
      vim.defer_fn(function()
        emit(options, { id = request.id, result = {
          session = active_session,
          interaction = {},
          capability = {
            native_fork = false,
            native_compact = true,
            native_steer = true,
            native_turn_rollback = true,
            model_selection = true,
            effort_selection = true,
            fast_mode = true,
          },
          no_checkpoint = false,
          approval = {},
          prompt_history = prompt_history,
        } })
      end, 80)
    elseif request.method == "prompt.submit" then
      if request.params.text == "/goal fail" then
        emit(options, { id = request.id, error = { message = "synthetic goal failure" } })
        return
      end
      if request.params.text == "retract me" then
        retract_request = request
        emit(options, { event = "backend_event", payload = {
          kind = "timeline_interaction_started",
          data = {
            id = "retracted-interaction",
            session_id = active_session.id,
            ordinal = prompt_count + 1,
            prompt = request.params.text,
            state = "running",
            node_list = {},
          },
        } })
        return
      end
      prompt_count = prompt_count + 1
      local interaction_id = "live-interaction-" .. prompt_count
      local thought_id = interaction_id .. ":thought:1"
      local completed_tool = nil
      if request.params.text == "hello harness" then
        local output = table.concat({ "# Rendering", "line two", "line three", "line four", "last line" }, "\n")
        emit(options, { event = "backend_event", payload = {
          kind = "timeline_node_updated",
          data = {
            interaction_id = interaction_id,
            node = mock_segment(interaction_id, "running", {
              interaction_id = interaction_id,
              thought_id = thought_id,
              text = "Working",
              synthetic = true,
              tool_count = 1,
              failed_count = 0,
              revision = 1,
            }),
          },
        } })
        completed_tool = {
          id = "command-one",
          kind = "command",
          title = "Get-Content README.md",
          status = "completed",
          output = output,
        }
        emit(options, { event = "backend_event", payload = {
          kind = "timeline_node_updated",
          data = {
            interaction_id = interaction_id,
            node = mock_segment(interaction_id, "running", nil, { {
              id = thought_id,
              text = "Working",
              synthetic = true,
              tool = { completed_tool },
              started_at_ms = 0,
              completed_at_ms = 1000,
            } }),
          },
        } })
      end
      emit(options, { event = "backend_event", payload = {
        kind = "timeline_node_updated",
        data = {
          interaction_id = interaction_id,
          node = mock_segment(interaction_id, "running", {
            interaction_id = interaction_id,
            thought_id = interaction_id .. ":thought:2",
            text = "Mock ",
            synthetic = false,
            tool_count = 0,
            failed_count = 0,
            revision = 2,
          }, completed_tool and { {
            id = thought_id,
            text = "Working",
            synthetic = true,
            tool = { completed_tool },
            started_at_ms = 0,
            completed_at_ms = 1000,
          } } or {}),
        },
      } })
      emit(options, { event = "backend_event", payload = {
        kind = "timeline_node_updated",
        data = {
          interaction_id = interaction_id,
          node = mock_segment(interaction_id, "running", {
            interaction_id = interaction_id,
            thought_id = interaction_id .. ":thought:2",
            text = "Mock reply",
            synthetic = false,
            tool_count = 0,
            failed_count = 0,
            revision = 3,
          }, completed_tool and { {
            id = thought_id,
            text = "Working",
            synthetic = true,
            tool = { completed_tool },
            started_at_ms = 0,
            completed_at_ms = 1000,
          } } or {}),
        },
      } })
      if request.params.text == "/plan choose a migration" then
        local question_set = planning_question_set()
        active_plan = {
          id = "plan-question",
          session_id = active_session.id,
          title = "Choose a migration",
          state = "awaiting_input",
          working_path = "",
          elicitation = {
            revision = 1,
            question_set = question_set,
            answer = {},
            current_index = 0,
            clarification_active = false,
          },
        }
        active_elicitation = {
          owner = "plan",
          plan_id = active_plan.id,
          elicitation = active_plan.elicitation,
        }
        emit(options, { event = "plan_question", payload = {
          plan = active_plan,
          lifecycle = { id = "question-lifecycle", kind = "question_asked" },
          question = question_set,
        } })
      elseif request.params.text == "ask choices" then
        active_elicitation = {
          owner = "interaction",
          interaction_id = interaction_id,
          elicitation = {
            revision = 1,
            question_set = planning_question_set(),
            answer = {},
            current_index = 0,
            clarification_active = false,
          },
        }
        emit(options, { event = "question", payload = active_elicitation })
      elseif request.params.text == "replace choices" and active_elicitation then
        local replacement = planning_question_set()
        replacement.id = "question-set-replacement"
        replacement.questions[1].question = "Which revised migration strategy should the plan use?"
        active_elicitation.elicitation.question_set = replacement
        active_elicitation.elicitation.revision = active_elicitation.elicitation.revision + 1
        emit(options, { event = "question", payload = active_elicitation })
      elseif request.params.text:match("^/plan ") then
        active_plan = {
          id = "plan-one",
          session_id = active_session.id,
          title = "Inspect the feature",
          state = "awaiting_review",
          working_path = plan_path,
          review_digest = "digest",
        }
        emit(options, { event = "plan_created", payload = {
          plan = active_plan,
          lifecycle = { id = "lifecycle-one", kind = "created" },
          content = "# Plan\n\n1. Review the implementation.",
        } })
      end
      if request.params.text == "hello harness" then
        vim.defer_fn(function()
          local completed = {
            id = interaction_id,
            session_id = active_session.id,
            ordinal = prompt_count,
            prompt = request.params.text,
            state = "complete",
            node_list = { mock_segment(interaction_id, "complete", nil, completed_tool and { {
              id = thought_id,
              text = "Working",
              synthetic = true,
              tool = { completed_tool },
              started_at_ms = 0,
              completed_at_ms = 1000,
            } } or {}, "Mock reply", 2000) },
            duration_ms = 2000,
            token_count = 2700,
          }
          emit(options, { event = "interaction_complete", payload = completed })
          emit(options, { id = request.id, result = {
            interaction = completed,
            session = active_session,
            capability = { native_fork = false, native_steer = true, native_turn_rollback = true },
          } })
        end, 1200)
      else
        local completed = {
          id = interaction_id,
          session_id = active_session.id,
          ordinal = prompt_count,
          prompt = request.params.text,
          state = "complete",
          node_list = { mock_segment(interaction_id, "complete", nil, {}, "Mock reply", 2000) },
          awaiting_input = request.params.text == "/plan choose a migration" or request.params.text == "ask choices",
          duration_ms = 2000,
          token_count = 2700,
        }
        emit(options, { event = "interaction_complete", payload = completed })
        emit(options, { id = request.id, result = {
          interaction = completed,
          session = active_session,
          capability = { native_fork = false, native_steer = true, native_turn_rollback = true },
        } })
      end
    elseif request.method == "history.record" then
      table.insert(prompt_history, 1, request.params.text)
      while #prompt_history > 100 do table.remove(prompt_history) end
      emit(options, { id = request.id, result = prompt_history })
    elseif request.method == "question.answer" or request.method == "question.skip" then
      local response = request.method == "question.skip" and { kind = "skipped" } or request.params.response
      active_plan.elicitation.answer[#active_plan.elicitation.answer + 1] = {
        question_id = request.params.question_id,
        response = response,
      }
      active_plan.elicitation.current_index = active_plan.elicitation.current_index + 1
      active_plan.elicitation.clarification_active = false
      active_elicitation.elicitation = active_plan.elicitation
      emit(options, { event = "question_updated", payload = active_elicitation })
      emit(options, { id = request.id, result = {
        session = active_session,
        active_plan = active_plan,
        active_elicitation = active_elicitation,
      } })
    elseif request.method == "question.ask" then
      active_plan.elicitation.clarification_active = true
      active_elicitation.elicitation = active_plan.elicitation
      emit(options, { event = "question_updated", payload = active_elicitation })
      emit(options, { id = request.id, result = { session = active_session } })
    elseif request.method == "question.continue" then
      active_plan.elicitation = nil
      active_elicitation = nil
      active_plan.state = "awaiting_review"
      active_plan.working_path = plan_path
      active_plan.review_digest = "digest"
      emit(options, { event = "plan_question_answered", payload = {
        plan = active_plan,
        lifecycle = { id = "answer-lifecycle", kind = "question_answered" },
      } })
      emit(options, { event = "plan_created", payload = {
        plan = active_plan,
        lifecycle = { id = "question-plan-created", kind = "created" },
        content = "# Plan\n\n1. Review the implementation.",
      } })
      emit(options, { id = request.id, result = { session = active_session } })
    elseif request.method == "interaction.list" then
      emit(options, { id = request.id, result = { interaction } })
    elseif request.method == "session.list" then
      emit(options, { id = request.id, result = { active_session } })
    elseif request.method == "session.preview" then
      emit(options, { id = request.id, result = {
        session = active_session,
        interaction = session.harness.interaction,
        timeline = session.harness.timeline,
        agent = session.harness.agent or { definition = {}, run = {}, turn = {} },
      } })
    elseif request.method == "session.rename" then
      active_session.name = vim.trim(request.params.name or "")
      emit(options, { id = request.id, result = active_session })
    elseif request.method == "backend.models" then
      emit(options, { id = request.id, result = { { id = "mock-model", label = "Mock model", effort = { "low" } } } })
    elseif request.method == "session.execution_mode" then
      active_session.execution_mode = request.params.mode
      emit(options, { event = "execution_mode_changed", payload = active_session })
      emit(options, { id = request.id, result = active_session })
    elseif request.method == "test.approval.open" then
      emit(options, { event = "backend_event", payload = {
        kind = "approval_requested",
        data = {
          id = "approval-test",
          title = "Run command",
          detail = "rg TODO",
          profile = "Read",
          choice_list = { { id = "deny_once", label = "Deny once" } },
        },
      } })
      emit(options, { id = request.id, result = { opened = true } })
    elseif request.method == "test.approval.cancel" then
      emit(options, { event = "backend_event", payload = {
        kind = "approval_cancelled",
        data = { id = "approval-test" },
      } })
      emit(options, { id = request.id, result = { cancelled = true } })
    elseif request.method == "session.configure" then
      active_session = vim.tbl_extend("force", active_session, request.params)
      emit(options, { event = "session_configured", payload = active_session })
      emit(options, { id = request.id, result = active_session })
    elseif request.method == "session.compact" then
      active_session.context_usage = { used = 40000, size = 353000, remaining_percent = 92 }
      emit(options, { event = "context_compacted", payload = {
        session = active_session,
        interaction = session.harness.interaction,
        capability = {
          native_fork = false,
          native_compact = true,
          native_steer = true,
          native_turn_rollback = true,
          model_selection = true,
          effort_selection = true,
          fast_mode = true,
        },
      } })
      emit(options, { id = request.id, result = {
        session = active_session,
        interaction = session.harness.interaction,
        capability = {
          native_fork = false,
          native_compact = true,
          native_steer = true,
          native_turn_rollback = true,
          model_selection = true,
          effort_selection = true,
          fast_mode = true,
        },
      } })
    elseif request.method == "turn.steer" then
      steered_text_list[#steered_text_list + 1] = request.params.text
      vim.defer_fn(function()
        if steer_should_fail then
          emit(options, { id = request.id, error = { message = "active turn already completed" } })
        else
          local active_interaction = session.harness.interaction[#session.harness.interaction]
          emit(options, { event = "backend_event", payload = {
            kind = "timeline_node_updated",
            data = {
              interaction_id = active_interaction.id,
              node = {
                kind = "steering_prompt",
                prompt = {
                  id = active_interaction.id .. ":steering:1",
                  text = request.params.text,
                  created_at_ms = 1,
                },
              },
            },
          } })
          emit(options, { id = request.id, result = { steered = true } })
        end
      end, 80)
    elseif request.method == "turn.cancel" then
      emit(options, { id = request.id, result = { cancel_requested = true } })
      if request.params.restore_prompt_if_no_output and retract_request then
        emit(options, { event = "backend_event", payload = {
          kind = "timeline_interaction_retracted",
          data = { interaction_id = "retracted-interaction", prompt = retract_request.params.text },
        } })
        emit(options, { id = retract_request.id, error = {
          code = "turn_retracted",
          message = "Output-free turn retracted",
          data = { prompt = retract_request.params.text },
        } })
        retract_request = nil
      end
    elseif request.method == "session.new" then
      active_session = vim.tbl_extend("force", active_session, { id = "session-two", execution_mode = "read" })
      emit(options, { event = "session_changed", payload = { session = active_session, interaction = {} } })
      emit(options, { id = request.id, result = {
        session = active_session,
        interaction = {},
        prompt_history = prompt_history,
      } })
    elseif request.method == "session.fork" then
      active_session = vim.tbl_extend("force", active_session, {
        id = "session-fork",
        native_fork = true,
        execution_mode = "read",
      })
      emit(options, { event = "session_changed", payload = { session = active_session } })
      emit(options, { id = request.id, result = {
        session = active_session,
        interaction = { interaction },
        capability = { native_fork = true },
      } })
    elseif request.method == "goal.continue" then
      goal_continue_count = goal_continue_count + 1
      if goal_continue_count == 2 then
        emit(options, { event = "goal_changed", payload = { objective = "finish", state = "stalled" } })
      end
      emit(options, { id = request.id, result = { session = active_session } })
    elseif request.method == "state.get" then
      emit(options, { id = request.id, result = {
        session = active_session,
        interaction = session.harness.interaction,
        capability = {
          native_fork = false,
          native_compact = true,
          native_steer = true,
          native_turn_rollback = true,
          model_selection = true,
          effort_selection = true,
          fast_mode = true,
        },
        no_checkpoint = false,
        goal = { objective = "fail", state = "paused" },
        active_plan = active_plan,
        active_elicitation = active_elicitation,
        approval = {},
        agent = { definition = {}, run = {}, turn = {} },
        artifact = active_plan and active_plan.working_path ~= "" and { active_plan } or {},
        prompt_history = prompt_history,
      } })
    elseif request.method == "plan.activate" then
      emit(options, { id = request.id, result = active_plan })
    elseif request.method == "plan.accept" then
      emit(options, { event = "goal_changed", payload = { objective = "Complete the plan", state = "complete" } })
      emit(options, { id = request.id, result = { session = active_session } })
    elseif request.method == "shutdown" then
      emit(options, { id = request.id, result = { shutdown = true } })
    else
      emit(options, { id = request.id, result = active_session })
    end
  end
  function process:kill() end
  return process
end

local ok, failure = pcall(function()
  local default_config = require("diff_review.infra.config").setup()
  assert_equals(default_config.harness.backend, "codex",
    "an empty setup should retain the complete default configuration")
  diff_review.setup({
    harness = { backend = "mock" },
    keymaps = { status = { open = { "x" } } },
  })
  local backend_preference = require("diff_review.harness.backend_preference")
  local backend_preference_path = vim.fs.joinpath(test_root, "backend.json")
  backend_preference._set_path_for_test(backend_preference_path)
  local preference_saved, preference_error = backend_preference.save("copilot")
  assert_true(preference_saved, preference_error)
  assert_equals(backend_preference.load(require("diff_review.infra.config").options.harness.backends, "codex"),
    "copilot", "backend preferences should round-trip through the Harness data file")
  assert_equals(require("diff_review.infra.config").options.harness.backend, "mock",
    "an explicit setup backend should remain authoritative over persisted defaults")
  assert_equals(#require("diff_review.infra.config").defaults.harness.backends.copilot.command, 0,
    "default Copilot backend should launch the SDK-managed CLI")
  assert_equals(
    require("diff_review.harness.backends.copilot")
      .descriptor(require("diff_review.infra.config").defaults.harness).kind,
    "copilot",
    "Copilot launch descriptors should select the native Rust backend"
  )
  assert_equals(require("diff_review.infra.config").defaults.harness.backend, "codex",
    "Harness should default to the direct Codex backend")
  assert_equals(#require("diff_review.infra.config").options.keymaps.status.open, 1, "configured key lists should replace defaults atomically")
  assert_true(vim.uv.fs_stat(builder.manifest_path()) ~= nil, "Harness builder should resolve its crate from the plugin runtime")

  local question_model = require("diff_review.views.harness.plan_question.model")
  local question_entries = question_model.entries({
    options = {
      { label = "Staged", description = "Support both formats." },
      { label = "Immediate", description = "Replace immediately." },
      { label = "Compatible", description = "Keep the compatibility layer." },
    },
    allow_freeform = true,
  }, require("diff_review.infra.config").options.picker.choice_keys)
  assert_equals(question_entries[1].key, "n", "question choices should start with the configured quick keys")
  assert_equals(question_entries[4].key, "o", "Other should always use o")
  assert_equals(question_entries[5].key, "a", "Ask should always use a regardless of option count")
  local question_render = interaction_renderer.build({ {
    kind = "plan_lifecycle",
    id = "question-lifecycle",
    lifecycle = {
      kind = "question_asked",
      question = { questions = { {
        header = "Migration",
        question = "Which migration should the plan use?",
        options = { { label = "Staged", description = "Support both formats temporarily." } },
      } } },
    },
  } })
  assert_true(vim.tbl_contains(question_render.lines, "▸ Planning paused for feedback"),
    "planning questions should render as a durable paused state")
  assert_true(vim.tbl_contains(question_render.lines, "Which migration should the plan use?"),
    "planning questions should remain visible without expansion")
  assert_true(vim.tbl_contains(question_render.lines, "    ○ Staged — Support both formats temporarily."),
    "planning question choices should remain visible in the timeline")
  local withdrawn_question_render = interaction_renderer.build({ {
    kind = "plan_lifecycle",
    id = "withdrawn-question-lifecycle",
    lifecycle = {
      kind = "question_withdrawn",
      answer = "Repository policy determines the migration.",
    },
  } })
  assert_true(vim.tbl_contains(withdrawn_question_render.lines,
    "▸ Question withdrawn: Repository policy determines the migration."),
    "withdrawn planning questions should retain their evidence in the durable timeline")
  local agent_render = interaction_renderer.build({ {
    kind = "agent_lifecycle",
    id = "agent-run",
    run = {
      id = "agent-run",
      definition = "explorer",
      nickname = "Bevy scout",
      task = "Inspect Bevy ownership",
      status = "running",
      created_at_ms = 1000,
      updated_at_ms = 2000,
    },
    interaction = { {
      id = "child-turn",
      state = "running",
      thought = { {
        id = "child-thought",
        text = "Tracing Bevy ownership.",
        tool = { { id = "tool-1", kind = "command", title = "Ran rg Bevy", status = "completed" } },
      } },
    } },
  } }, {
    expanded = { ["agent_lifecycle:agent-run"] = true },
    now_ms = 3000,
  })
  assert_equals(agent_render.lines[1], "▸ Agent Bevy scout running for 2s, 1 tool called",
    "parent timelines should render child runtime and tool activity")
  assert_equals(agent_render.lines[2], "  ↳ Tracing Bevy ownership.",
    "expanded child lifecycle nodes should expose the child timeline")
  local parent_with_agent = interaction_renderer.build({ {
    kind = "interaction",
    interaction = {
      id = "parent-turn",
      prompt = "/agent explorer inspect Bevy",
      state = "running",
      duration_ms = 1000,
      thought = { {
        id = "delegation-thought",
        text = "Delegating the repository map.",
        tool = {},
      } },
    },
    agent = { {
      kind = "agent_lifecycle",
      id = "agent-run",
      run = {
        id = "agent-run",
        definition = "explorer",
        nickname = "Bevy scout",
        status = "running",
        created_at_ms = 1000,
        updated_at_ms = 2000,
      },
      interaction = { {
        id = "child-turn",
        state = "running",
        thought = {},
        active = { text = "Inspecting Bevy ownership.", tool_count = 0, failed_count = 0 },
      } },
    } },
  } }, { now_ms = 3000, timeline_status = { id = "subagents", text = "Waiting for 1 subagent" } })
  assert_equals(parent_with_agent.lines[2], "▸ Thinking for 1s, 1 agent spawned",
    "a parent interaction should summarize its owned child run")
  assert_true(vim.tbl_contains(parent_with_agent.lines, "  Waiting for 1 subagent"),
    "a parent without active work should expose its transient waiting state")
  assert_equals(parent_with_agent.lines[parent_with_agent.timeline_status_line - 1], "",
    "Timeline Status should always have one leading blank line")
  assert_true(vim.tbl_contains(parent_with_agent.lines, "▸ Agent Bevy scout running for 2s"),
    "the main timeline should keep child runtime at top-level indentation")
  parent_with_agent = interaction_renderer.build({ {
    kind = "interaction",
    interaction = {
      id = "parent-turn",
      prompt = "/agent explorer inspect Bevy",
      state = "running",
      duration_ms = 1000,
      thought = {},
      active = { text = "Continuing parent analysis.", tool_count = 0, failed_count = 0 },
    },
    agent = { {
      kind = "agent_lifecycle",
      id = "agent-run",
      run = { id = "agent-run", definition = "explorer", status = "running", created_at_ms = 1000 },
    } },
  } }, { now_ms = 3000 })
  assert_true(not vim.tbl_contains(parent_with_agent.lines, "↳ Waiting on 1 subagent."),
    "parent work should replace the synthetic waiting state while its child keeps running")
  local interaction_state = require("diff_review.views.harness.interaction_state")
  local steering_state = {
    active_wait = { interaction_id = "steering-parent", started_at_ms = 1000, agent_count = 1 },
    interaction = { {
      id = "steering-parent",
      prompt = "/agent explorer inspect Bevy",
      state = "running",
      node_list = {},
    } },
  }
  steering_state.interaction_by_id = { ["steering-parent"] = steering_state.interaction[1] }
  interaction_state.apply_node(steering_state, {
    interaction_id = "steering-parent",
    node = {
      kind = "steering_prompt",
      prompt = { id = "steering-parent:steering:1", text = "What day is it?", created_at_ms = 2000 },
    },
  })
  assert_true(steering_state.active_wait ~= nil,
    "timeline nodes should not own session-level waiting state")
  interaction_state.apply_wait(steering_state, { interaction_id = "steering-parent", wait = nil })
  assert_true(steering_state.active_wait == nil,
    "an explicit wait update should atomically remove transient waiting chrome")
  assert_equals(steering_state.interaction[1].node_list[1].prompt.text, "What day is it?",
    "steering should retain its chronological position after waiting disappears")
  local projected_timeline = require("diff_review.views.harness.timeline").project({
    timeline = {},
    interaction = { {
      id = "parent-turn",
      prompt = "/agent local-code-explorer inspect Bevy",
      state = "complete",
      thought = {},
    } },
    agent = {
      run = {
        {
          id = "coordinator-run",
          definition = "default",
          parent_interaction_id = "parent-turn",
          provider_thread_id = "coordinator-thread",
          status = "completed",
        },
        {
          id = "explorer-run",
          definition = "local-code-explorer",
          parent_interaction_id = "parent-turn",
          parent_thread_id = "coordinator-thread",
          provider_thread_id = "explorer-thread",
          status = "completed",
        },
      },
      turn = { {
        agent_run_id = "explorer-run",
        interaction = { id = "child-turn", state = "complete", response = "Mapped Bevy." },
      } },
    },
  })
  assert_equals(#projected_timeline, 1,
    "completed interactions missing from a durable snapshot should remain in the local projection")
  assert_equals(projected_timeline[1].interaction.id, "parent-turn",
    "the projection should retain the spawning parent interaction")
  local coordinator_entry = projected_timeline[1].agent_by_id["coordinator-run"]
  assert_true(coordinator_entry ~= nil,
    "provider-nested children should not flatten beside their parent agent")
  assert_equals(coordinator_entry.run.definition, "default",
    "the root provider child should remain attached to the interaction")
  assert_equals(coordinator_entry.agent[1].run.definition, "local-code-explorer",
    "parent thread identity should preserve nested provider hierarchy")
  assert_equals(coordinator_entry.agent[1].interaction[1].response, "Mapped Bevy.",
    "the nested run should carry its durable child interaction timeline")
  local delegation_segment = mock_segment("ordered-parent", "complete", nil, { {
    id = "delegating",
    text = "Delegating the repository map.",
    tool = {},
  } }, nil, 1000)
  delegation_segment.segment.spawned_agent_count = 1
  local ordered_render = interaction_renderer.build({ {
    kind = "interaction",
    interaction = {
      id = "ordered-parent",
      prompt = "/agent explorer inspect Bevy",
      state = "complete",
      node_list = {
        delegation_segment,
        {
          kind = "agent_reference",
          agent = { id = "ordered-parent:agent:agent-one", agent_run_id = "agent-one" },
        },
        {
          kind = "steering_prompt",
          prompt = { id = "ordered-parent:steering:1", text = "What day is it?" },
        },
        mock_segment("ordered-parent-answer", "complete", nil, {}, "Today is Tuesday.", 1000),
        mock_segment("ordered-parent-final", "complete", nil, {}, "Final synthesis.", 1000),
      },
    },
    agent_by_id = {
      ["agent-one"] = {
        kind = "agent_lifecycle",
        id = "agent-one",
        run = {
          id = "agent-one",
          definition = "explorer",
          status = "completed",
          created_at_ms = 1000,
          updated_at_ms = 2000,
          completed_at_ms = 2000,
        },
        interaction = {},
      },
    },
  } }, { now_ms = 3000 })
  local agent_line = line_number(ordered_render.lines, "▸ Agent explorer completed in 1s")
  local steering_line = line_number(ordered_render.lines, "▸ What day is it?")
  local response_line = line_number(ordered_render.lines, "▸ Response")
  local intermediate_response_line = line_number(ordered_render.lines, "Today is Tuesday.")
  local final_response_line = line_number(ordered_render.lines, "Final synthesis.")
  assert_true(agent_line ~= nil and steering_line ~= nil and response_line ~= nil,
    "ordered interaction nodes should all render:\n" .. table.concat(ordered_render.lines, "\n"))
  assert_true(agent_line < steering_line and steering_line < response_line,
    "agent completion should stay at its spawn position while steering and response append after it")
  assert_true(intermediate_response_line < final_response_line,
    "a completed steering answer should remain visible before the final parent synthesis")
  local paused_plan_render = interaction_renderer.build({ {
    id = "paused-plan",
    prompt = "/plan choose a migration",
    kind = "plan_draft",
    state = "complete",
    awaiting_input = true,
    duration_ms = 3000,
    thought = {},
  } })
  assert_true(vim.tbl_contains(paused_plan_render.lines, "▸ Planning paused after 3s"),
    "question-only plan turns should not claim that the plan completed")

  builder._set_crate_dir_for_test(crate_root)
  builder._set_artifact_root_for_test(vim.fs.joinpath(test_root, "artifacts"))
  builder._set_runner_for_test(function(_, _, callback)
    vim.fn.mkdir(vim.fs.dirname(builder.release_binary_path()), "p")
    vim.fn.writefile({ "fake" }, builder.release_binary_path())
    callback({ code = 0, stdout = "", stderr = "" })
  end)
  client._set_launcher_for_test(fake_launcher)

  local render_frame_list = {}
  local render_transaction_list = {}
  controller._set_render_observer_for_test(function(lines, transaction)
    render_frame_list[#render_frame_list + 1] = lines
    render_transaction_list[#render_transaction_list + 1] = transaction
  end)
  require("diff_review.views.harness").open()
  assert_true(vim.wait(1000, function() return launched_binary ~= nil end, 10), "Harness broker did not launch")
  assert_true(vim.wait(1000, function()
    return session.harness.prompt_history[1] == "most recent prompt"
  end, 10), "Harness did not restore the broker prompt history")
  assert_true(launched_binary ~= builder.release_binary_path(), "Harness must not execute Cargo's replaceable output")
  assert_equals(launched_binary, builder.deployed_binary_path(), "Harness should launch the immutable staged executable")
  assert_equals(vim.wo[session.harness.transcript_win].breakindentopt, "shift:0",
    "wrapped Harness rows should preserve their existing indent without adding another shift")
  assert_equals(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, 1, false)[1], "",
    "new Harness transcripts should open empty")
  client.request("test.approval.open", {}, function() end)
  assert_true(vim.wait(1000, function()
    return require("diff_review.views.harness.approval").is_open()
      and #(session.harness.approval or {}) == 1
  end, 10), "streamed approvals should open without waiting for a state snapshot")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Approval requested", 1, true) ~= nil,
    "a pending approval should appear in the Harness winbar")
  client.request("test.approval.cancel", {}, function() end)
  assert_true(vim.wait(1000, function()
    return not require("diff_review.views.harness.approval").is_open()
      and #(session.harness.approval or {}) == 0
  end, 10), "approval cancellation should close the float and clear pending state")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Approval requested", 1, true) == nil,
    "approval cancellation should clear the Harness winbar")
  local original_get_parser = vim.treesitter.get_parser
  local original_treesitter_start = vim.treesitter.start
  local original_render_markdown = package.loaded["render-markdown"]
  local parser_region_list = nil
  local highlighted_language = nil
  local render_markdown_options = nil
  package.loaded["render-markdown"] = { render = function(options) render_markdown_options = options end }
  vim.treesitter.get_parser = function(buf, language)
    assert_equals(buf, session.harness.transcript_buf, "markdown should parse the Harness transcript")
    assert_equals(language, "markdown", "Harness responses should use the markdown parser")
    return {
      set_included_regions = function(_, region_list) parser_region_list = region_list end,
      invalidate = function() end,
    }
  end
  vim.treesitter.start = function(buf, language)
    assert_equals(buf, session.harness.transcript_buf, "markdown highlighting should target the Harness transcript")
    highlighted_language = language
  end
  require("diff_review.render.harness.markdown").render(
    session.harness.transcript_buf,
    session.harness.transcript_win,
    { { first0 = 2, after0 = 4 } }
  )
  vim.treesitter.get_parser = original_get_parser
  vim.treesitter.start = original_treesitter_start
  package.loaded["render-markdown"] = original_render_markdown
  assert_equals(highlighted_language, "markdown",
    "Harness markdown regions should activate inline Tree-sitter highlights")
  assert_equals(parser_region_list[1][1][1], 2,
    "Harness markdown highlighting should begin at the response range")
  assert_equals(parser_region_list[1][1][3], 4,
    "Harness markdown highlighting should stop before unrelated transcript rows")
  assert_equals(render_markdown_options.config.win_options.conceallevel.default, 0,
    "Harness markdown should restore the transcript's original conceal level in raw mode")
  assert_equals(render_markdown_options.config.win_options.conceallevel.rendered, 3,
    "Harness markdown should conceal syntax markers in rendered mode")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "what is this repo?" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    if not session.harness.ready or session.harness.busy then return false end
    for _, item in ipairs(session.harness.interaction) do
      if item.prompt == "what is this repo?" and interaction_response(item) == "Mock reply" then return true end
    end
    return false
  end, 10), "Harness did not complete the prompt queued during initialization")
  local prompt_seen = false
  for _, frame in ipairs(render_frame_list) do
    local frame_text = table.concat(frame, "\n")
    if frame_text:find("what is this repo?", 1, true) then
      if not prompt_seen then
        assert_true(frame_text:find("▸ Thinking for 0s", 1, true) ~= nil,
          "the first prompt frame must include its optimistic Thinking node: " .. frame_text)
      end
      prompt_seen = true
    end
    if prompt_seen then
      assert_true(frame_text:find("what is this repo?", 1, true) ~= nil,
        "rendered transcript must not regress to a stale pre-prompt snapshot: " .. frame_text)
      assert_true(frame_text:find("Harness is ready.", 1, true) == nil,
        "ready placeholder must not flash after the first prompt renders")
    end
  end
  assert_true(prompt_seen, "mock frame log should observe the first prompt")
  local partial_response_seen = false
  local complete_response_seen = false
  local summary_seen = false
  for _, frame in ipairs(render_frame_list) do
    local frame_text = table.concat(frame, "\n")
    if frame_text:find("↳ Mock", 1, true) and not frame_text:find("↳ Mock reply", 1, true) then
      partial_response_seen = true
    end
    if frame_text:find("Mock reply", 1, true) then complete_response_seen = true end
    if frame_text:find("▸ Thought for 2s, 2.7k tokens", 1, true) then summary_seen = true end
  end
  assert_true(partial_response_seen, "each assistant delta should produce a visible intermediate frame")
  assert_true(complete_response_seen, "assistant deltas should compose into the complete response")
  assert_true(summary_seen, "assistant completion should produce a final summary frame")
  local timing_state = {
    interaction = {},
    interaction_by_id = {},
    interaction_presentation = {},
  }
  interaction_state.begin(timing_state, "Measure the turn", 1000000000)
  local immediate_timing = raw_interaction_renderer.build(timing_state.interaction, { working_seconds = 0 })
  assert_equals(immediate_timing.lines[2], "▸ Thinking for 0s",
    "optimistic interactions should render before the first provider event")
  interaction_state.start_interaction(timing_state, {
    id = "timed-interaction",
    prompt = "Measure the turn",
    state = "running",
    node_list = {},
  })
  local admitted_timing = raw_interaction_renderer.build(timing_state.interaction, { working_seconds = 3 })
  assert_equals(admitted_timing.lines[2], "▸ Thinking for 3s",
    "interaction admission should preserve the optimistic segment")
  interaction_state.apply_node(timing_state, {
    interaction_id = "timed-interaction",
    node = mock_segment("timed-interaction", "running", nil, {}, nil, 1000),
  })
  assert_equals(#timing_state.interaction[1].node_list, 1,
    "the first provider segment should replace the optimistic segment without duplication")
  local completed_timing = {
    id = "timed-interaction",
    prompt = "Measure the turn",
    state = "complete",
    node_list = { mock_segment("timed-interaction", "complete", nil, {}, "Done", 1000) },
  }
  interaction_state.complete_interaction(timing_state, completed_timing, 7200000000)
  local final_timing = raw_interaction_renderer.build(timing_state.interaction)
  assert_equals(final_timing.lines[2], "▸ Thought for 6s, 2.7k tokens",
    "completion should retain local wall-clock time when provider duration is shorter")
  interaction_state.reconcile_snapshot(timing_state, { completed_timing })
  local reconciled_timing = raw_interaction_renderer.build(timing_state.interaction)
  assert_equals(reconciled_timing.lines[2], "▸ Thought for 6s, 2.7k tokens",
    "snapshot reconciliation should not regress the completed duration")
  local cached_context = { used = 275000, size = 353000, remaining_percent = 22 }
  local context_state = {
    session = { id = "same-session", context_usage = cached_context },
    interaction = {},
    interaction_by_id = {},
  }
  harness_snapshot.apply(context_state, {
    session = { id = "same-session" },
    interaction = {},
  }, "reconcile")
  assert_equals(context_state.session.context_usage.remaining_percent, 22,
    "same-session snapshots should retain the latest context status until the provider refreshes it")
  local markdown_render = interaction_renderer.build({ {
    id = "markdown",
    prompt = "Explain the result",
    state = "complete",
    duration_ms = 42000,
    token_count = 28500,
    thought = {},
    response = "# Heading\n\n- **item**",
  } })
  assert_equals(markdown_render.markdown_ranges[1].first0, 3,
    "assistant markdown should begin after its thought summary")
  assert_equals(markdown_render.markdown_ranges[1].after0, 6,
    "assistant markdown should end with its response body")
  assert_true(vim.iter(markdown_render.highlights):any(function(highlight)
    return highlight.line == 2 and highlight.group == "DiffReviewHarnessThought"
  end),
    "thought summaries should use the Harness green highlight")
  assert_equals(interaction_renderer.foldtext("▸ Thought for 42s, 28.5k tokens")[1][2],
    "DiffReviewHarnessThought",
    "closed thought folds should retain the Harness green highlight")
  assert_true(vim.iter(markdown_render.highlights):any(function(highlight)
    return highlight.line == 3 and highlight.group == "DiffReviewHarnessResponse"
  end),
    "response headings should use the Harness white highlight")
  local completed_thought_render = interaction_renderer.build({ {
    id = "completed-thought",
    prompt = "Inspect the repository",
    state = "complete",
    thought = { {
      id = "completed-thought:1",
      text = "I’m tracing the repository’s ownership boundaries\nand generated configuration.",
      tool = { {
        id = "completed-thought:tool:1",
        kind = "command",
        title = "rg --files",
        status = "completed",
        output = "README.md",
      } },
    } },
  } }, { expanded = {
    ["interaction:completed-thought:segment:completed-thought:segment:1"] = true,
    ["interaction:completed-thought:thought:completed-thought:1"] = true,
  } })
  assert_equals(completed_thought_render.lines[3],
    "↳ I’m tracing the repository’s ownership boundaries",
    "completed thoughts should retain their first text row")
  assert_equals(completed_thought_render.lines[4], "  and generated configuration.",
    "completed thoughts should retain every continuation row")
  assert_true(vim.iter(completed_thought_render.highlights):all(function(highlight)
    if highlight.line ~= 3 and highlight.line ~= 4 then return true end
    return highlight.group == "DiffReviewHarnessCommentary"
  end), "completed thought rows should retain the original italic commentary highlight")
  assert_equals(completed_thought_render.rows[3].expand_key,
    "interaction:completed-thought:thought:completed-thought:1",
    "completed thought rows should own their semantic expansion state")
  assert_equals(completed_thought_render.rows[5].expand_key,
    "interaction:completed-thought:thought:completed-thought:1:tools",
    "completed tool summaries should own the tool-list expansion state")
  assert_equals(completed_thought_render.lines[5], "  ▸ Ran 1 tool",
    "thought tool summaries should render as nested disclosure nodes")
  assert_equals(interaction_renderer.foldtext("  ▸ Ran 3 tools (2 failed)")[1][2], "Normal",
    "collapsed tool summaries should not inherit the dark Folded highlight")
  local folded_command_chunks = require("diff_review.render.harness.tool").foldtext_chunks({
    kind = "command",
    title = "rg --files",
    status = "completed",
  }, "  ")
  assert_equals(folded_command_chunks[1][2], "DiffReviewHarnessToolSuccess",
    "collapsed command bullets should retain their completion highlight")
  assert_true(vim.iter(folded_command_chunks):any(function(chunk)
    return chunk[1] == "rg" and chunk[2] == "DiffReviewHarnessCommand"
  end), "collapsed commands should retain their command-name highlight")
  local commentary_render = interaction_renderer.build({ {
    id = "commentary",
    prompt = "Map the repo",
    state = "running",
    thought = {},
    active = {
      text = "I’ll trace the repository’s actual control points and generated boundaries, then give you the operational map rather\nthan a directory listing.",
      tool_count = 0,
      failed_count = 0,
    },
  } }, { working_seconds = 2 })
  assert_equals(commentary_render.lines[3],
    "↳ I’ll trace the repository’s actual control points and generated boundaries, then give you the operational map rather",
    "intermediate commentary should carry the response indent")
  assert_equals(commentary_render.lines[4], "  than a directory listing.",
    "explicit commentary continuations should preserve the response indent")
  assert_true(vim.iter(commentary_render.highlights):any(function(highlight)
    return highlight.line == 3 and highlight.group == "DiffReviewHarnessCommentary"
  end),
    "intermediate commentary should render in italics")
  assert_equals(#commentary_render.markdown_ranges, 0,
    "streaming commentary should stay outside the markdown region")
  assert_true(not vim.tbl_contains(commentary_render.lines, "▸ Thinking…"),
    "streaming should rely on actual commentary instead of a transient Thinking placeholder")
  local wrapped_commentary = interaction_renderer.build({ {
    id = "wrapped-commentary",
    prompt = "Plan a narrow timeline",
    state = "running",
    thought = {},
    active = {
      text = "Repository facts leave three product decisions unresolved. Each one changes the dependency stack and test boundary.",
      tool_count = 0,
      failed_count = 0,
    },
  } }, { working_seconds = 2, content_width = 44 })
  local wrapped_thought_line_list = {}
  for line, row in pairs(wrapped_commentary.rows) do
    if row.kind == "active_thought" then wrapped_thought_line_list[#wrapped_thought_line_list + 1] = line end
  end
  table.sort(wrapped_thought_line_list)
  assert_true(#wrapped_thought_line_list > 1, "long timeline thoughts should become real wrapped rows")
  for index, line in ipairs(wrapped_thought_line_list) do
    local text = wrapped_commentary.lines[line]
    assert_true(vim.fn.strdisplaywidth(text) <= 44, "timeline thought exceeded its content width: " .. text)
    assert_true(text:sub(1, index == 1 and 4 or 2) == (index == 1 and "↳ " or "  "),
      "timeline thought continuations should preserve their semantic indent")
  end
  local unicode_wrap = require("diff_review.render.display_text").wrap(
    "界界界界界界界界界界界界",
    10,
    "↳ ",
    "  "
  )
  assert_true(#unicode_wrap > 1, "display wrapping should split overlong multibyte words")
  assert_true(vim.iter(unicode_wrap):all(function(line) return vim.fn.strdisplaywidth(line) <= 10 end),
    "display wrapping should measure multibyte text by rendered cells")
  local settled_transcript = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(settled_transcript, "▸ what is this repo?"), "user prompts should use the Harness prompt marker")
  assert_true(vim.tbl_contains(settled_transcript, "▸ Thought for 2s, 2.7k tokens"),
    "completed responses should expose timing and token usage")
  assert_true(vim.tbl_contains(settled_transcript, "Mock reply"),
    "assistant response content should remain raw Markdown in the buffer")
  local response_body_line = line_number(settled_transcript, "Mock reply")
  assert_true(vim.iter(vim.api.nvim_buf_get_extmarks(
    session.harness.transcript_buf,
    session.harness.render_namespace,
    { response_body_line - 1, 0 },
    { response_body_line - 1, -1 },
    { details = true }
  )):any(function(extmark)
    local virtual_text = extmark[4].virt_text
    return virtual_text and virtual_text[1] and virtual_text[1][1] == "  "
  end), "response rows should use a visual two-column prefix so soft wraps stay aligned")
  assert_true(not vim.tbl_contains(settled_transcript, "You"), "transcript should not render a You label")
  assert_true(not vim.tbl_contains(settled_transcript, "Assistant"), "transcript should not render an Assistant label")
  local multiline_prompt = interaction_renderer.build({ {
    id = "multiline",
    prompt = "What is this repo?\nis it cool?",
    state = "running",
    thought = {},
  } })
  assert_equals(multiline_prompt.lines[1], "▸ What is this repo?", "prompt should mark its first line")
  assert_equals(multiline_prompt.lines[2], "  is it cool?", "prompt continuation should align under quoted input")
  assert_equals(multiline_prompt.highlights[1].group, "DiffReviewHarnessPrompt",
    "user prompts should use the Harness yellow highlight")
  local steering_render = interaction_renderer.build({ {
    id = "steering",
    prompt = "Start the analysis",
    state = "complete",
    duration_ms = 1000,
    thought = {},
    steering_input = { {
      id = "steering:steering:1",
      text = "Also inspect the renderer\nand its tests",
      created_at_ms = 1,
    } },
  } }, { content_width = 80 })
  assert_equals(steering_render.lines[3], "▸ Also inspect the renderer",
    "acknowledged steering should render as user input under the owning turn summary")
  assert_equals(steering_render.lines[4], "  and its tests",
    "multiline steering should preserve prompt continuation alignment")
  assert_equals(#steering_render.prompt_lines, 2,
    "prompt navigation should include original and steering inputs")
  local command_render = interaction_renderer.build({ {
    id = "commands",
    prompt = "Run commands",
    state = "complete",
    duration_ms = 1,
    thought = { {
      id = "commands:thought:1",
      text = "Checking the workspace.",
      tool = { {
      id = "rejected-command",
      kind = "command",
      title = "Get-ChildItem -Force",
      output = "`pwsh -Command Get-ChildItem` rejected: Generated from Rulesync permission.bash: pwsh",
      status = "failed",
      failed = true,
    }, {
      id = "ansi-command",
      kind = "command",
      title = "Get-ChildItem -Force",
      output = "\27[32;1mMode\27[0m  \27[32;1mName\27[0m",
      status = "completed",
      failed = false,
    } },
      started_at_ms = 0,
      completed_at_ms = 1,
    } },
  } })
  assert_true(vim.tbl_contains(command_render.lines, "  • Ran Get-ChildItem -Force"),
    "completed commands should publish one stable command row")
  assert_true(vim.tbl_contains(command_render.lines, "    └ Mode  Name"),
    "command output should never expose terminal color controls")
  assert_true(vim.iter(command_render.highlights):any(function(highlight)
    return command_render.lines[highlight.line] == "  • Ran Get-ChildItem -Force"
      and highlight.first == 2
      and highlight.last == 2 + #"•"
      and highlight.group == "DiffReviewHarnessToolFailure"
  end), "rejected command bullets should use the failure highlight")
  local long_command_render = interaction_renderer.build({ {
    id = "long-command",
    prompt = "Inspect manifests",
    state = "complete",
    thought = { {
      id = "long-command:thought:1",
      text = "Inspecting manifests.",
      tool = { {
        id = "long-command:tool:1",
        kind = "command",
        title = "rg -n --glob 'README*' --glob 'rulesync.jsonc' --glob 'package.json' --glob 'Cargo.toml' --glob 'init.lua' --glob 'config.toml' 'Rulesync|dotfiles|Neovim|Nushell|WezTerm|Codex' .",
        output = "README.md:1:# Config",
        status = "completed",
      } },
    } },
  } }, { content_width = 72 })
  local command_line_list = {}
  for line, row in pairs(long_command_render.rows) do
    if row.tool and row.tool.id == "long-command:tool:1" and row.kind ~= "tool_output" then
      command_line_list[#command_line_list + 1] = long_command_render.lines[line]
    end
  end
  assert_true(#command_line_list > 1, "long folded commands should render as multiple real buffer lines")
  for _, line in ipairs(command_line_list) do
    assert_true(vim.fn.strdisplaywidth(line) <= 72, "wrapped command row exceeded the transcript width: " .. line)
  end
  local output_line = line_number(long_command_render.lines, "    └ README.md:1:# Config")
  assert_true(output_line ~= nil, "long command fixture should retain its folded output")
  assert_equals(long_command_render.rows[output_line - #command_line_list].expand_key,
    "interaction:long-command:thought:long-command:thought:1:tool:1",
    "wrapped command rows should share one semantic output expansion key")
  assert_equals(vim.bo[session.harness.transcript_buf].filetype, "Harness", "transcript filetype mismatch")
  assert_equals(vim.bo[session.harness.composer_buf].filetype, "HarnessInput", "composer filetype mismatch")
  for _, harness_win in ipairs({ session.harness.transcript_win, session.harness.composer_win }) do
    assert_true(not vim.wo[harness_win].number, "Harness windows should hide absolute line numbers")
    assert_true(not vim.wo[harness_win].relativenumber, "Harness windows should hide relative line numbers")
    assert_equals(vim.wo[harness_win].signcolumn, "no", "Harness windows should hide the sign gutter")
  end
  assert_equals(vim.wo[session.harness.transcript_win].statuscolumn, "",
    "Harness transcripts should hide custom status columns")
  assert_equals(vim.wo[session.harness.transcript_win].foldcolumn, "0",
    "Harness transcripts should hide the fold gutter")
  assert_equals(vim.wo[session.harness.composer_win].foldcolumn, "2",
    "Harness input should reserve a two-character gutter")
  assert_equals(vim.wo[session.harness.composer_win].statuscolumn,
    "%#DiffReviewHarnessPrompt#%{v:lnum == 1 ? '❯ ' : '  '}%*",
    "Harness input should render a prompt marker only beside its first line")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "first line", "second line" })
  local first_composer_gutter = vim.api.nvim_eval_statusline(
    vim.wo[session.harness.composer_win].statuscolumn,
    { winid = session.harness.composer_win, use_statuscol_lnum = 1 }
  )
  local second_composer_gutter = vim.api.nvim_eval_statusline(
    vim.wo[session.harness.composer_win].statuscolumn,
    { winid = session.harness.composer_win, use_statuscol_lnum = 2 }
  )
  assert_equals(first_composer_gutter.str, "❯ ", "Harness input should mark and pad its first line")
  assert_true(second_composer_gutter.str:match("^%s*$") ~= nil,
    "Harness input should leave continuation-line gutters blank")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "" })
  local composer_mode_autocmd_list = vim.api.nvim_get_autocmds({
    group = "DiffReviewHarnessComposerMode",
    buffer = session.harness.composer_buf,
  })
  assert_true(#composer_mode_autocmd_list == 2,
    "HarnessInput should enforce Normal mode on buffer and window entry")
  local wrapped_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(wrapped_buf, 0, -1, false, {
    "header",
    string.rep("wrapped response content ", 12),
    "final response",
  })
  local wrapped_win = vim.api.nvim_open_win(wrapped_buf, false, {
    relative = "editor",
    row = 0,
    col = 0,
    width = 20,
    height = 5,
    style = "minimal",
  })
  vim.wo[wrapped_win].wrap = true
  assert_true(require("diff_review.views.harness.layout").maximum_content_topline(wrapped_buf, wrapped_win) > 1,
    "scroll boundaries should count wrapped screen rows instead of only physical lines")
  vim.api.nvim_win_close(wrapped_win, true)
  vim.api.nvim_buf_delete(wrapped_buf, { force = true })
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Read", 1, true) ~= nil, "winbar should expose Read mode")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("%#DiffReviewHarnessRead#Read%*", 1, true) ~= nil,
    "winbar should render Read with the white mode highlight")
  assert_true(vim.wo[session.harness.transcript_win].breakindent,
    "Harness soft-wrapped commentary should preserve its leading indent")
  assert_equals(vim.wo[session.harness.transcript_win].breakindentopt, "shift:0",
    "Harness soft wraps should preserve the row indent without adding another two columns")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Mock CLI", 1, true) ~= nil, "winbar should expose the underlying CLI")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("mock-model-resolved", 1, true) ~= nil, "winbar should expose the resolved model")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("22%% context left (353K)", 1, true) ~= nil,
    "winbar should expose normalized context remaining at the right edge")
  local plain_winbar = vim.wo[session.harness.transcript_win].winbar:gsub("%%#.-#", ""):gsub("%%%*", "")
  assert_true(plain_winbar:find("Harness", 1, true) == nil, "winbar should omit the redundant view name")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("@workspace", 1, true) == nil, "winbar should omit legacy trust labels")
  assert_equals(vim.wo[session.harness.composer_win].winbar, "", "composer should not duplicate the Harness winbar")
  session.harness.goal = { objective = "Complete the plan", state = "active" }
  controller.refresh_winbar()
  assert_true(vim.wo[session.harness.transcript_win].winbar:find(
    "%#DiffReviewHarnessGoal# • Goal: Complete the plan%*", 1, true
  ) ~= nil, "winbar should render the active goal with the green goal highlight")
  session.harness.goal = nil
  controller.refresh_winbar()
  assert_true(vim.fn.maparg("<C-s>", "i", false, true).callback ~= nil, "composer should map submit in insert mode")
  assert_true(vim.fn.maparg("<C-q>", "n", false, true).callback ~= nil,
    "HarnessInput should map Ctrl-q to active-turn steering in normal mode")
  assert_true(vim.fn.maparg("<C-q>", "i", false, true).callback ~= nil,
    "HarnessInput should map Ctrl-q to active-turn steering in insert mode")
  assert_true(vim.fn.maparg("<S-Tab>", "n", false, true).callback ~= nil, "Harness should map Shift-Tab in normal mode")
  assert_true(vim.fn.maparg("<S-Tab>", "i", false, true).callback ~= nil, "Harness should map Shift-Tab in insert mode")
  assert_true(vim.fn.maparg("<M-s>", "i", false, true).callback ~= nil,
    "Harness should map Alt-s to edit the newest queued prompt")
  assert_true(vim.fn.maparg("<C-y>", "n", false, true).callback ~= nil,
    "Harness should map Ctrl-y to the previous user prompt")
  assert_true(vim.fn.maparg("<C-z>", "n", false, true).callback ~= nil,
    "Harness should map Ctrl-z to the next user prompt")
  assert_true(vim.fn.maparg("<C-n>", "n", false, true).callback == nil,
    "Harness should not retain the old Ctrl-n prompt mapping")
  assert_true(vim.fn.maparg("<Tab>", "n", false, true).callback ~= nil,
    "Harness should map Tab to toggle complete command output")
  assert_true(has_buffer_keymap(session.harness.transcript_buf, "n", "q"),
    "Harness transcript should map q to close")
  assert_true(not has_buffer_keymap(session.harness.composer_buf, "n", "q"),
    "HarnessInput should leave q available for normal-mode editing")
  session.harness.busy = true
  vim.api.nvim_set_current_win(session.harness.transcript_win)
  local transcript_cancel = vim.fn.maparg("<C-c>", "n", false, true).callback
  assert_true(transcript_cancel ~= nil, "Harness should map Ctrl-c to turn cancellation")
  vim.api.nvim_set_current_win(session.harness.composer_win)
  vim.wait(100, function() return false end, 10)
  assert_equals(vim.wo[session.harness.composer_win].winbar, "",
    "entering HarnessInput should not let Dropbar restore a composer winbar")
  vim.api.nvim_set_current_win(session.harness.transcript_win)
  vim.api.nvim_set_current_win(session.harness.composer_win)
  vim.wait(100, function() return false end, 10)
  assert_equals(vim.wo[session.harness.composer_win].winbar, "",
    "re-entering HarnessInput should preserve the composer winbar exclusion")
  assert_true(vim.fn.maparg("<C-c>", "i", false, true).callback ~= nil,
    "HarnessInput should map Ctrl-c to turn cancellation")
  vim.api.nvim_set_current_win(session.harness.transcript_win)
  transcript_cancel()
  assert_true(vim.wait(1000, function() return request_by_method["turn.cancel"] ~= nil end, 10),
    "Ctrl-c should send an out-of-band turn cancellation request")
  assert_true(request_by_method["turn.cancel"].params.restore_prompt_if_no_output,
    "Ctrl-c should request prompt restoration when the backend can roll back an output-free turn")
  session.harness.busy = false
  session.harness.cancel_requested = false
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "retract me" })
  controller.submit()
  assert_true(vim.wait(1000, function() return retract_request ~= nil end, 10),
    "the output-free prompt should remain active until cancellation")
  controller.cancel_turn()
  assert_true(vim.wait(1000, function()
    return not session.harness.busy
      and table.concat(vim.api.nvim_buf_get_lines(session.harness.composer_buf, 0, -1, false), "\n") == "retract me"
  end, 10), "an output-free cancellation should restore the exact prompt to HarnessInput")
  assert_true(not vim.tbl_contains(vim.tbl_map(function(item) return item.id end, session.harness.interaction),
    "retracted-interaction"), "the retracted interaction should disappear from the timeline")
  assert_true(not table.concat(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false), "\n")
    :find("▸ retract me", 1, true), "the settled transcript should not retain the retracted prompt")
  if prompt_history[1] == "retract me" then table.remove(prompt_history, 1) end
  if session.harness.prompt_history[1] == "retract me" then table.remove(session.harness.prompt_history, 1) end
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "" })
  assert_true(not has_buffer_keymap(session.harness.transcript_buf, "i", "<Up>"),
    "prompt history should remain scoped to HarnessInput")
  assert_true(has_buffer_keymap(session.harness.composer_buf, "i", "<Up>"),
    "HarnessInput should map Up to older global prompts")
  assert_true(has_buffer_keymap(session.harness.composer_buf, "i", "<Down>"),
    "HarnessInput should map Down to newer global prompts")
  vim.api.nvim_set_current_win(session.harness.composer_win)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "" })
  local history_previous = vim.fn.maparg("<Up>", "i", false, true).callback
  local history_next = vim.fn.maparg("<Down>", "i", false, true).callback
  assert_equals(session.harness.prompt_history[1], "what is this repo?",
    "composer navigation should use the current broker history")
  history_previous()
  assert_equals(vim.api.nvim_get_current_line(), "what is this repo?",
    "Up on an empty composer should recall the newest global prompt")
  history_previous()
  assert_equals(vim.api.nvim_get_current_line(), "most recent prompt",
    "repeated Up should continue toward older prompts")
  history_next()
  assert_equals(vim.api.nvim_get_current_line(), "what is this repo?",
    "Down should move toward newer prompts")
  history_next()
  assert_equals(vim.api.nvim_get_current_line(), "",
    "Down past the newest prompt should restore the empty composer")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "unsent draft" })
  history_previous()
  assert_equals(vim.api.nvim_get_current_line(), "unsent draft",
    "Up should preserve a nonempty composer before history navigation starts")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "" })
  history_previous()
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "edited recalled prompt" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = session.harness.composer_buf })
  assert_equals(session.harness.prompt_history_index, 0,
    "editing a recalled prompt should leave history navigation")
  history_previous()
  assert_equals(vim.api.nvim_get_current_line(), "edited recalled prompt",
    "Up should preserve an edited recalled prompt as a new draft")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "" })
  controller.select_effort()
  local effort_picker = require("diff_review.views.picker")._state_for_test()
  assert_true(effort_picker ~= nil, "effort selection should use the shared picker")
  assert_equals(effort_picker.spec.page_list[1].title, "Select reasoning effort",
    "effort selection should label its picker page")
  assert_equals(#effort_picker.spec.page_list[1].option_list, 5,
    "effort selection should expose every reasoning level")
  assert_equals(vim.api.nvim_win_get_config(effort_picker.win).focusable, true,
    "effort picker should own modal focus")
  require("diff_review.views.picker").close(false)
  controller.select_model()
  assert_true(vim.wait(1000, function() return require("diff_review.views.picker").is_open() end, 10),
    "model selection should open the shared picker after backend discovery")
  local model_picker = require("diff_review.views.picker")._state_for_test()
  assert_equals(model_picker.spec.page_list[1].option_list[1].detail, "mock-model",
    "model picker should render backend identifiers in the detail column")
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_true(vim.wait(1000, function()
    return request_by_method["session.configure"]
      and request_by_method["session.configure"].params.model == "mock-model"
  end, 10), "model selection should configure the chosen backend model")
  local command_source = require("diff_review.views.harness.completion.command_source").new()
  vim.api.nvim_set_current_win(session.harness.composer_win)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/ " })
  vim.api.nvim_win_set_cursor(session.harness.composer_win, { 1, 1 })
  assert_true(command_source:enabled(), "slash completion should activate at the start of a Harness prompt")
  local command_completion = nil
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_true(command_completion ~= nil and #command_completion.items >= 9, "slash completion should list Harness commands")
  assert_true(vim.iter(command_completion.items):any(function(item) return item.label == "/rename" end),
    "slash completion should expose session rename")
  assert_true(vim.iter(command_completion.items):any(function(item) return item.label == "/backend" end),
    "slash completion should expose backend selection")
  assert_true(vim.iter(command_completion.items):any(function(item) return item.label == "/compact" end),
    "slash completion should expose compact when the backend advertises it")
  assert_true(vim.iter(command_completion.items):any(function(item) return item.label == "/full" end),
    "slash completion should expose Full mode")
  assert_true(vim.iter(command_completion.items):any(function(item) return item.label == "/yolo" end),
    "slash completion should expose YOLO mode")
  assert_equals(command_completion.items[1].label, "/plan", "slash completion should prioritize plan creation")

  session.harness.capability.agent = { catalog = false }
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_true(not vim.iter(command_completion.items):any(function(item) return item.label == "/agent" end),
    "slash completion should hide child timelines when observation is unavailable")
  assert_true(not vim.iter(command_completion.items):any(function(item) return item.label == "/spawn" end),
    "slash completion should hide child-agent spawning when the catalog is unavailable")
  session.harness.capability.fast_mode = false
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_true(not vim.iter(command_completion.items):any(function(item) return item.label == "/fast" end),
    "slash completion should hide fast mode when the backend does not advertise it")
  session.harness.capability.fast_mode = true
  session.harness.capability.agent = { catalog = true, observe = true }
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_true(vim.iter(command_completion.items):any(function(item) return item.label == "/agent" end),
    "slash completion should expose child timelines for native Codex sessions")
  assert_true(vim.iter(command_completion.items):any(function(item) return item.label == "/spawn" end),
    "slash completion should expose child-agent spawning for native Codex sessions")
  session.harness.agent = {
    definition = {
      { name = "explorer", description = "Read-oriented repository explorer" },
      { name = "worker", description = "Implementation-focused Codex agent" },
    },
    run = {},
    turn = {},
  }
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/spawn exp " })
  vim.api.nvim_win_set_cursor(session.harness.composer_win, { 1, 10 })
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_equals(#command_completion.items, 2, "/spawn should complete definitions from the broker catalog")
  assert_equals(command_completion.items[1].label, "explorer", "spawn completion should preserve catalog order")
  assert_equals(command_completion.items[1].textEdit.newText, "explorer",
    "spawn completion should insert only the selected definition")
  assert_equals(command_completion.items[1].textEdit.range.start.character, 7,
    "spawn completion should replace only the partial definition token")
  assert_equals(command_completion.items[1].textEdit.range["end"].character, 10,
    "spawn completion should stop at the current cursor")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/spawn explorer inspect Bevy" })
  vim.api.nvim_win_set_cursor(session.harness.composer_win, { 1, 28 })
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_equals(#command_completion.items, 0, "spawn completion should stop after the definition argument")

  session.harness.agent.run = {
    { id = "run-zeta", definition = "zeta", status = "running", created_at_ms = 2, updated_at_ms = 2 },
    { id = "run-done", definition = "ended", status = "completed", created_at_ms = 1, updated_at_ms = 2 },
    { id = "run-alpha", definition = "alpha", status = "waiting", created_at_ms = 1, updated_at_ms = 2 },
  }
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/agent x" })
  vim.api.nvim_win_set_cursor(session.harness.composer_win, { 1, 8 })
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_equals(command_completion.items[1].label, "main", "/agent completion should lead with Main")
  assert_equals(command_completion.items[2].label, "a", "/agent completion should expose the first running alias")
  assert_equals(command_completion.items[3].label, "b", "/agent completion should expose the second running alias")
  assert_equals(#command_completion.items, 3, "/agent completion should exclude ended children from aliases")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/agent" })
  controller.submit()
  assert_true(require("diff_review.views.picker").is_open("agent"),
    "/agent without arguments should open the timeline picker")
  require("diff_review.views.picker").close(true)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/spawn" })
  controller.submit()
  local spawn_picker_state = require("diff_review.views.picker")._state_for_test()
  assert_true(require("diff_review.views.picker").is_open("spawn") and spawn_picker_state.search_win ~= nil,
    "/spawn without arguments should open the searchable definition picker")
  require("diff_review.views.picker").close(true)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/agent a" })
  controller.submit()
  assert_equals(session.harness.selected_agent_run_id, "run-alpha",
    "/agent a should select the first alphabetic running child")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/agent 1" })
  controller.submit()
  assert_equals(session.harness.selected_agent_run_id, "run-zeta",
    "/agent 1 should share the second running-child alias")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/agent main" })
  controller.submit()
  assert_equals(session.harness.selected_agent_run_id, nil, "/agent main should always restore the parent timeline")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/spawn explorer inspect Bevy" })
  controller.submit()
  assert_true(vim.wait(1000, function()
    return request_by_method["agent.start"] ~= nil and not session.harness.busy
  end, 10), "/spawn should route the definition and task through agent.start")
  assert_equals(request_by_method["agent.start"].params.definition, "explorer",
    "/spawn should preserve the selected definition")
  assert_equals(request_by_method["agent.start"].params.task, "inspect Bevy",
    "/spawn should preserve the child task")

  session.harness.capability.native_compact = false
  command_source:get_completions({}, function(result) command_completion = result end)
  assert_true(not vim.iter(command_completion.items):any(function(item) return item.label == "/compact" end),
    "slash completion should omit compact when the backend does not advertise it")
  session.harness.capability.native_compact = true

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/compact" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return request_by_method["session.compact"] ~= nil
      and session.harness.session.context_usage.remaining_percent == 92
      and not session.harness.busy
  end, 10), "/compact should invoke provider compaction without creating a chat interaction")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("92%% context left (353K)", 1, true) ~= nil,
    "compaction should refresh the right-aligned context status")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "ask choices" })
  controller.submit()
  local ordinary_question_view = require("diff_review.views.harness.plan_question")
  assert_true(vim.wait(2000, ordinary_question_view.is_open, 10),
    "ordinary chat questions should open the shared Harness question float")
  assert_equals(session.harness.active_elicitation.owner, "interaction",
    "ordinary questions should remain interaction-owned instead of creating a plan")
  vim.fn.maparg("q", "n", false, true).callback()
  controller.render()
  local question_timeline = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(question_timeline, "  Waiting for input (press oe)"),
    "closed ordinary questions should advertise the configured reopen key in Timeline Status")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Input requested", 1, true) == nil,
    "pending input should no longer duplicate status in the winbar")
  assert_true(has_buffer_keymap(session.harness.transcript_buf, "n", "oe"),
    "Harness should bind the configured question reopen action")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "clarify choices" })
  controller.submit()
  assert_true(vim.wait(1000, function() return not session.harness.busy end, 10),
    "clarification chat should complete while the question remains pending")
  assert_true(not ordinary_question_view.is_open(),
    "an unchanged question revision should remain dismissed after clarification chat")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "replace choices" })
  controller.submit()
  assert_true(vim.wait(1000, ordinary_question_view.is_open, 10),
    "a provider-replaced question revision should present automatically")
  local replacement_question_state = ordinary_question_view._state_for_test()
  local replacement_question_text = table.concat(
    vim.api.nvim_buf_get_lines(replacement_question_state.buf, 0, -1, false),
    "\n"
  )
  assert_true(replacement_question_text:find("Which revised migration strategy", 1, true) ~= nil,
    "the replacement picker should render the provider's current questions")
  vim.fn.maparg("q", "n", false, true).callback()
  vim.api.nvim_set_current_win(session.harness.transcript_win)
  vim.fn.maparg("oe", "n", false, true).callback()
  assert_true(vim.wait(1000, ordinary_question_view.is_open, 10),
    "the Harness reopen key should restore an ordinary interaction question")
  vim.fn.maparg("q", "n", false, true).callback()
  active_elicitation = nil
  session.harness.active_elicitation = nil
  session.harness.presented_question_key = nil

  local file_source = require("diff_review.views.harness.completion.file_source").new()
  vim.api.nvim_set_current_win(session.harness.composer_win)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "inspect @nvim/lua/diff_review" })
  vim.api.nvim_win_set_cursor(session.harness.composer_win, { 1, 29 })
  assert_true(file_source:enabled(), "file completion should activate after @")
  local file_completion = nil
  file_source:get_completions({}, function(result) file_completion = result end)
  assert_true(vim.wait(3000, function() return file_completion ~= nil end, 10), "workspace file completion timed out")
  assert_true(vim.iter(file_completion.items):any(function(item)
    return item.label == "@nvim/lua/diff_review/init.lua"
  end), "workspace file completion should include Git-visible files")
  vim.fn.maparg("<S-Tab>", "n", false, true).callback()
  assert_true(vim.wait(1000, function() return active_session.execution_mode == "write" end, 10),
    "Shift-Tab should enable Write mode")
  assert_equals(request_by_method["session.execution_mode"].params.mode, "write",
    "Shift-Tab should route Write mode through the broker")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("Write", 1, true) ~= nil, "winbar should show Write mode")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("%#DiffReviewHarnessWrite#Write%*", 1, true) ~= nil,
    "winbar should render Write with its mode highlight")
  vim.fn.maparg("<S-Tab>", "n", false, true).callback()
  assert_true(vim.wait(1000, function() return active_session.execution_mode == "full" end, 10),
    "second Shift-Tab should enable Full mode")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("%#DiffReviewHarnessFull#Full%*", 1, true) ~= nil,
    "winbar should render Full with its mode highlight")
  vim.fn.maparg("<S-Tab>", "n", false, true).callback()
  assert_true(vim.wait(1000, function() return active_session.execution_mode == "yolo" end, 10),
    "third Shift-Tab should enable YOLO mode")
  assert_true(vim.wo[session.harness.transcript_win].winbar:find("%#DiffReviewHarnessYolo#YOLO%*", 1, true) ~= nil,
    "winbar should render YOLO with its mode highlight")
  vim.fn.maparg("<S-Tab>", "n", false, true).callback()
  assert_true(vim.wait(1000, function() return active_session.execution_mode == "read" end, 10),
    "fourth Shift-Tab should restore Read mode")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "hello harness" })
  local tool_frame_start = #render_frame_list + 1
  controller.submit()
  assert_true(vim.wait(2000, function()
    for _, item in ipairs(session.harness.interaction) do
      if item.prompt == "hello harness" and item.state == "complete" then return true end
    end
    return false
  end, 10), "Harness did not complete the interaction timeline")
  assert_true(vim.wait(1000, function() return session.harness.prompt_history[1] == "hello harness" end, 10),
    "submitted prompts should enter global history immediately")
  local active_counter_seen = false
  local active_tool_leak_seen = false
  local mixed_transition_seen = false
  local tool_completed_seen = false
  for frame_index = tool_frame_start, #render_frame_list do
    local frame_text = table.concat(render_frame_list[frame_index], "\n")
    if frame_text:find("Running 1 tool", 1, true) then
      active_counter_seen = true
      if frame_text:find("Get-Content README.md", 1, true) then active_tool_leak_seen = true end
      if frame_text:find("Ran 1 tool", 1, true) then mixed_transition_seen = true end
    end
    if frame_text:find("▸ Thought for 2s, 2.7k tokens, 1 tool called", 1, true)
      and not frame_text:find("• Ran Get-Content README.md", 1, true) then tool_completed_seen = true end
  end
  assert_true(active_counter_seen, "active thoughts should stream an updating tool counter")
  assert_true(not active_tool_leak_seen, "active thoughts must not expose expandable tool rows")
  assert_true(not mixed_transition_seen, "Running-to-Ran transitions must publish one coherent frame")
  assert_true(tool_completed_seen, "completed thoughts should atomically publish their collapsed summary\n"
    .. table.concat(render_frame_list[#render_frame_list] or {}, "\n"))
  for transaction_index = tool_frame_start, #render_transaction_list do
    assert_true((render_transaction_list[transaction_index].first0 or 0) > 0,
      "streaming a later interaction should never rewrite the settled transcript prefix")
  end
  local transcript_line = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(not vim.tbl_contains(transcript_line, "  • Ran Get-Content README.md"),
    "completed summaries should initially hide their thought tree")
  local summary_line = line_number(transcript_line, "▸ Thought for 2s, 2.7k tokens, 1 tool called")
  assert_true(summary_line ~= nil, "completed interaction should render its summary node")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { summary_line, 0 })
  controller.toggle_activity()
  local thought_line = line_number(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "↳ Working")
  assert_true(thought_line ~= nil, "expanding a summary should reveal its completed thought")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { thought_line, 0 })
  controller.toggle_activity()
  local tool_summary_line = line_number(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "  ▸ Ran 1 tool")
  assert_true(tool_summary_line ~= nil, "expanding a thought should reveal its tool summary")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { tool_summary_line, 0 })
  controller.toggle_activity()
  transcript_line = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
  assert_true(vim.tbl_contains(transcript_line, "  • Ran Get-Content README.md"),
    "expanding a tool summary should reveal command headers")
  assert_true(not vim.tbl_contains(transcript_line, "    └ # Rendering"),
    "command headers should initially hide their output")
  local task_snapshot = {
    scope_id = "turn-plan",
    revision = 1,
    current = {
      { id = "task-1", title = "Inventory the repository", status = "completed" },
      { id = "task-2", title = "Trace configuration boundaries", status = "in_progress" },
      { id = "task-3", title = "Synthesize the analysis", status = "pending" },
    },
  }
  local active_plan_render = interaction_renderer.build({ {
    id = "plan-interaction",
    prompt = "/plan analyze the repo",
    kind = "plan_draft",
    state = "running",
    thought = {},
    active = { text = "Working", tool_count = 0, failed_count = 0 },
    task = task_snapshot,
  } }, {
    working_seconds = 1,
  })
  assert_true(vim.tbl_contains(active_plan_render.lines, "▸ Planning for 1s"),
    "Harness planning should use a dedicated inline summary")
  assert_true(vim.tbl_contains(active_plan_render.lines, "● Inventory the repository"),
    "active planning should show completed provider tasks inline")
  assert_true(vim.tbl_contains(active_plan_render.lines, "◐ Trace configuration boundaries"),
    "active planning should show the provider's current task inline")
  assert_true(vim.tbl_contains(active_plan_render.lines, "○ Synthesize the analysis"),
    "active planning should show pending provider tasks inline")
  assert_true(not vim.tbl_contains(active_plan_render.lines, "• Plan"),
    "provider tasks should never reappear as a tail-only plan block")
  for _, task in ipairs(task_snapshot.current) do task.status = "completed" end
  local completed_plan = interaction_renderer.build({ {
    id = "plan-interaction",
    prompt = "/plan analyze the repo",
    kind = "plan_draft",
    state = "complete",
    duration_ms = 1000,
    thought = {},
    task = task_snapshot,
  } })
  assert_true(vim.tbl_contains(completed_plan.lines, "▸ Planned for 1s"),
    "completed planning should retain its dedicated summary")
  assert_true(vim.tbl_contains(completed_plan.lines, "● Synthesize the analysis"),
    "completed provider tasks should remain inline with their interaction")
  assert_equals(#completed_plan.folds, 0,
    "provider task state should use semantic expansion instead of native folds")
  local lifecycle_collapsed = interaction_renderer.build({ {
    kind = "plan_lifecycle",
    id = "lifecycle-created",
    lifecycle = { id = "lifecycle-created", kind = "created" },
    content = "# Streaming plan\n\nUse one timeline projection.",
  } }, { expanded = {} })
  assert_equals(lifecycle_collapsed.lines[1], "▸ Plan created",
    "plan creation should render as one inline lifecycle node")
  assert_true(not vim.tbl_contains(lifecycle_collapsed.lines, "  ▸ Content"),
    "collapsed lifecycle nodes should hide artifact content")
  local lifecycle_expanded = interaction_renderer.build({ {
    kind = "plan_lifecycle",
    id = "lifecycle-created",
    lifecycle = { id = "lifecycle-created", kind = "created" },
    content = "# Streaming plan\n\nUse one timeline projection.",
  } }, { expanded = { ["plan_lifecycle:lifecycle-created"] = true } })
  assert_true(vim.tbl_contains(lifecycle_expanded.lines, "  ▸ Content"),
    "expanded lifecycle nodes should reveal artifact content")
  assert_equals(#lifecycle_expanded.markdown_ranges, 1,
    "expanded plan content should register one markdown region")
  local attributed_execution = interaction_renderer.build({ {
    kind = "plan_execution",
    id = "execution-one",
    execution = { state = "complete" },
    interaction = { {
      id = "execution-interaction",
      state = "complete",
      duration_ms = 1000,
      task = { current = { { id = "task-one", title = "Rewrite core", status = "completed" } } },
      thought = { {
        id = "thought-one",
        text = "Rewriting core.",
        task_id = "task-one",
        tool = {},
      } },
    } },
  } }, { expanded = { ["interaction:execution-one:task:task-one"] = true } })
  assert_true(vim.tbl_contains(attributed_execution.lines, "▸ Executed plan (1/1) for 1s"),
    "accepted plan execution should render as one grouped timeline node")
  assert_true(vim.tbl_contains(attributed_execution.lines, "● Rewrite core"),
    "execution should render the provider's final task presentation")
  assert_true(vim.tbl_contains(attributed_execution.lines, "↳ Rewriting core."),
    "completed tasks should expand their frozen attributed thoughts")

  local wrapped_tool = {
    id = "wrapped-command",
    kind = "command",
    status = "completed",
    title = [["C:\Program Files\PowerShell\7\pwsh.exe" -Command 'Get-ChildItem -Force | Select-Object Mode,Name']],
    output = "",
    failed = false,
  }
  local wrapped_command = interaction_renderer.build({ {
    id = "wrapped",
    prompt = "list files",
    state = "complete",
    thought = { {
      id = "wrapped:thought:1",
      text = "Listing files.",
      tool = { wrapped_tool },
      started_at_ms = 0,
      completed_at_ms = 1,
    } },
  } })
  assert_true(vim.tbl_contains(wrapped_command.lines, "  • Ran Get-ChildItem -Force | Select-Object Mode,Name"),
    "command rows should remove the PowerShell launcher wrapper")
  assert_true(vim.tbl_contains(wrapped_command.lines, "    └ no output"),
    "empty commands should retain an explicit muted result inside their fold")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.group == "DiffReviewHarnessOption"
  end), "command options should use their semantic highlight")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.group == "DiffReviewHarnessOutput"
  end), "command output should use the muted output highlight")
  assert_true(vim.iter(wrapped_command.highlights):any(function(highlight)
    return highlight.first == 2
      and highlight.last == 2 + #"•"
      and highlight.group == "DiffReviewHarnessToolSuccess"
  end), "successful command bullets should use the success highlight")
  local diff_text = table.concat({
    "diff --git a/nvim/lua/diff_review/render/diff_tree.lua b/nvim/lua/diff_review/render/diff_tree.lua",
    "--- a/nvim/lua/diff_review/render/diff_tree.lua",
    "+++ b/nvim/lua/diff_review/render/diff_tree.lua",
    "@@ -1,2 +1,2 @@",
    " local M = {}",
    "-return M",
    "+return { M }",
  }, "\n")
  local thought_diff_render = interaction_renderer.build({ {
    id = "thought-diff",
    prompt = "change the renderer",
    state = "complete",
    duration_ms = 1,
    diff_text = diff_text,
    thought = { {
      id = "thought-diff:thought:1",
      text = "Updating the renderer.",
      tool = {},
      diff_text = diff_text,
    } },
  } })
  local thought_change_line = line_number(thought_diff_render.lines, "  ▸ Changed 1 file +1 -1")
  assert_true(thought_change_line ~= nil, "thought change summaries should render as nested disclosure nodes")
  assert_true(thought_diff_render.lines[thought_change_line + 1]:match("^    Modified") ~= nil,
    "thought-level file rows should nest beneath their change summary")
  assert_true(thought_diff_render.lines[thought_change_line + 2]:match("^    @@") ~= nil,
    "thought-level hunk rows should align with their nested file header")
  local aggregate_diff_render = interaction_renderer.build({ {
    id = "aggregate-diff",
    prompt = "change the renderer",
    state = "complete",
    duration_ms = 1,
    diff_text = diff_text,
    thought = {},
  } }, { expanded = {
    ["interaction:aggregate-diff:aggregate"] = true,
    ["interaction:aggregate-diff:aggregate:file:1"] = true,
    ["interaction:aggregate-diff:aggregate:file:1:hunk:1"] = true,
  } })
  local aggregate_change_line = line_number(aggregate_diff_render.lines, "▸ Changed 1 file +1 -1")
  assert_true(aggregate_change_line ~= nil, "aggregate change summaries should remain top-level nodes")
  assert_true(aggregate_diff_render.lines[aggregate_change_line + 1]:match("^  Modified") ~= nil,
    "aggregate file rows should nest one level below their change summary")
  assert_true(aggregate_diff_render.lines[aggregate_change_line + 2]:match("^  @@") ~= nil,
    "aggregate hunk rows should align with their nested file header")
  local collapsed_aggregate_render = interaction_renderer.build({ {
    id = "aggregate-diff",
    prompt = "change the renderer",
    state = "complete",
    duration_ms = 1,
    diff_text = diff_text,
    thought = {},
    response = "Done",
  } }, { expanded = {} })
  local transaction_buf = vim.api.nvim_create_buf(false, true)
  local transaction_namespace = vim.api.nvim_create_namespace("diff_review_harness_collapse_test")
  local transaction_state = {
    transcript_buf = transaction_buf,
    transcript_win = nil,
    rendered_lines = {},
    render_rows = {},
    render_namespace = transaction_namespace,
    render_initialized = false,
    fold_installed = {},
  }
  require("diff_review.render.harness.transaction").apply(transaction_state, aggregate_diff_render, { reset = true })
  assert_true(vim.iter(vim.api.nvim_buf_get_extmarks(transaction_buf, transaction_namespace, 0, -1, { details = true }))
    :any(function(extmark) return type(extmark[4].virt_text) == "table" end),
    "expanded Harness diffs should install persistent shared gutters")
  require("diff_review.render.harness.transaction").apply(transaction_state, collapsed_aggregate_render)
  local collapsed_extmark_list = vim.api.nvim_buf_get_extmarks(
    transaction_buf, transaction_namespace, 0, -1, { details = true }
  )
  assert_true(not vim.iter(collapsed_extmark_list)
    :any(function(extmark)
      local virtual_text = extmark[4].virt_text
      return type(virtual_text) == "table" and #virtual_text > 1
    end),
    "collapsing a Harness diff should remove old gutters before deleted rows can relocate them")
  assert_true(not vim.iter(collapsed_extmark_list):any(function(extmark)
    local details = extmark[4]
    local group = tostring(details.hl_group or details.line_hl_group or "")
    return group:find("DiffReviewAdd", 1, true) or group:find("DiffReviewDelete", 1, true)
  end), "collapsing a Harness diff should remove old diff colors before suffix rows relocate")
  assert_equals(vim.api.nvim_buf_get_lines(transaction_buf, -2, -1, false)[1], "Done",
    "collapsing a Harness diff should leave the response row uncorrupted")
  local duplicate_thought_key = "interaction:duplicate-diff:thought:duplicate-thought"
  local duplicate_aggregate_key = "interaction:duplicate-diff:aggregate"
  local duplicate_expanded = {
    ["interaction:duplicate-diff:thoughts"] = true,
    [duplicate_thought_key] = true,
    [duplicate_thought_key .. ":changes"] = true,
    [duplicate_thought_key .. ":changes:file:1"] = true,
    [duplicate_thought_key .. ":changes:file:1:hunk:1"] = true,
    [duplicate_aggregate_key] = true,
    [duplicate_aggregate_key .. ":file:1"] = true,
    [duplicate_aggregate_key .. ":file:1:hunk:1"] = true,
  }
  local duplicate_interaction = { {
    id = "duplicate-diff",
    prompt = "change the renderer",
    state = "complete",
    duration_ms = 1,
    diff_text = diff_text,
    thought = { {
      id = "duplicate-thought",
      text = "Updating the renderer.",
      tool = {},
      diff_text = diff_text,
    } },
  } }
  local duplicate_render = interaction_renderer.build(duplicate_interaction, { expanded = duplicate_expanded })
  local duplicate_buf = vim.api.nvim_create_buf(false, true)
  local duplicate_win = vim.api.nvim_open_win(duplicate_buf, false, {
    relative = "editor",
    row = 1,
    col = 1,
    width = 60,
    height = 12,
    style = "minimal",
  })
  local duplicate_state = {
    transcript_buf = duplicate_buf,
    transcript_win = duplicate_win,
    rendered_lines = {},
    render_rows = {},
    render_namespace = vim.api.nvim_create_namespace("diff_review_harness_duplicate_cursor_test"),
    render_initialized = false,
    fold_installed = {},
  }
  require("diff_review.render.harness.transaction").apply(duplicate_state, duplicate_render, { reset = true })
  local aggregate_hunk_line = line_number(duplicate_render.lines, "  @@ +1 -1")
  assert_true(aggregate_hunk_line ~= nil, "duplicate diff fixture should expose the aggregate hunk header")
  vim.api.nvim_win_set_cursor(duplicate_win, { aggregate_hunk_line, 0 })
  duplicate_expanded[duplicate_aggregate_key .. ":file:1:hunk:1"] = false
  local collapsed_duplicate_render = interaction_renderer.build(duplicate_interaction, { expanded = duplicate_expanded })
  require("diff_review.render.harness.transaction").apply(duplicate_state, collapsed_duplicate_render)
  local restored_hunk_line = vim.api.nvim_win_get_cursor(duplicate_win)[1]
  assert_equals(collapsed_duplicate_render.lines[restored_hunk_line], "  @@ +1 -1",
    "collapsing one repeated diff should keep the cursor on that tree's hunk header")
  local stale_projection_render = {
    lines = { "short child timeline" },
    rows = { [999] = duplicate_state.render_rows[restored_hunk_line] },
    highlights = {},
    extmarks = {},
    diff_row_spans = {},
    folds = {},
  }
  local projection_ok, projection_error = pcall(
    require("diff_review.render.harness.transaction").apply,
    duplicate_state,
    stale_projection_render
  )
  assert_true(projection_ok, "shrinking a timeline projection should bound semantic cursor restoration: "
    .. tostring(projection_error))
  assert_equals(vim.api.nvim_win_get_cursor(duplicate_win)[1], 1,
    "shrinking a timeline projection should clamp the cursor to the rendered buffer")
  local foreign_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(foreign_buf, 0, -1, false, { "trust policy", "still editing" })
  vim.api.nvim_win_set_buf(duplicate_win, foreign_buf)
  vim.api.nvim_win_set_cursor(duplicate_win, { 2, 0 })
  local hidden_render = interaction_renderer.build(duplicate_interaction, { expanded = duplicate_expanded })
  local hidden_ok, hidden_error = pcall(
    require("diff_review.render.harness.transaction").apply,
    duplicate_state,
    hidden_render,
    { follow_tail = true }
  )
  assert_true(hidden_ok, "rendering a hidden transcript should not address the visible policy buffer: "
    .. tostring(hidden_error))
  assert_equals(vim.api.nvim_win_get_buf(duplicate_win), foreign_buf,
    "a hidden transcript render should preserve the window's current buffer")
  assert_equals(vim.api.nvim_win_get_cursor(duplicate_win)[1], 2,
    "a hidden transcript render should preserve the foreign buffer cursor")
  vim.api.nvim_win_set_buf(duplicate_win, duplicate_buf)
  vim.api.nvim_buf_delete(foreign_buf, { force = true })
  vim.api.nvim_win_close(duplicate_win, true)
  vim.api.nvim_buf_delete(duplicate_buf, { force = true })
  local diff_key = "projection:file:1"
  local hunk_key = diff_key .. ":hunk:1"
  local collapsed_diff = require("diff_review.render.diff_tree").build(diff_text, {
    key_prefix = "projection",
    expanded = {},
  })
  assert_equals(#collapsed_diff.lines, 1, "collapsed changed-file nodes should materialize only the file header")
  assert_equals(collapsed_diff.rows[1].expand_key, diff_key,
    "changed-file headers should own stable semantic expansion keys")
  local file_diff = require("diff_review.render.diff_tree").build(diff_text, {
    key_prefix = "projection",
    expanded = { [diff_key] = true },
  })
  assert_equals(#file_diff.lines, 2, "expanded files should materialize only collapsed hunk headers")
  assert_equals(file_diff.rows[2].expand_key, hunk_key,
    "hunk headers should own stable semantic expansion keys")
  local indented_file_diff = require("diff_review.render.diff_tree").build(diff_text, {
    key_prefix = "projection",
    indent = 2,
    expanded = { [diff_key] = true },
  })
  assert_true(indented_file_diff.lines[1]:match("^  Modified") ~= nil,
    "embedded file headers should apply only the caller-selected indent")
  assert_true(indented_file_diff.lines[2]:match("^  @@") ~= nil,
    "embedded hunk headers should align exactly with their file header")
  local background_extmark = require("diff_review.render.row_emitter").extmark_list(4, {
    bg = { hl_group = "DiffReviewAddBg", priority = 60 },
    highlights = {},
  }, false)[1]
  assert_equals(background_extmark.options.end_row, 5,
    "the shared row emitter should cover the complete diff row instead of a zero-length gutter span")
  assert_true(background_extmark.options.hl_eol == true,
    "the shared row emitter should extend diff backgrounds to the window edge")
  local syntax_updated = false
  local expanded_diff_options = {
    key_prefix = "projection",
    cwd = vim.uv.cwd(),
    expanded = { [diff_key] = true, [hunk_key] = true },
    on_update = function() syntax_updated = true end,
  }
  local expanded_diff = require("diff_review.render.diff_tree").build(diff_text, expanded_diff_options)
  vim.wait(1000, function() return syntax_updated end, 10)
  assert_true(syntax_updated, "Harness diff syntax should schedule an asynchronous shared-renderer upgrade")
  vim.wait(2000, function()
    expanded_diff = require("diff_review.render.diff_tree").build(diff_text, expanded_diff_options)
    for _, spans in pairs(expanded_diff.diff_row_spans or {}) do
      if vim.iter(spans.highlights or {}):any(function(highlight)
        return tostring(highlight.hl_group):match("^@") ~= nil
      end) then return true end
    end
    return false
  end, 10)
  assert_true(#expanded_diff.lines > 2, "expanded hunks should materialize their shared fancy diff rows")
  local syntax_group_list = {}
  for _, spans in pairs(expanded_diff.diff_row_spans or {}) do
    for _, highlight in ipairs(spans.highlights or {}) do
      syntax_group_list[#syntax_group_list + 1] = tostring(highlight.hl_group)
    end
  end
  assert_true(vim.iter(syntax_group_list):any(function(group)
    return group:match("^@") ~= nil
  end), "Harness diffs should retain the shared Tree-sitter syntax highlights: " .. table.concat(syntax_group_list, ","))
  local active_tree = interaction_renderer.build({ {
    id = "active-only",
    prompt = "stream",
    state = "running",
    thought = {},
    active = {
      text = "Working",
      tool_count = 3,
      failed_count = 1,
      latest_tool = {
        id = "latest-tool",
        kind = "command",
        title = "rg -n -C 3 pattern nvim/lua/plugins/highlight.lua",
        output = "line one\nline two\nline three\nline four\nline five",
        status = "completed",
        failed = false,
      },
    },
  } }, { working_seconds = 1 })
  assert_true(vim.tbl_contains(active_tree.lines, "  ▸ Running 3 tools (1 failed)"),
    "active thoughts should expose their updating tool counter")
  assert_equals(#active_tree.folds, 0, "active thoughts must not expose expandable ranges")
  assert_true(vim.tbl_contains(active_tree.lines,
    "  • Ran rg -n -C 3 pattern nvim/lua/plugins/highlight.lua"),
    "active thoughts should render the latest tool identity")
  assert_true(vim.tbl_contains(active_tree.lines, "    └ line one"),
    "active tool previews should render their first output line")
  assert_true(vim.tbl_contains(active_tree.lines, "      line four"),
    "active tool previews should render up to four output lines")
  assert_true(not vim.tbl_contains(active_tree.lines, "      line five"),
    "active tool previews should truncate output after four lines")

  local cancelled_tree = interaction_renderer.build({ {
    id = "cancelled-only",
    prompt = "stop this turn",
    state = "cancelled",
    duration_ms = 2000,
    thought = {},
  } })
  assert_true(vim.tbl_contains(cancelled_tree.lines, "▸ Cancelled after 2s"),
    "cancelled interactions should retain an explicit terminal state in the timeline")

  vim.api.nvim_set_current_win(session.harness.transcript_win)
  local tool_line = nil
  for line, row in pairs(session.harness.render_rows) do
    if row.kind == "tool" and row.tool.id == "command-one" then tool_line = line end
  end
  assert_true(tool_line ~= nil, "completed thought should index its tool row")
  assert_true(session.harness.render_rows[tool_line].expand_key ~= nil,
    "completed tool rows should own one output expansion key")
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { tool_line, 0 })
  controller.toggle_activity()
  assert_true(vim.tbl_contains(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "    └ # Rendering"), "one activity toggle should reveal the complete command output")
  controller.toggle_activity()
  assert_true(not vim.tbl_contains(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "    └ # Rendering"), "the second activity toggle should hide the complete command output")
  controller.render()
  assert_true(not vim.tbl_contains(vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "    └ # Rendering"), "collapsed completed output should survive later render transactions")

  vim.api.nvim_set_current_win(session.harness.transcript_win)
  assert_equals(#session.harness.activity_range, 0,
    "interaction nodes should avoid overlapping native fold ranges")
  assert_true(#session.harness.prompt_line >= 2, "Harness should index each user prompt for cursor-relative navigation")
  local first_prompt_line = session.harness.prompt_line[1]
  local second_prompt_line = session.harness.prompt_line[2]
  vim.api.nvim_win_set_cursor(session.harness.transcript_win, { second_prompt_line, 0 })
  vim.fn.maparg("<C-y>", "n", false, true).callback()
  assert_equals(vim.api.nvim_win_get_cursor(session.harness.transcript_win)[1], first_prompt_line,
    "Ctrl-y should find the previous prompt from the current cursor row")
  vim.fn.maparg("<C-z>", "n", false, true).callback()
  assert_equals(vim.api.nvim_win_get_cursor(session.harness.transcript_win)[1], second_prompt_line,
    "Ctrl-z should find the next prompt from the current cursor row")
  local hello_count = 0
  for _, item in ipairs(session.harness.interaction) do
    if item.prompt == "hello harness" then hello_count = hello_count + 1 end
  end
  assert_equals(hello_count, 1, "broker completion should replace the optimistic interaction")

  local prompt_count_before_steer = prompt_count
  session.harness.capability.native_steer = true
  session.harness.active_plan = { id = "steering-plan", state = "planning" }
  session.harness.busy = true
  vim.api.nvim_set_current_win(session.harness.composer_win)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false,
    { "And be sure to modify Y" })
  vim.fn.maparg("<C-q>", "i", false, true).callback()
  assert_equals(table.concat(vim.api.nvim_buf_get_lines(session.harness.composer_buf, 0, -1, false), "\n"), "",
    "Ctrl-q should clear the composer after accepting a steering prompt")
  local steer_preview = false
  for _, extmark in ipairs(vim.api.nvim_buf_get_extmarks(session.harness.composer_buf, -1, 0, -1, { details = true })) do
    local virtual_line_list = extmark[4].virt_lines or {}
    if virtual_line_list[1] and virtual_line_list[1][1]
      and virtual_line_list[1][1][1] == "• Steering active turn"
      and virtual_line_list[2] and virtual_line_list[2][1]
      and virtual_line_list[2][1][1] == "└ And be sure to modify Y"
    then
      steer_preview = true
    end
  end
  assert_true(steer_preview, "pending steering should render above HarnessInput until the provider acknowledges it")
  assert_true(vim.wait(1000, function() return #(session.harness.pending_steer or {}) == 0 end, 10),
    "provider acknowledgement should clear the pending steering indicator")
  assert_equals(steered_text_list[#steered_text_list], "And be sure to modify Y",
    "planning steering should send the follow-up to the active provider turn")
  assert_equals(prompt_count, prompt_count_before_steer,
    "planning steering should not create a second Harness interaction")
  local transcript = table.concat(
    vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "\n"
  )
  assert_true(transcript:find("▸ And be sure to modify Y", 1, true) ~= nil,
    "acknowledged steering should appear in the owning interaction timeline")
  assert_equals(#session.harness.queue, 0,
    "successful steering should not leave a queued follow-up")

  session.harness.active_wait = { interaction_id = "active", agent_count = 1 }
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false,
    { "Also inspect the transaction layer" })
  controller.submit()
  assert_true(vim.wait(1000, function()
    return steered_text_list[#steered_text_list] == "Also inspect the transaction layer"
      and #(session.harness.pending_steer or {}) == 0
  end, 10), "C-s should start immediate parent chat while Main waits on a subagent")
  assert_equals(prompt_count, prompt_count_before_steer,
    "wait chat should reuse the parent turn instead of creating a concurrent broker interaction")
  assert_equals(#session.harness.queue, 0, "admitted wait chat should not enter the follow-up queue")
  session.harness.active_wait = nil

  steer_should_fail = true
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false,
    { "Preserve the verification section" })
  controller.steer_submit()
  assert_true(vim.wait(1000, function()
    return #(session.harness.pending_steer or {}) == 0
      and session.harness.queue[#session.harness.queue] == "Preserve the verification section"
  end, 10), "a completion race should preserve failed steering as a regular queued follow-up")
  assert_equals(prompt_count, prompt_count_before_steer,
    "failed steering should remain queued while the current turn is still active")
  session.harness.queue = {}
  steer_should_fail = false

  local previous_steer_request = request_by_method["turn.steer"]
  session.harness.capability.native_steer = false
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "Unsupported steering" })
  controller.steer_submit()
  assert_equals(request_by_method["turn.steer"], previous_steer_request,
    "unsupported backends should not receive steering requests")
  assert_equals(vim.api.nvim_get_current_line(), "Unsupported steering",
    "unsupported steering should preserve the composer draft")
  session.harness.capability.native_steer = true
  session.harness.busy = false
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "No active turn" })
  controller.steer_submit()
  assert_equals(request_by_method["turn.steer"], previous_steer_request,
    "idle Harness sessions should not receive steering requests")
  assert_equals(vim.api.nvim_get_current_line(), "No active turn",
    "idle steering should preserve the composer draft")
  session.harness.active_plan = nil

  session.harness.busy = true
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "queued prompt" })
  controller.submit()
  local queue_preview = false
  for _, extmark in ipairs(vim.api.nvim_buf_get_extmarks(session.harness.composer_buf, -1, 0, -1, { details = true })) do
    local virtual_line_list = extmark[4].virt_lines or {}
    if virtual_line_list[1] and virtual_line_list[1][1]
      and virtual_line_list[1][1][1] == "• Queued follow-up inputs"
      and virtual_line_list[2] and virtual_line_list[2][1]
      and virtual_line_list[2][1][1] == "└ queued prompt"
    then
      queue_preview = true
    end
  end
  assert_true(queue_preview, "queued prompts should render above the composer")
  vim.api.nvim_win_call(session.harness.composer_win, function()
    assert_equals(vim.fn.winsaveview().topfill, 2,
      "queued prompt virtual lines should be included in the visible composer viewport")
  end)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "displaced draft" })
  controller.edit_last_queued()
  assert_equals(table.concat(vim.api.nvim_buf_get_lines(session.harness.composer_buf, 0, -1, false), "\n"), "queued prompt", "edit queued should restore newest prompt")
  assert_equals(session.harness.queue[#session.harness.queue], "displaced draft", "edit queued should preserve an existing draft")
  session.harness.queue = {}
  session.harness.busy = false

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/plan choose a migration" })
  controller.submit()
  local question_view = require("diff_review.views.harness.plan_question")
  assert_true(vim.wait(2000, question_view.is_open, 10), "planning questions should open a custom float")
  local question_state = question_view._state_for_test()
  local question_config = vim.api.nvim_win_get_config(question_state.win)
  assert_equals(question_config.relative, "editor", "planning questions should overlay the complete Harness surface")
  assert_true(vim.inspect(question_config.title):find("1/3", 1, true) ~= nil,
    "planning question title should show sequence progress")
  assert_true(vim.fn.maparg("a", "n", false, true).callback ~= nil,
    "Ask should always own the a key")
  vim.fn.maparg("q", "n", false, true).callback()
  assert_true(not question_view.is_open(), "q should hide the question float without resolving it")
  controller.render()
  assert_true(vim.tbl_contains(
    vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false),
    "  Waiting for input (press oe)"
  ), "closed planning questions should advertise the configured reopen key in Timeline Status")
  vim.api.nvim_set_current_win(session.harness.transcript_win)
  vim.fn.maparg("oe", "n", false, true).callback()
  assert_true(vim.wait(1000, question_view.is_open, 10), "oe should reopen durable planning elicitation state")
  question_state = question_view._state_for_test()
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  assert_equals(question_state.input_kind, "feedback", "Tab should open the attached feedback editor")
  assert_true(vim.api.nvim_win_is_valid(question_state.input_win), "feedback editor should own a separate float")
  vim.api.nvim_buf_set_lines(question_state.input_buf, 0, -1, false,
    { "Preserve compatibility for one release" })
  vim.fn.maparg("<C-s>", "i", false, true).callback()
  assert_true(vim.wait(1000, function()
    return active_plan.elicitation.current_index == 1
  end, 10), "inline feedback should commit the selected option")
  assert_true(vim.inspect(vim.api.nvim_win_get_config(question_state.win).title):find(
    "2/3", 1, true
  ) ~= nil, "the float should advance without reopening")
  vim.fn.maparg("o", "n", false, true).callback()
  assert_equals(question_state.input_kind, nil, "Other quick key should only highlight its row")
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_equals(question_state.input_kind, "other", "Enter should open Other's free-form input row")
  vim.fn.maparg("go", "n", false, true).callback()
  vim.fn.maparg("<Right>", "n", false, true).callback()
  assert_equals(question_state.state.page_index, 3, "Right should preview the next question")
  assert_equals(active_plan.elicitation.current_index, 1, "question navigation should not submit an answer")
  vim.fn.maparg("<Left>", "n", false, true).callback()
  vim.fn.maparg("n", "n", false, true).callback()
  assert_equals(active_plan.elicitation.current_index, 1, "direct choice keys should not commit")
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_true(vim.wait(1000, function() return active_plan.elicitation.current_index == 2 end, 10),
    "Enter should commit the highlighted choice without feedback")
  vim.fn.maparg("a", "n", false, true).callback()
  assert_equals(question_state.input_kind, nil, "a should highlight Ask without opening an editor")
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_equals(question_state.input_kind, "ask", "Enter should open the Ask input below its row")
  vim.api.nvim_buf_set_lines(question_state.input_buf, 0, -1, false, { "Why does full verification matter?" })
  vim.fn.maparg("<C-s>", "i", false, true).callback()
  assert_true(vim.wait(2000, function()
    return request_by_method["question.ask"] ~= nil and not session.harness.busy
  end, 10), "Ask should clarify without consuming the pending question")
  assert_true(not question_view.is_open(), "clarification chat should leave the current question revision dismissed")
  vim.api.nvim_set_current_win(session.harness.transcript_win)
  vim.fn.maparg("oe", "n", false, true).callback()
  assert_true(vim.wait(1000, question_view.is_open, 10), "oe should reopen the unchanged question revision")
  question_state = question_view._state_for_test()
  assert_equals(active_plan.elicitation.current_index, 2, "clarification should preserve the current question")
  vim.fn.maparg("n", "n", false, true).callback()
  assert_equals(active_plan.elicitation.current_index, 2, "direct choice keys should still wait for Enter")
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_true(vim.wait(1000, function()
    return active_plan.elicitation.current_index == 3
  end, 10), "direct choice keys should commit the current question")
  local review_submit_mapping = vim.api.nvim_buf_call(question_state.buf, function()
    return vim.fn.maparg("<C-s>", "n", false, true)
  end)
  assert_equals(review_submit_mapping.callback, nil,
    "the final answer review should not bind Ctrl-s")
  vim.fn.maparg("y", "n", false, true).callback()
  assert_true(vim.wait(2000, function() return active_plan.state == "awaiting_review" end, 10),
    "y should explicitly continue planning with reviewed answers")
  assert_equals(active_plan.state, "awaiting_review", "answering a planning question should produce the review artifact")
  assert_equals(active_plan.elicitation, nil, "continued planning should clear durable elicitation state")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/plan inspect the feature" })
  controller.submit()
  assert_true(vim.wait(2000, function() return #(session.harness.artifact or {}) == 1 end, 10),
    "completed planning should publish a session artifact")
  assert_true(vim.api.nvim_buf_get_name(0) ~= plan_path,
    "completed planning should not open PlanReview automatically")
  controller.open_artifact_picker()
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_true(vim.wait(2000, function() return vim.api.nvim_buf_get_name(0) == plan_path end, 10),
    "artifact selection did not open the physical plan")
  assert_equals(vim.bo.filetype, "markdown", "PlanReview should use markdown")
  assert_true(vim.fn.maparg("oY", "n", false, true).callback ~= nil, "PlanReview accept mapping missing")
  assert_true(vim.fn.maparg("oN", "n", false, true).callback ~= nil, "PlanReview request-changes mapping missing")
  vim.fn.maparg("oY", "n", false, true).callback()
  assert_true(vim.wait(2000, function() return request_by_method["plan.accept"] ~= nil end, 10), "PlanReview did not submit acceptance")
  assert_equals(#request_by_method["plan.accept"].params.digest, 64, "PlanReview should submit the exact saved digest")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/plan revise the feature" })
  controller.submit()
  assert_true(vim.wait(2000, function() return not session.harness.busy end, 10), "second planning turn did not complete")
  controller.open_artifact_picker()
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_true(vim.wait(2000, function() return vim.api.nvim_buf_get_name(0) == plan_path end, 10),
    "second artifact selection did not open PlanReview")
  local original_input = vim.ui.input
  vim.ui.input = function(options, callback)
    if options.prompt == "Plan comment: " then callback("Name the exact module")
    else callback("Include explicit integration coverage") end
  end
  vim.fn.maparg("C", "n", false, true).callback()
  vim.fn.maparg("oN", "n", false, true).callback()
  vim.ui.input = original_input
  assert_true(vim.wait(2000, function() return request_by_method["plan.request_changes"] ~= nil end, 10), "PlanReview did not request changes")
  assert_equals(request_by_method["plan.request_changes"].params.annotations[1].body, "Name the exact module",
    "PlanReview should serialize anchored annotations")
  assert_equals(request_by_method["plan.request_changes"].params.comment, "Include explicit integration coverage",
    "PlanReview should serialize the optional overall review comment")
  vim.cmd("tabclose")

  require("diff_review.views.interactions").open()
  assert_true(vim.wait(2000, function() return vim.bo.filetype == "DiffReviewInteractions" end, 10), "Interactions did not open")
  local interaction_text = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert_true(interaction_text:find("Interaction 1", 1, true) ~= nil, "Interactions should render interaction headers")
  assert_true(interaction_text:find("return 'after'", 1, true) ~= nil, "Interactions should render parsed diff lines")
  local comment_rendered = false
  for _, extmark in ipairs(vim.api.nvim_buf_get_extmarks(0, -1, 0, -1, { details = true })) do
    for _, virtual_line in ipairs(extmark[4].virt_lines or {}) do
      for _, segment in ipairs(virtual_line) do
        if segment[1]:find("Persisted review note", 1, true) then comment_rendered = true end
      end
    end
  end
  assert_true(comment_rendered, "Interactions should restore persisted review comments")
  assert_true(vim.fn.maparg("R", "n", false, true).callback ~= nil, "checkpointed sessions should expose rollback")
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  assert_equals(vim.fn.foldclosed(1), 1, "interaction folds should begin on their visible header")
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  vim.cmd("tabclose")

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/rename Architecture review" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return request_by_method["session.rename"] ~= nil and session.harness.session.name == "Architecture review"
  end, 10), "/rename should persist the active session name")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/sessions" })
  controller.submit()
  local session_picker = require("diff_review.views.picker")
  assert_true(vim.wait(2000, function() return session_picker.is_open("sessions") end, 10),
    "session search picker did not open")
  local session_picker_state = session_picker._state_for_test()
  local session_page = require("diff_review.views.picker.state").page(session_picker_state.state, session_picker_state.spec)
  assert_equals(session_page.option_list[1].label, "Architecture review",
    "session search should expose the durable name as its label")
  session_picker.close(true)

  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/rename" })
  controller.submit()
  assert_true(vim.wait(2000, function() return session.harness.session.name == "" end, 10),
    "/rename without a name should clear the active session name")
  controller.open_session_picker()
  assert_true(vim.wait(2000, function() return session_picker.is_open("sessions") end, 10),
    "session search did not reopen for unnamed-session verification")
  session_picker_state = session_picker._state_for_test()
  session_page = require("diff_review.views.picker.state").page(session_picker_state.state, session_picker_state.spec)
  assert_equals(session_page.option_list[1].label, "[unnamed]",
    "session search should render empty names as [unnamed]")
  session_picker.close(true)
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/model selected-model" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return request_by_method["session.configure"] ~= nil and session.harness.session.model == "selected-model"
  end, 10), "/model should configure the active session")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/effort high" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return request_by_method["session.configure"] ~= nil and session.harness.session.effort == "high"
  end, 10), "/effort should configure the active session")
  assert_true(vim.wait(1000, function() return session.harness.prompt_history[1] == "/effort high" end, 10),
    "slash commands should enter shared prompt history")
  require("diff_review.views.harness").new_session()
  assert_true(vim.wait(2000, function()
    return session.harness.session.id == "session-two"
  end, 10), "new session should complete")
  assert_equals(session.harness.session.model, "selected-model", "new sessions should preserve the selected model")
  assert_equals(session.harness.session.effort, "high", "new sessions should preserve the selected effort")
  assert_equals(session.harness.prompt_history[1], "/effort high",
    "new sessions should retain the shared prompt history")
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/goal fail" })
  controller.submit()
  assert_true(vim.wait(2000, function()
    return session.harness.goal and session.harness.goal.state == "paused" and not session.harness.busy
  end, 10), "failed goals should reconcile to paused durable state")
  assert_equals(goal_continue_count, 0, "failed goals must not enter an automatic retry loop")

  session.harness.active_plan = nil
  session.harness.goal = { objective = "finish", state = "active" }
  controller.drain()
  assert_true(vim.wait(2000, function() return goal_continue_count == 2 and not session.harness.busy end, 10), "goal continuation should drain until a terminal guard state")
  assert_equals(session.harness.goal.state, "stalled", "goal guard should stop automatic continuation")

  local new_session_result
  client.resolve_lease_conflict("new", { session_id = "busy-session" }, function(result, resolve_error)
    assert_true(resolve_error == nil, resolve_error)
    new_session_result = result
  end)
  assert_true(vim.wait(2000, function() return new_session_result ~= nil end, 10),
    "lease conflict should allow an independently initialized new session")
  assert_equals(request_by_method.initialize.params.lease_conflict_action, "new",
    "lease recovery should bypass the busy latest-session lease")

  local fork_result
  client.resolve_lease_conflict("fork", { session_id = "busy-session" }, function(result, resolve_error)
    assert_true(resolve_error == nil, resolve_error)
    fork_result = result
  end)
  assert_true(vim.wait(2000, function() return fork_result ~= nil end, 10),
    "lease conflict should fork through an independently leased broker")
  assert_equals(request_by_method["session.fork"].params.session_id, "busy-session",
    "lease recovery should fork the conflicted source session")
  assert_equals(fork_result.session.id, "session-fork", "lease recovery should return the fork snapshot")
  local harness_view = require("diff_review.views.harness")
  assert_true(not vim.iter(harness_view.lease_conflict_options({ native_fork = false })):any(function(option)
    return option.value == "fork"
  end), "lease recovery should hide fork when the backend cannot fork")
  assert_true(vim.iter(harness_view.lease_conflict_options({ native_fork = true })):any(function(option)
    return option.value == "fork"
  end), "lease recovery should expose fork when the persisted backend capability allows it")

  session.harness.busy = false
  vim.api.nvim_buf_set_lines(session.harness.composer_buf, 0, -1, false, { "/backend" })
  controller.submit()
  local backend_picker = require("diff_review.views.picker")._state_for_test()
  assert_true(backend_picker ~= nil, "/backend should open the shared picker")
  assert_equals(backend_picker.spec.page_list[1].title, "Select Harness backend",
    "the backend picker should identify its purpose")
  assert_equals(#backend_picker.spec.page_list[1].option_list, 2,
    "the backend picker should hide non-user test backends")
  vim.fn.maparg("<CR>", "n", false, true).callback()
  assert_true(vim.wait(2000, function()
    return require("diff_review.infra.config").options.harness.backend == "codex"
      and request_by_method.initialize.params.backend.kind == "codex"
      and session.harness.session.backend == "codex"
  end, 10), "selecting a backend should restart the broker with that backend")
  assert_equals(backend_preference.load(require("diff_review.infra.config").options.harness.backends, "copilot"),
    "codex", "a successful backend switch should persist the selected backend")
end)

controller._set_render_observer_for_test(nil)
client._reset_for_test()
builder._reset_for_test()
require("diff_review.harness.backend_preference")._set_path_for_test(nil)
pcall(vim.fn.delete, test_root, "rf")

if not ok then
  pcall(vim.cmd, "stopinsert")
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then vim.bo[buf].modified = false end
  end
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
