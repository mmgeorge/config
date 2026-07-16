local agent_picker = require("diff_review.views.harness.agent_picker")
local spawn_picker = require("diff_review.views.harness.spawn_picker")

local origin_win = vim.api.nvim_get_current_win()
local now_ms = os.time() * 1000
local state = {
  selected_agent_run_id = nil,
  agent_live = {},
  agent = {
    definition = {
      { name = "explorer", description = "Read-oriented repository explorer" },
      { name = "local-code-explorer", description = "Semantic local-code investigator" },
      { name = "worker", description = "Implementation-focused child agent" },
    },
    run = {
      {
        id = "running-explorer",
        definition = "explorer",
        status = "running",
        created_at_ms = now_ms - 10000,
        updated_at_ms = now_ms,
      },
      {
        id = "done-explorer",
        definition = "explorer",
        status = "completed",
        created_at_ms = now_ms - 65000,
        updated_at_ms = now_ms - 5000,
      },
    },
    turn = {
      {
        agent_run_id = "running-explorer",
        interaction = {
          id = "running-turn",
          node_list = {
            {
              segment = {
                thought = {
                  {
                    text = "Inspecting",
                    tool = {
                      { id = "one", status = "completed", output = "done" },
                      { id = "two", status = "failed", output = "denied" },
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  },
}

local host = { window_list = { origin_win }, control_win = origin_win }

local function open_agent_picker()
  agent_picker.open({
    host = host,
    state_provider = function() return state end,
    on_select = function(run_id)
      state.selected_agent_run_id = run_id
      vim.notify(run_id and ("Selected " .. run_id) or "Selected Main")
    end,
  })
end

local function open_spawn_picker()
  spawn_picker.open({
    host = host,
    definition_list = state.agent.definition,
    on_spawn = function(definition, task)
      vim.notify(("Spawn %s: %s"):format(definition, task))
    end,
  })
end

vim.keymap.set("n", "<F5>", open_agent_picker, { desc = "Open manual agent timeline picker" })
vim.keymap.set("n", "<F6>", open_spawn_picker, { desc = "Open manual spawn picker" })

open_agent_picker()
