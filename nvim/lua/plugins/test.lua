local function result_suffix(id)
  return id:match("^[^:]+::(.+)$") or id
end

local function ends_with_test_suffix(value, suffix)
  return value == suffix or value:sub(-#suffix - 2) == "::" .. suffix
end

local function remap_results_to_positions(results, tree)
  local test_ids = {}
  local suffix_to_ids = {}

  for _, node in tree:iter_nodes() do
    local pos = node:data()
    if pos.type == "test" then
      local suffix = result_suffix(pos.id)
      test_ids[#test_ids + 1] = { id = pos.id, suffix = suffix }
      suffix_to_ids[suffix] = suffix_to_ids[suffix] or {}
      table.insert(suffix_to_ids[suffix], pos.id)
    end
  end

  local function unique_match(matches)
    if #matches == 1 then
      return matches[1]
    end
  end

  local function find_position_id(result_id)
    if tree:get_key(result_id) then
      return result_id
    end

    local suffix = result_suffix(result_id)
    local direct = unique_match(suffix_to_ids[suffix] or {})
    if direct then
      return direct
    end

    local matches = {}
    for _, test_id in ipairs(test_ids) do
      if ends_with_test_suffix(suffix, test_id.suffix) or ends_with_test_suffix(test_id.suffix, suffix) then
        table.insert(matches, test_id.id)
      end
    end

    return unique_match(matches)
  end

  local mapped = {}
  for id, result in pairs(results) do
    mapped[find_position_id(id) or id] = result
  end

  return mapped
end

local function vitest_adapter()
  local adapter = require("neotest-vitest")({
    vitestCommand = "pnpm vitest run --browser.headless",
  })

  local function emit_review_results(spec, results, phase)
    local review = spec and spec.context and spec.context.screenshot_review
    if not review then
      return
    end

    local screenshot_review = require("screenshot_review")
    local failures = screenshot_review.extract_failures_from_results(results, { cwd = spec.cwd or review.cwd or vim.uv.cwd() })

    if phase == "complete" then
      screenshot_review.emit_run_complete(review, failures)
    else
      screenshot_review.emit_run_update(review, failures)
    end
  end

  local base_results = adapter.results
  adapter.results = function(spec, process_result, tree)
    local results = remap_results_to_positions(base_results(spec, process_result, tree), tree)
    emit_review_results(spec, results, "complete")
    return results
  end

  local query = [[
    ((call_expression
      function: (identifier) @func_name (#any-of? @func_name "describe" "suite")
      arguments: (arguments . (string (string_fragment) @namespace.name))
    )) @namespace.definition
    ((call_expression
      function: (member_expression
        object: (identifier) @func_name (#any-of? @func_name "describe" "suite")
      )
      arguments: (arguments . (string (string_fragment) @namespace.name))
    )) @namespace.definition
    ((call_expression
      function: (identifier) @func_name (#any-of? @func_name "it" "test")
      arguments: (arguments . (string (string_fragment) @test.name))
    )) @test.definition
    ((call_expression
      function: (member_expression
        object: (identifier) @func_name (#any-of? @func_name "test" "it")
      )
      arguments: (arguments . (string (string_fragment) @test.name))
    )) @test.definition
  ]]

  adapter.discover_positions = function(path)
    return require("neotest.lib").treesitter.parse_positions(path, query, { nested_tests = true })
  end

  local base_build_spec = adapter.build_spec
  adapter.build_spec = function(args)
    local spec = base_build_spec(args)
    if spec then
      spec.context.tree = args.tree
      spec.context.screenshot_review = args.screenshot_review

      if spec.context.screenshot_review then
        require("screenshot_review").emit_run_start(spec.context.screenshot_review)
      end

      if spec.stream then
        local base_stream = spec.stream
        spec.stream = function(...)
          local next_results = base_stream(...)
          return function()
            local results = next_results()
            if not results then
              return nil
            end

            local mapped = remap_results_to_positions(results, args.tree)
            emit_review_results(spec, mapped, "update")
            return mapped
          end
        end
      end
    end
    return spec
  end

  return adapter
end

return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "marilari88/neotest-vitest",
      -- "mrcjkb/rustaceanvim",
    },
    cmd = {
      "ScreenshotReviewFile",
      "ScreenshotReviewNearest",
    },
    keys = {
      {
        "<leader>tn",
        function()
          require("neotest").run.run()
        end,
        desc = "Run nearest test",
      },
      {
        "<leader>tf",
        function()
          require("neotest").run.run(vim.fn.expand("%:p"))
        end,
        desc = "Run test file",
      },
      {
        "<leader>ts",
        function()
          require("neotest").summary.toggle()
        end,
        desc = "Toggle test summary",
      },
      {
        "<leader>tu",
        function()
          require("neotest").output.open({ enter = true, auto_close = true })
        end,
        desc = "Open test output",
      },
    },
    config = function()
      vim.api.nvim_create_user_command("ScreenshotReviewFile", function()
        require("screenshot_review").review_file()
      end, { desc = "Run screenshot tests for current file and open browser review" })

      vim.api.nvim_create_user_command("ScreenshotReviewNearest", function()
        require("screenshot_review").review_nearest()
      end, { desc = "Run nearest screenshot test and open browser review" })

      vim.api.nvim_create_autocmd("VimLeavePre", {
        group = vim.api.nvim_create_augroup("screenshot-review", { clear = true }),
        callback = function()
          require("screenshot_review").release_client_lease()
        end,
      })

      require("neotest").setup({
        adapters = {
          vitest_adapter(),
          -- require("rustaceanvim.neotest"),
        },
      })
    end
  }
}
