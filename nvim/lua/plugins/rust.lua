return {
  {
    'saecki/crates.nvim',
    event = { "BufRead Cargo.toml" },    
    tag = 'stable',
    keys = {
      {
        ".",
        function ()
          require("crates").show_popup() 
          require("crates").focus_popup()
        end,
        mode = { "n", "x" },
        desc = "Open crates popup",
      }
    },
    config = function()
      require("crates").setup {
        smart_insert = true,
        insert_closing_quote = true,
        autoload = true,
        autoupdate = true,
        autoupdate_throttle = 250,
        loading_indicator = true,
        search_indicator = true,
        date_format = "%Y-%m-%d",
        thousands_separator = ".",
        notification_title = "crates.nvim",
        curl_args = { "-sL", "--retry", "1" },
        max_parallel_requests = 80,
        open_programs = { "xdg-open", "open" },
        expand_crate_moves_cursor = true,
        enable_update_available_warning = true,
        on_attach = function(bufnr) end,
        highlight = {
          searching = "CratesNvimSearching",
          loading = "CratesNvimLoading",
          version = "CratesNvimVersion",
          prerelease = "CratesNvimPreRelease",
          yanked = "CratesNvimYanked",
          nomatch = "CratesNvimNoMatch",
          upgrade = "CratesNvimUpgrade",
          error = "CratesNvimError",
        },
        popup = {
          autofocus = false,
          hide_on_select = false,
          copy_register = '"',
          style = "minimal",
          border = "none",
          show_version_date = false,
          show_dependency_version = true,
          max_height = 30,
          min_width = 20,
          padding = 1,
          highlight = {
            title = "CratesNvimPopupTitle",
            pill_text = "CratesNvimPopupPillText",
            pill_border = "CratesNvimPopupPillBorder",
            description = "CratesNvimPopupDescription",
            created_label = "CratesNvimPopupLabel",
            created = "CratesNvimPopupValue",
            updated_label = "CratesNvimPopupLabel",
            updated = "CratesNvimPopupValue",
            downloads_label = "CratesNvimPopupLabel",
            downloads = "CratesNvimPopupValue",
            homepage_label = "CratesNvimPopupLabel",
            homepage = "CratesNvimPopupUrl",
            repository_label = "CratesNvimPopupLabel",
            repository = "CratesNvimPopupUrl",
            documentation_label = "CratesNvimPopupLabel",
            documentation = "CratesNvimPopupUrl",
            crates_io_label = "CratesNvimPopupLabel",
            crates_io = "CratesNvimPopupUrl",
            lib_rs_label = "CratesNvimPopupLabel",
            lib_rs = "CratesNvimPopupUrl",
            categories_label = "CratesNvimPopupLabel",
            keywords_label = "CratesNvimPopupLabel",
            version = "CratesNvimPopupVersion",
            prerelease = "CratesNvimPopupPreRelease",
            yanked = "CratesNvimPopupYanked",
            version_date = "CratesNvimPopupVersionDate",
            feature = "CratesNvimPopupFeature",
            enabled = "CratesNvimPopupEnabled",
            transitive = "CratesNvimPopupTransitive",
            normal_dependencies_title = "CratesNvimPopupNormalDependenciesTitle",
            build_dependencies_title = "CratesNvimPopupBuildDependenciesTitle",
            dev_dependencies_title = "CratesNvimPopupDevDependenciesTitle",
            dependency = "CratesNvimPopupDependency",
            optional = "CratesNvimPopupOptional",
            dependency_version = "CratesNvimPopupDependencyVersion",
            loading = "CratesNvimPopupLoading",
          },
          keys = {
            hide = { "q", "<esc>" },
            open_url = { "<cr>" },
            select = { "<cr>" },
            select_alt = { "s" },
            toggle_feature = { "<cr>" },
            copy_value = { "yy" },
            goto_item = { "gd", "K", "<C-LeftMouse>" },
            jump_forward = { "<c-i>" },
            jump_back = { "<c-o>", "<C-RightMouse>" },
          },
        },
        completion = {
          insert_closing_quote = true,
          text = {
            prerelease = " ? pre-release ",
            yanked = " ? yanked ",
          },
          cmp = {
            enabled = false,
            use_custom_kind = true,
            kind_text = {
              version = "Version",
              feature = "Feature",
            },
            kind_highlight = {
              version = "CmpItemKindVersion",
              feature = "CmpItemKindFeature",
            },
          },
          coq = {
            enabled = false,
            name = "crates.nvim",
          },
          crates = {
            enabled = false,
            min_chars = 3,
            max_results = 8,
          },
        },
        null_ls = {
          enabled = false,
          name = "crates.nvim",
        },
        lsp = {
          enabled = true,
          name = "crates.nvim",
          on_attach = function(client, bufnr) end,
          actions = true,
          completion = true,
          hover = true,
        },
      }
    end
  },
  {
    'mrcjkb/rustaceanvim',
    version = '^4', -- Recommended
    lazy = false, -- This plugin is already lazy
    config = function ()
      vim.g.rustaceanvim = {
        -- Plugin configuration
        tools = {
          enable_clippy = false
        },
        -- LSP configuration
        server = {
          on_attach = function(client, bufnr)
            local format_sync_grp = vim.api.nvim_create_augroup("RustaceanFormat", {})

            vim.api.nvim_create_autocmd("BufWritePre", {
              buffer = bufnr,
              callback = function() vim.lsp.buf.format() end,
              group = format_sync_grp,
            }) 
            -- you can also put keymaps in here
          end,
          default_settings = {
            ['rust-analyzer'] = {
              ["cargo"] = {
                ["allFeatures"] = true,
                ["extraEnv"] = {
                  ["CARGO_TARGET_DIR"] = "target/check",
                  -- ["RUSTFLAGS"] = "-Zthreads=8 -Zshare-generics=n"
                }
              },
              -- Doesn't work... not supported in nvim? 
              -- completion = {
              --   snippets = {
              --     custom = {
              --       ["myprint!"] = {
              --         ["postfix"] = "myp",
              --         ["body"] = {
              --           "println!(\"$0\", ${receiver});"
              --         },
              --         ["description"] = "println!()",
              --         ["scope"] = "expr"
              --       }
              --
              --     }
              --   }
              -- },
              -- checkOnSave = false,
            },
          },
        },
        -- DAP configuration
        dap = {
        },
      }
    end 
  }
}
