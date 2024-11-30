-- This config is designed to set up and use LSPs automatically, so long as the
-- required server is installed. If the server is installed, no additional
-- config is required unless modifying behaviour specific to that server.
-- Additionally, installation is made trivial by Mason's :LspInstall, which
-- installs a relevant server for the current filetype.

-- TODO disable ensure installed. I think lazyvim is setting it (so mabye do it in
-- override instead)

-- local lspCapabilities = vim.lsp.protocol.make_client_capabilities()
-- -- Enable folding (for nvim-ufo)
-- lspCapabilities.textDocument.foldingRange = {
-- 	dynamicRegistration = false,
-- 	lineFoldingOnly = true,
-- }

-- Diganostic config. I guess this technically doesn't need to be in the lspconfig module,
-- but I don't really use diagnostics anywhere else... do I?
vim.diagnostic.config({
		signs = false,
		virtual_text = {
			prefix = function() return "" end,
			format = function(diagnostic)
				if DiagnosticVirtualTextPrefix then
					return DiagnosticVirtualTextPrefix(diagnostic)
				end
				symbols = {
					[vim.diagnostic.severity.ERROR] = "E",
					[vim.diagnostic.severity.WARN] = "W",
					[vim.diagnostic.severity.INFO] = "I",
					[vim.diagnostic.severity.HINT] = "H",
				}
				return symbols[diagnostic.severity]
			end,
			severity_sort = true,
			spacing = 3,
			underline = false,
		},
	})

local diagnosticsConfig = vim.diagnostic.config()
vim.g.diagnosticsEnabled = true
function ToggleDiagnostics()
	if not vim.g.diagnosticsEnabled then
		vim.diagnostic.config(diagnosticsConfig)
		vim.g.diagnosticsEnabled = true
	else
		vim.diagnostic.config({
			virtual_text = false,
			signs = false,
			float = false,
			update_in_insert = false,
			severity_sort = false,
			underline = false,
		})
		vim.g.diagnosticsEnabled = false
	end
end
vim.keymap.set("n", "yod", ToggleDiagnostics, { noremap = true, silent = true })

return {
	{
		"neovim/nvim-lspconfig",
		-- Lazyvim override can't happen in config because it is too big to replace.
		-- Abusing the opts function to set things up on load instead.
		opts = function(_, opts)
			local maps = vim.g.IDE_mappings

			-- Mappings.
			-- See `:help vim.lsp.*` for documentation on any of the below functions
			local lsp_nbufmaps = {
				[maps.implementation] = "declaration()",
				[maps.implementation2] = "declaration()",
				[maps.definition] = "definition()",
				[maps.definition2] = "definition()",
				[maps.documentation] = "hover()",
				[maps.documentation2] = "hover()",
				[maps.implementation] = "implementation()",
				[maps.implementation2] = "implementation()",
				[maps.typeDefinition] = "type_definition()",
				[maps.typeDefinition2] = "type_definition()",
				[maps.rename] = "rename()",
				[maps.codeAction] = "code_action()",
				[maps.references] = "references()",
				[maps.references2] = "references()",
				[maps.reformat] = "format()",
				-- [maps.refactor] = "format()",
				-- ['<space>wa'] = 'add_workspace_folder()',
				-- ['<space>wr'] = 'remove_workspace_folder()',
				--  ['<space>wl'] = '<cmd>lua print(vim.inspect(vim.lsp.list_workspace_folders))()',
			}

			local diagnostic_nbufmaps = {
				[maps.listErrs] = "set_loclist()",
				[maps.diagnostic] = "open_float(nil, {focus=false, scope='cursor',})",
				[maps.diagnosticPrev] = "goto_prev()",
				[maps.diagnosticNext] = "goto_next()",
			}

			-- Use an on_attach function to only map keys etc after the language server
			-- attaches to the current buffer
			local on_attach = function(client, bufnr)
				vim.api.nvim_create_augroup("lsp_on_attach", { clear = true })

				--Enable completion triggered by <c-x><c-o>
				vim.bo.omnifunc = "v:lua.vim.lsp.omnifunc"

				-- Auto-show diagnostics on pause over them.
				vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
					group = "lsp_on_attach",
					buffer = bufnr,
					callback = function()
						if vim.g.diagnosticsEnabled then
							vim.diagnostic.open_float(nil, { focus = false })
							-- Enable underline if the current line as a diagnostic
							if #vim.diagnostic.get(bufnr, { lnum = vim.fn.line(".") }) > 0 then
								vim.diagnostic.config({ underline = true })
							end
						end
					end,
				})
				-- Hide again on move
				vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
					group = "lsp_on_attach",
					buffer = bufnr,
					callback = function()
						vim.diagnostic.config({ underline = false })
					end,
				})

				-- Mappings.
				local tables = {
					["vim.lsp.buf."] = lsp_nbufmaps,
					["vim.diagnostic."] = diagnostic_nbufmaps,
				}

				for prefix, table in pairs(tables) do
					require("my/utils").map_table_with_prefix(table, "<cmd>lua " .. prefix, "n", { buffer = bufnr })
				end
				if client.server_capabilities.workspaceSymbolProvider then
					vim.keymap.set('n', maps.FuzzySymbols,
					function() require('telescope.builtin').lsp_document_symbols() end,
					{ buffer = bufnr})
				end
				if client.server_capabilities.documentSymbolProvider then
					vim.keymap.set('n', maps.FuzzySymbolsWorkspace,
					function() require('telescope.builtin').lsp_workspace_symbols() end,
					{ buffer = bufnr})
				end
			end

			vim.api.nvim_create_autocmd("LspAttach", {
					callback = function(args)
						local bufnr = args.buf
						local client = vim.lsp.get_client_by_id(args.data.client_id)
						on_attach(client, bufnr)
					end,
				})

			opts.main_attachment_callback = on_attach
			-- Override lazyvim
			-- opts.diagnostics = {signs = {}}
			-- Ensure file that triggers lspconfig to load also triggers
			-- attachment.
			vim.fn.timer_start(1000,
				function() vim.cmd.doautocmd("bufread") end)
		end,

		-- Letting lazyvim handle config
		-- config = function()
		-- 	local lsp_flags = {
		-- 		debounce_text_changes = 200,
		-- 	}

		-- 	require("lspconfig")
		-- 	require("mason").setup()

		-- 	if IsPluginUsed("nvim-cmp") then
		-- 		Nvim_cmp_capabilities = require("cmp_nvim_lsp").default_capabilities()

		-- 		Nvim_cmp_capabilities.textDocument.completion.completionItem.snippetSupport = true
		-- 		Nvim_cmp_capabilities.textDocument.completion.completionItem.resolveSupport = {
		-- 			properties = {
		-- 				"documentation",
		-- 				"detail",
		-- 				"additionalTextEdits",
		-- 			},
		-- 		}
		-- 	end

		-- 	local lsp_installer = require("mason-lspconfig")

		-- 	lsp_installer.setup({
		-- 		-- ensure_installed = { "pylsp" },
		-- 	})

		-- 	local default_handler = function(server_name)
		-- 		require("lspconfig")[server_name].setup({
		-- 			on_attach = on_attach,
		-- 			capabilities = Nvim_cmp_capabilities,
		-- 			flags = lsp_flags,
		-- 		})
		-- 	end

		-- 	-- Register a handler that will be called for each installed server when it's
		-- 	-- ready (i.e. when installation is finished or if the server is already
		-- 	-- installed).
		-- 	lsp_installer.setup_handlers({
		-- 		default_handler,

		-- 		-- (optional) Customize the options passed to the server
		-- 		["pylsp"] = function()
		-- 			default_handler("pylsp")
		-- 			-- vim.cmd("UnPlug 'davidhalter/jedi-vim'")
		-- 			vim.cmd([[let g:jedi#auto_initialization = 0]])
		-- 			vim.cmd([[let g:pymode = 0]])
		-- 			vim.cmd([[silent! au! myPymode"]])
		-- 		end,

		-- 		["lua_ls"] = function()
		-- 			local server_name = "lua_ls"
		-- 			require("lspconfig")[server_name].setup({
		-- 				on_attach = opts.main_attachment_callback,
		-- 				capabilities = Nvim_cmp_capabilities,
		-- 				settings = {
		-- 					-- Get the language server to recognize the `vim` global
		-- 					Lua = { diagnostics = { globals = { "vim" } } },
		-- 				},
		-- 			})
		-- 		end,
		-- 	})

		-- 	-- Instead of showing signs, change the colour of the numbercolumn.
		-- 	-- vim.fn.sign_define("LspDiagnosticsSignError", {text = "", numhl = "LspDiagnosticsDefaultError"})
		-- 	-- vim.fn.sign_define("LspDiagnosticsSignWarning", {text = "", numhl = "LspDiagnosticsDefaultWarning"})
		-- 	-- vim.fn.sign_define("LspDiagnosticsSignInformation", {text = "", numhl = "LspDiagnosticsDefaultInformation"})
		-- 	-- vim.fn.sign_define("LspDiagnosticsSignHint", {text = "", numhl = "LspDiagnosticsDefaultHint"})

		-- end,

		dependencies = {
			-- Relying on lazyvim for these
			-- { "mason.nvim"},
			-- { "williamboman/mason-lspconfig.nvim"},
			-- Create appropriate colours for old colourschemes
			{ "https://github.com/folke/lsp-colors.nvim.git"},
		}

	},

	{
		"folke/trouble.nvim",
		action_keys = { -- key mappings for actions in the trouble list
			toggle_fold = { "zA", "za", "<BS>" },
		},
		auto_close = true,
		use_diagnostic_signs = true, -- enabling this will use the signs defined in your lsp client
		icons = function()
			if os.getenv("USENF") == 1 then
				return true
			else
				return false
			end
		end,
		event="LspAttach",
	},
	
	-- {
	-- 	"hinell/lsp-timeout.nvim",
	-- 	dependencies={"neovim/nvim-lspconfig"},
	-- 	event="LspAttach",
	-- },

}
