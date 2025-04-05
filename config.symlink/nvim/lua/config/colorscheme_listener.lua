-- On SIGUSR1, reload config.colorscheme and re-initialize lualine. This facilitates the switch from dark to light
-- and vice versa.
vim.api.nvim_create_autocmd("Signal", {
	pattern = { "SIGUSR1" },
	callback = function()
		-- Hack to ensure that config.colorscheme can actually be reloaded.
		package.loaded["config.colorscheme"] = nil
		require("config.colorscheme")

		-- Call lualine.setup() again to force it to pick up the changed colorscheme.
		local status_ok, lualine = pcall(require, "lualine")
		if status_ok then
			lualine.setup()
		end

		vim.cmd("redraw!")
	end,
})
