require("lazy").setup({
  spec = {
    { "LazyVim/LazyVim", import = "lazyvim.plugins" },
    { "nvim-telescope/telescope-fzf-native.nvim", enabled = true },
    { "williamboman/mason-lspconfig.nvim", enabled = false },
    { "williamboman/mason.nvim", enabled = false },
    { "jaybaby/mason-nvim-dap.nvim", enabled = false },
    { import = "plugins" },
    {
      "nvim-treesitter/nvim-treesitter",
      init = function()
        vim.opt.rtp:prepend(vim.g.treesitter_path)
      end,
      opts = { auto_install = false, ensure_installed = {} },
    },
  },
  dev = {
    path = vim.g.lazy_path,
    patterns = { "." },
    fallback = false,
  },
  defaults = {
    lazy = false,
    version = false,
  },
  performance = {
    rtp = {
      paths = { vim.g.config_path },
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
})
