require("lazy").setup({
  spec = {
    { "LazyVim/LazyVim", import = "lazyvim.plugins" },

    -- Typescript
    { import = "lazyvim.plugins.extras.lang.typescript" },
    { import = "lazyvim.plugins.extras.linting.eslint" },
    { import = "lazyvim.plugins.extras.formatting.prettier" },
    { import = "lazyvim.plugins.extras.lang.json" },

    -- Python
    { import = "lazyvim.plugins.extras.lang.python" },
    { import = "lazyvim.plugins.extras.formatting.black" },

    -- YAML
    { import = "lazyvim.plugins.extras.lang.yaml" },

    -- C/C++
    { import = "lazyvim.plugins.extras.lang.clangd" },

    -- Show inlay hints
    {
      "neovim/nvim-lspconfig",
      opts = {
        inlay_hints = {
          enabled = true,
        },
      },
    },

    -- Use fzf provided by Nix
    { "nvim-telescope/telescope-fzf-native.nvim", enabled = true },

    -- LSP is managed by Nix
    { "williamboman/mason-lspconfig.nvim", enabled = false },
    { "williamboman/mason.nvim", enabled = false },
    { "jaybaby/mason-nvim-dap.nvim", enabled = false },

    -- Load Treesitter prepared by Nix
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
