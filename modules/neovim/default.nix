{ pkgs, lib, ... }:

let
  packages = with pkgs; [
    git
    lazygit

    ripgrep

    lua-language-server
    stylua

    # Typescript
    nodePackages.typescript-language-server
    nodePackages.eslint
    nodePackages.prettier
    vscode-langservers-extracted

    # Python
    nodePackages.pyright
    ruff-lsp
    black

    # YAML
    yaml-language-server

    # C/C++
    clang-tools
  ];

  plugins = with pkgs.vimPlugins; [
    # LazyVim
    LazyVim
    bufferline-nvim
    cmp-buffer
    cmp-nvim-lsp
    cmp-path
    cmp_luasnip
    conform-nvim
    dashboard-nvim
    dressing-nvim
    flash-nvim
    friendly-snippets
    gitsigns-nvim
    indent-blankline-nvim
    lualine-nvim
    neo-tree-nvim
    neoconf-nvim
    neodev-nvim
    noice-nvim
    nui-nvim
    nvim-cmp
    nvim-lint
    nvim-lspconfig
    nvim-notify
    nvim-spectre
    nvim-treesitter
    nvim-treesitter-context
    nvim-treesitter-textobjects
    nvim-ts-autotag
    nvim-ts-context-commentstring
    nvim-web-devicons
    persistence-nvim
    plenary-nvim
    telescope-fzf-native-nvim
    telescope-nvim
    todo-comments-nvim
    tokyonight-nvim
    trouble-nvim
    vim-illuminate
    vim-startuptime
    which-key-nvim
    { name = "LuaSnip"; path = luasnip; }
    { name = "catppuccin"; path = catppuccin-nvim; }
    { name = "mini.ai"; path = mini-nvim; }
    { name = "mini.bufremove"; path = mini-nvim; }
    { name = "mini.comment"; path = mini-nvim; }
    { name = "mini.indentscope"; path = mini-nvim; }
    { name = "mini.pairs"; path = mini-nvim; }
    { name = "mini.surround"; path = mini-nvim; }

    # C/C++
    clangd_extensions-nvim
  ];

  mkEntryFromDrv = drv:
    if lib.isDerivation drv then
      { name = "${lib.getName drv}"; path = drv; }
    else
      drv;

  lazyPath = pkgs.linkFarm "lazy-plugins" (builtins.map mkEntryFromDrv plugins);

  treesitterPath = pkgs.symlinkJoin {
    name = "lazyvim-nix-treesitter-parsers";
    paths = pkgs.vimPlugins.nvim-treesitter.withAllGrammars.dependencies;
  };
in
{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    extraPackages = packages; 

    plugins = with pkgs.vimPlugins; [
      lazy-nvim
    ];

    extraConfig = ''
      let g:config_path = "${./config}"
      let g:lazy_path = "${lazyPath}"
      let g:treesitter_path = "${treesitterPath}"
      source ${./config/init.lua}
    '';
  };
}
