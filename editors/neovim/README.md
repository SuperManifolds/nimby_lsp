# NimbyScript Neovim Support

Language server integration for NimbyScript in Neovim.

## Prerequisites

- Neovim 0.10+ (uses `vim.system` for async operations)
- `curl` available in PATH (for auto-downloading the LSP binary)
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) (optional, for syntax highlighting)

The LSP binary is automatically downloaded from GitHub releases on first use.

## Installation

### Using lazy.nvim

```lua
{
    'supermanifolds/nimby_lsp',
    config = function()
        require('nimbyscript').setup({
            -- Optional: specify path to LSP binary
            -- cmd = '/path/to/nimbyscript-lsp',
        })
    end,
    ft = { 'nimbyscript' },
}
```

### Using packer.nvim

```lua
use {
    'supermanifolds/nimby_lsp',
    config = function()
        require('nimbyscript').setup()
    end,
}
```

### Manual Installation

1. Clone the repository
2. Add the `editors/neovim` directory to your Neovim runtimepath
3. Add to your `init.lua`:

```lua
require('nimbyscript').setup()
```

## Configuration Options

```lua
require('nimbyscript').setup({
    -- Path to LSP binary (auto-downloads from GitHub if nil)
    cmd = nil,

    -- Automatically check for and install updates on startup
    auto_update = true,

    -- LSP server settings
    settings = {},

    -- File types to attach
    filetypes = { 'nimbyscript' },

    -- Root directory markers
    root_markers = { '.git', 'mod.txt' },

    -- Enable debug logging
    debug = false,
})
```

### Using a Custom Binary

To use a locally built or custom binary instead of auto-downloading:

```lua
require('nimbyscript').setup({
    cmd = '/path/to/nimbyscript-lsp',
})
```

### Disabling Auto-Update

To disable automatic update checks on startup:

```lua
require('nimbyscript').setup({
    auto_update = false,
})
```

## Features

- Syntax highlighting (tree-sitter based)
- Diagnostics (semantic errors and warnings)
- Completions (keywords, types, functions, methods, fields)
- Signature help (function parameter hints)
- Hover information (type info and documentation)
- Document symbols
- Semantic tokens (enhanced highlighting from LSP)
- Filetype icons (nvim-web-devicons and mini.icons)

## Tree-sitter Syntax Highlighting

The tree-sitter parser is automatically registered when you call `require('nimbyscript').setup()`.

To install and enable the parser:

```vim
:TSInstall nimbyscript
```

Make sure highlighting is enabled in your nvim-treesitter config:

```lua
require('nvim-treesitter.configs').setup({
    highlight = { enable = true },
})
```

## Troubleshooting

### LSP not starting

1. Check if the binary exists (auto-downloaded or custom path):
   ```bash
   # Auto-downloaded location:
   ls ~/.local/share/nvim/nimbyscript/
   # Or check if custom binary is in PATH:
   which nimbyscript-lsp
   ```

2. Check Neovim LSP logs:
   ```vim
   :LspLog
   ```

3. Enable debug mode:
   ```lua
   require('nimbyscript').setup({ debug = true })
   ```

### No syntax highlighting

1. Ensure nvim-treesitter is installed and configured
2. Install the parser: `:TSInstall nimbyscript`
3. Check parser status: `:TSInstallInfo`
