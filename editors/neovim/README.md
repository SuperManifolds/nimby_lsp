# NimbyScript Neovim Support

Language server integration for NimbyScript in Neovim.

## Prerequisites

- Neovim 0.10+ (uses `vim.system` for async operations)
- `curl` available in PATH (for auto-downloading the LSP binary)

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
- Diagnostics (parse errors)
- Completions (keywords, types, functions)
- Hover information
- Document symbols
- Semantic tokens

## Tree-sitter Syntax Highlighting

For enhanced syntax highlighting, install the tree-sitter parser:

1. Ensure you have [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) installed
2. Install the parser:
   ```vim
   :TSInstall nimbyscript
   ```
3. Enable highlighting in your config:
   ```lua
   require('nvim-treesitter.configs').setup({
       highlight = { enable = true },
   })
   ```

The parser is automatically registered when you call `require('nimbyscript').setup()`.

## Default Keymaps

When attached to a NimbyScript buffer:

| Key | Action |
|-----|--------|
| `gd` | Go to definition |
| `K` | Hover documentation |
| `gr` | Find references |
| `<leader>rn` | Rename symbol |
| `<leader>ca` | Code actions |

## Troubleshooting

### LSP not starting

1. Check if `nimbyscript-lsp` is in your PATH:
   ```bash
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
