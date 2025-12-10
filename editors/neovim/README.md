# NimbyScript Neovim Support

Language server integration for NimbyScript in Neovim.

## Prerequisites

1. Install the `nimbyscript-lsp` binary and ensure it's in your PATH:
   ```bash
   cargo install --path crates/nimbyscript-lsp
   ```

2. Neovim 0.8+ (0.11+ recommended for native LSP config)

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
    -- Path to LSP binary (searches PATH if nil)
    cmd = nil,

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

## Features

- Syntax highlighting (basic, via filetype detection)
- Diagnostics (parse errors)
- Completions (keywords, types, functions)
- Hover information
- Document symbols
- Semantic tokens

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

The basic syntax highlighting is provided by the LSP semantic tokens.
For enhanced highlighting, consider using tree-sitter (grammar not yet available).
