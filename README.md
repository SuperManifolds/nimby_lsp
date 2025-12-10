# NimbyScript LSP

A Language Server Protocol (LSP) implementation for [NimbyScript](https://wiki.nimbyrails.com/NimbyScript), the modding language for NIMBY Rails.

## Features

- **Syntax Highlighting** - Full TextMate grammar for VSCode, semantic tokens for all editors
- **Diagnostics** - Parse error detection and reporting
- **Completions** - Keywords, types, standard library functions, and game API
- **Hover Information** - Documentation for symbols
- **Document Symbols** - Outline view of structs, enums, functions

## Project Structure

```
nimby_lsp/
├── crates/
│   ├── nimbyscript-parser/     # PEG-based parser using pest
│   ├── nimbyscript-analyzer/   # Semantic analysis and API definitions
│   └── nimbyscript-lsp/        # LSP server implementation
├── api-definitions/            # Game API definitions (TOML)
├── editors/
│   ├── vscode/                 # VSCode extension
│   └── neovim/                 # Neovim plugin
└── tests/fixtures/             # Test NimbyScript files
```

## Installation

### Building from Source

```bash
# Clone the repository
git clone https://github.com/supermanifolds/nimby_lsp
cd nimby_lsp

# Build the LSP server
cargo build --release

# The binary will be at target/release/nimbyscript-lsp
```

### VSCode Extension

1. Navigate to `editors/vscode`
2. Run `npm install`
3. Run `npm run compile`
4. Package with `npm run package`
5. Install the generated `.vsix` file

Or for development:
1. Open `editors/vscode` in VSCode
2. Press F5 to launch Extension Development Host

### Neovim

See [editors/neovim/README.md](editors/neovim/README.md) for detailed instructions.

Quick setup with lazy.nvim:
```lua
{
    'supermanifolds/nimby_lsp',
    config = function()
        require('nimbyscript').setup()
    end,
    ft = { 'nimbyscript' },
}
```

## Grammar

The parser uses a PEG grammar based on the [official NimbyScript documentation](https://wiki.nimbyrails.com/NimbyScript). Grammar files are located in `crates/nimbyscript-parser/grammar/`:

- `common.pest` - Whitespace, comments, identifiers
- `literals.pest` - Numbers, strings, booleans
- `types.pest` - Type expressions
- `metadata.pest` - Meta blocks
- `expressions.pest` - Expression grammar
- `statements.pest` - Statement grammar
- `declarations.pest` - Top-level declarations
- `main.pest` - Entry point

## API Definitions

Game types and functions are defined in `api-definitions/nimbyrails.v1.toml`. This file can be customized to add new types as the game updates.

## Development

```bash
# Run tests
cargo test

# Run the LSP server directly
cargo run --bin nimbyscript-lsp

# Check parser with a test file
cargo run --example parse tests/fixtures/valid/example.nimbyscript
```

## License

MIT OR Apache-2.0
