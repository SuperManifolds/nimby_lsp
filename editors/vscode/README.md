# NimbyScript for Visual Studio Code

Language support for [NimbyScript](https://wiki.nimbyrails.com/NimbyScript), the modding language for [NIMBY Rails](https://store.steampowered.com/app/1134710/NIMBY_Rails/).

## Features

### Syntax Highlighting

Full syntax highlighting for NimbyScript files with semantic token support for context-aware coloring.

### Intelligent Completions

Context-aware autocompletion for:
- Language keywords (`fn`, `struct`, `let`, `if`, `for`, etc.)
- Primitive types (`i32`, `f64`, `bool`, etc.)
- Game API types, methods, and fields
- Your defined structs, enums, and functions
- Callback function templates

### Real-time Diagnostics

Catch errors as you type:
- Syntax errors with error recovery
- Type mismatches
- Undefined variables and functions
- Invalid callback signatures
- Meta field validation

### Hover Information

Hover over symbols to see:
- Type information
- Documentation from API definitions
- Function and method signatures
- Field types and readonly status

### Signature Help

Get parameter hints while typing function and method calls.

### Document Outline

Navigate your code with the outline view showing all:
- Structs and their fields
- Enums and variants
- Functions and methods

### Inlay Hints

See parameter names and type annotations inline in your code.

### Type Hierarchy

Explore type relationships with the type hierarchy view.

## Requirements

No additional requirements. The extension bundles the language server for your platform.

## Extension Settings

| Setting | Description | Default |
|---------|-------------|---------|
| `nimbyscript.server.path` | Path to custom language server binary | (bundled) |
| `nimbyscript.api.definitionsPath` | Path to custom API definitions file | |
| `nimbyscript.trace.server` | Enable language server tracing for debugging | `off` |
| `nimbyscript.inlayHints.enabled` | Enable inlay hints for type annotations and parameter names | `true` |
| `nimbyscript.semanticTokens.enabled` | Enable semantic token highlighting | `true` |

## Supported Platforms

- macOS (Apple Silicon and Intel)
- Windows (x64 and ARM64)
- Linux (x64 and ARM64)

## Release Notes

See [GitHub Releases](https://github.com/supermanifolds/nimby_lsp/releases) for version history.

## Contributing

Found a bug or have a feature request? Please open an issue on [GitHub](https://github.com/supermanifolds/nimby_lsp/issues).

## License

MIT
