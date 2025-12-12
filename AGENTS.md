# Claude Code Preferences

## Git Commits

- Write clear, professional commit messages that focus on what changed and why
- Use conventional commit style when appropriate
- When asked to commit, create a clear commit message without asking for confirmation

## Communication Style

- Be concise and direct
- Avoid unnecessary preamble or postamble

## Code Changes

- Always run `cargo clippy --all-targets -- -D warnings` and `cargo fmt` after making Rust code changes
- Run `npm run lint` after making TypeScript changes in `editors/vscode/`
- When implementing a new utility function make sure it is not already implemented elsewhere in the code base

## Rust Code Quality

- Avoid excessive nesting in functions (prefer early returns, extract helper functions)
- Do not silence clippy warnings without expressed consent, address the problem instead
- Keep functions small and focused on a single responsibility
- Follow Rust naming conventions and idiomatic patterns
- Address clippy warnings and suggestions when they improve code quality
- Prefer declarative over imperative code when sensible (use iterators, functional patterns, etc.)
- Do not use `_` prefixes or `#[allow(dead_code)]` to silence unused code warnings - just remove code that is no longer used
- Do not create a new version of an existing function if it makes the old function redundant, just modify the existing function

## Project Structure

- `crates/tree-sitter-nimbyscript/` - Tree-sitter grammar for NimbyScript
- `crates/nimbyscript-parser/` - Parser built on tree-sitter
- `crates/nimbyscript-analyzer/` - Semantic analysis, type checking, diagnostics
- `crates/nimbyscript-lsp/` - Language server protocol implementation
- `editors/vscode/` - VS Code extension (TypeScript)
- `api-definitions/` - TOML files defining NimbyScript API types and functions

## LSP Development

- Test changes by running `make install` to build and install the extension locally
- The LSP server binary is bundled inside the VS Code extension
- API definitions in `api-definitions/nimbyrails.v1.toml` provide completions and hover info

## TypeScript (VS Code Extension)

- Keep the extension thin - it should only bootstrap the LSP server
- All language intelligence belongs in the Rust LSP server, not the extension
