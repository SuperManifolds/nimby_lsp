# Contributing to NimbyScript LSP

Thank you for your interest in contributing! This document provides guidelines and instructions for contributing.

## Getting Started

### Prerequisites

- **Rust** (stable toolchain)
- **Node.js** 20+
- **VS Code** (for testing the extension)

### Development Setup

1. **Install Rust**

```bash
# Install Rust (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

2. **Fork and Clone**

```bash
# Fork the repository on GitHub, then clone your fork
git clone https://github.com/YOUR_USERNAME/nimby_lsp.git
cd nimby_lsp
```

3. **Build and Install**

```bash
# Build and install the VS Code extension locally
make install
```

## Development Workflow

### Running Tests

```bash
# Run all Rust tests
cargo test --all

# Lint TypeScript extension
cd editors/vscode && npm run lint
```

### Code Quality

Before submitting changes, ensure your code passes quality checks:

```bash
# Run all linters at once
make lint

# Or run individually:
cargo check --all-targets
cargo fmt --all --check
cargo clippy --all-targets -- -D warnings
cd editors/vscode && npm run lint
luacheck --config editors/neovim/.luacheckrc editors/neovim
```

### Code Style

This project follows the conventions outlined in `AGENTS.md`:

- Run `cargo clippy` after making Rust changes
- Run `npm run lint` after making TypeScript changes
- Avoid excessive nesting (prefer early returns, extract helper functions)
- Keep functions small and focused on a single responsibility
- Follow Rust naming conventions and idiomatic patterns
- Prefer declarative over imperative code (iterators, functional patterns)
- Do not use `_` prefixes or `#[allow(dead_code)]` to silence warnings - remove unused code

## LLM and AI Assistance

Using LLMs and AI coding assistants is allowed, but requires deliberate and responsible use. See [Oxide's RFD 576](https://rfd.shared.oxide.computer/rfd/0576) for the philosophy behind these guidelines.

**Core principles:**

- **You are accountable** - You are responsible for all code you submit, regardless of how it was written
- **Understand your code** - Don't submit code you can't explain; be prepared to discuss every line in review
- **Verify everything** - Always review, test, and understand LLM-generated code before committing
- **Respect reviewer time** - Don't dump unreviewed LLM output into PRs

**Good uses:**

- Debugging assistance and "rubber duck" conversations
- Understanding unfamiliar code patterns or APIs
- Generating boilerplate or test cases (with careful review)
- Research and documentation help

**Discouraged practices:**

- Submitting wholesale LLM-generated code without understanding it
- Using LLMs to bypass learning the codebase
- Relying on LLMs for architectural decisions without human validation

## Making Changes

### Branch Naming

Create a branch using the format: `githubusername/<issue-id>-description`

```bash
git checkout -b yourname/123-add-goto-definition
# or for bug fixes
git checkout -b yourname/456-fix-hover-crash
```

### Commit Messages

Write clear, professional commit messages following the [Conventional Commits](https://www.conventionalcommits.org/) specification:

- `feat:` - New feature
- `fix:` - Bug fix
- `refactor:` - Code refactoring
- `docs:` - Documentation changes
- `test:` - Adding or updating tests
- `chore:` - Maintenance tasks

Example:
```
feat: add goto definition for function calls

- Resolve function definitions from API definitions
- Support jumping to user-defined functions
- Add tests for definition lookup
```

### Pull Requests

1. **Ensure tests pass and code is clean**
   - Run `cargo test --all`
   - Run `cargo clippy --all-targets -- -D warnings`
   - Run `npm run lint` in `editors/vscode/`

2. **Push your changes**

```bash
git push origin yourname/123-your-branch-name
```

3. **Open a Pull Request**
   - Go to the original repository on GitHub
   - Click "New Pull Request"
   - Select your fork and branch
   - Fill out the PR with a clear description

4. **Respond to feedback**
   - Address any review comments
   - Push additional commits to your branch as needed

## Project Structure

```
nimby_lsp/
├── crates/
│   ├── tree-sitter-nimbyscript/  # Tree-sitter grammar
│   ├── nimbyscript-parser/       # Parser built on tree-sitter
│   ├── nimbyscript-analyzer/     # Semantic analysis and diagnostics
│   └── nimbyscript-lsp/          # Language server implementation
├── editors/
│   └── vscode/                   # VS Code extension (TypeScript)
├── api-definitions/              # NimbyScript API definitions (TOML)
└── tests/                        # Integration test fixtures
```

## API Definitions

Game types and functions are defined in `api-definitions/nimbyrails.v1.toml`. This file provides completions and hover documentation for the NIMBY Rails API. It can be updated as the game adds new types and functions.

## Reporting Issues

When reporting bugs:
- Include the NimbyScript code that triggers the issue
- Describe expected vs actual behavior
- Include VS Code and extension version
- Check the Output panel (NimbyScript Language Server) for errors

For feature requests:
- Describe the use case
- Provide examples of how it would work

## Questions?

If you have questions about contributing, feel free to:
- Open a discussion on GitHub
- Ask in an issue

Thank you for contributing!
