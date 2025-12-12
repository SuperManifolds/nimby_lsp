# NimbyScript LSP Build System
# Targets: macOS ARM64, macOS x86_64, Windows x86_64, Linux x86_64

CARGO = cargo
EXTENSION_DIR = editors/vscode
NEOVIM_DIR = editors/neovim
SERVER_DIR = $(EXTENSION_DIR)/server
RELEASE_DIR = release
TREESITTER_QUERIES = crates/tree-sitter-nimbyscript/queries

# Rust target triples
TARGET_MACOS_ARM64 = aarch64-apple-darwin
TARGET_MACOS_X64 = x86_64-apple-darwin
TARGET_WINDOWS_X64 = x86_64-pc-windows-gnu
TARGET_LINUX_X64 = x86_64-unknown-linux-gnu

# VSCode platform targets
VSCODE_DARWIN_ARM64 = darwin-arm64
VSCODE_DARWIN_X64 = darwin-x64
VSCODE_WIN32_X64 = win32-x64
VSCODE_LINUX_X64 = linux-x64

# Binary names
BINARY_NAME = nimbyscript-lsp
BINARY_NAME_WIN = $(BINARY_NAME).exe

VERSION := $(shell grep '^version' Cargo.toml | head -1 | sed 's/.*"\(.*\)".*/\1/')

.PHONY: all clean install-targets build-all package-all \
        build-macos-arm64 build-macos-x64 build-windows-x64 build-linux-x64 \
        package-macos-arm64 package-macos-x64 package-windows-x64 package-linux-x64 \
        extension-deps dev test check sync-queries

#===============================================================================
# Main targets
#===============================================================================

all: build-all package-all

clean:
	$(CARGO) clean
	rm -rf $(RELEASE_DIR)
	rm -f $(SERVER_DIR)/$(BINARY_NAME) $(SERVER_DIR)/$(BINARY_NAME_WIN)
	rm -f $(EXTENSION_DIR)/*.vsix

#===============================================================================
# Prerequisites
#===============================================================================

install-targets:
	rustup target add $(TARGET_MACOS_ARM64)
	rustup target add $(TARGET_MACOS_X64)
	rustup target add $(TARGET_WINDOWS_X64)
	rustup target add $(TARGET_LINUX_X64)

extension-deps:
	cd $(EXTENSION_DIR) && npm install

#===============================================================================
# Build targets
#===============================================================================

build-macos-arm64:
	$(CARGO) build --release --target $(TARGET_MACOS_ARM64)
	mkdir -p $(RELEASE_DIR)/$(VSCODE_DARWIN_ARM64)
	cp target/$(TARGET_MACOS_ARM64)/release/$(BINARY_NAME) \
	   $(RELEASE_DIR)/$(VSCODE_DARWIN_ARM64)/

build-macos-x64:
	$(CARGO) build --release --target $(TARGET_MACOS_X64)
	mkdir -p $(RELEASE_DIR)/$(VSCODE_DARWIN_X64)
	cp target/$(TARGET_MACOS_X64)/release/$(BINARY_NAME) \
	   $(RELEASE_DIR)/$(VSCODE_DARWIN_X64)/

# Cross-compilation requires additional toolchains
# Install with: brew install mingw-w64
# Or use GitHub Actions for native Windows builds
build-windows-x64:
	$(CARGO) build --release --target $(TARGET_WINDOWS_X64)
	mkdir -p $(RELEASE_DIR)/$(VSCODE_WIN32_X64)
	cp target/$(TARGET_WINDOWS_X64)/release/$(BINARY_NAME_WIN) \
	   $(RELEASE_DIR)/$(VSCODE_WIN32_X64)/

# Linux cross-compilation from macOS is challenging
# Consider using Docker or GitHub Actions for native Linux builds
build-linux-x64:
	$(CARGO) build --release --target $(TARGET_LINUX_X64)
	mkdir -p $(RELEASE_DIR)/$(VSCODE_LINUX_X64)
	cp target/$(TARGET_LINUX_X64)/release/$(BINARY_NAME) \
	   $(RELEASE_DIR)/$(VSCODE_LINUX_X64)/

# Build for macOS platforms only (can be done natively on macOS)
build-macos: build-macos-arm64 build-macos-x64

build-all: build-macos-arm64 build-macos-x64 build-windows-x64 build-linux-x64

#===============================================================================
# Package targets (create platform-specific VSIX files)
#===============================================================================

package-macos-arm64: extension-deps
	rm -f $(SERVER_DIR)/$(BINARY_NAME) $(SERVER_DIR)/$(BINARY_NAME_WIN)
	cp $(RELEASE_DIR)/$(VSCODE_DARWIN_ARM64)/$(BINARY_NAME) $(SERVER_DIR)/
	chmod +x $(SERVER_DIR)/$(BINARY_NAME)
	cd $(EXTENSION_DIR) && npx vsce package --target $(VSCODE_DARWIN_ARM64)
	mkdir -p $(RELEASE_DIR)
	mv $(EXTENSION_DIR)/*.vsix $(RELEASE_DIR)/ 2>/dev/null || true

package-macos-x64: extension-deps
	rm -f $(SERVER_DIR)/$(BINARY_NAME) $(SERVER_DIR)/$(BINARY_NAME_WIN)
	cp $(RELEASE_DIR)/$(VSCODE_DARWIN_X64)/$(BINARY_NAME) $(SERVER_DIR)/
	chmod +x $(SERVER_DIR)/$(BINARY_NAME)
	cd $(EXTENSION_DIR) && npx vsce package --target $(VSCODE_DARWIN_X64)
	mkdir -p $(RELEASE_DIR)
	mv $(EXTENSION_DIR)/*.vsix $(RELEASE_DIR)/ 2>/dev/null || true

package-windows-x64: extension-deps
	rm -f $(SERVER_DIR)/$(BINARY_NAME) $(SERVER_DIR)/$(BINARY_NAME_WIN)
	cp $(RELEASE_DIR)/$(VSCODE_WIN32_X64)/$(BINARY_NAME_WIN) $(SERVER_DIR)/
	cd $(EXTENSION_DIR) && npx vsce package --target $(VSCODE_WIN32_X64)
	mkdir -p $(RELEASE_DIR)
	mv $(EXTENSION_DIR)/*.vsix $(RELEASE_DIR)/ 2>/dev/null || true

package-linux-x64: extension-deps
	rm -f $(SERVER_DIR)/$(BINARY_NAME) $(SERVER_DIR)/$(BINARY_NAME_WIN)
	cp $(RELEASE_DIR)/$(VSCODE_LINUX_X64)/$(BINARY_NAME) $(SERVER_DIR)/
	chmod +x $(SERVER_DIR)/$(BINARY_NAME)
	cd $(EXTENSION_DIR) && npx vsce package --target $(VSCODE_LINUX_X64)
	mkdir -p $(RELEASE_DIR)
	mv $(EXTENSION_DIR)/*.vsix $(RELEASE_DIR)/ 2>/dev/null || true

package-all: package-macos-arm64 package-macos-x64 package-windows-x64 package-linux-x64

#===============================================================================
# Development helpers
#===============================================================================

# Build and package for current platform only
dev:
	$(CARGO) build --release
	rm -f $(SERVER_DIR)/$(BINARY_NAME) $(SERVER_DIR)/$(BINARY_NAME_WIN)
	cp target/release/$(BINARY_NAME) $(SERVER_DIR)/ 2>/dev/null || \
	cp target/release/$(BINARY_NAME_WIN) $(SERVER_DIR)/ 2>/dev/null
	chmod +x $(SERVER_DIR)/$(BINARY_NAME) 2>/dev/null || true
	cd $(EXTENSION_DIR) && npx vsce package

# Run tests
test:
	$(CARGO) test --all

# Check compilation for all crates
check:
	$(CARGO) check --all-targets

# Run clippy
clippy:
	$(CARGO) clippy --all-targets -- -D warnings

# Format code
fmt:
	$(CARGO) fmt --all

# Install extension locally (after running dev)
install: dev
	code --install-extension $(EXTENSION_DIR)/*.vsix --force

#===============================================================================
# Neovim support
#===============================================================================

# Sync tree-sitter queries to neovim plugin
sync-queries:
	mkdir -p $(NEOVIM_DIR)/queries/nimbyscript
	cp $(TREESITTER_QUERIES)/highlights.scm $(NEOVIM_DIR)/queries/nimbyscript/
