#!/bin/bash
set -e

echo "Building LSP server..."
cargo build --release

echo "Copying binary to VSCode extension..."
/bin/rm -f editors/vscode/server/nimbyscript-lsp
/bin/cp target/release/nimbyscript-lsp editors/vscode/server/

echo "Packaging VSCode extension..."
cd editors/vscode
npx vsce package --allow-missing-repository

echo "Done! Install with:"
echo "  code --install-extension editors/vscode/nimbyscript-0.1.0.vsix --force"
