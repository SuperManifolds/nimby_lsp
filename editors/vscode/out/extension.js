"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const path = __importStar(require("path"));
const fs = __importStar(require("fs"));
const vscode = __importStar(require("vscode"));
const node_1 = require("vscode-languageclient/node");
let client;
let outputChannel;
function activate(context) {
    // Create output channel for logging
    outputChannel = vscode.window.createOutputChannel('NimbyScript');
    outputChannel.appendLine('NimbyScript extension activating...');
    const config = vscode.workspace.getConfiguration('nimbyscript');
    // Determine server path
    let serverCommand = config.get('server.path');
    outputChannel.appendLine(`Config server.path: ${serverCommand || '(not set)'}`);
    if (!serverCommand) {
        // Try to find bundled binary
        const bundledPath = context.asAbsolutePath(path.join('server', process.platform === 'win32'
            ? 'nimbyscript-lsp.exe'
            : 'nimbyscript-lsp'));
        outputChannel.appendLine(`Bundled path: ${bundledPath}`);
        // Check if bundled exists, otherwise use PATH
        if (fs.existsSync(bundledPath)) {
            outputChannel.appendLine('Bundled binary found!');
            // Check if it's executable
            try {
                fs.accessSync(bundledPath, fs.constants.X_OK);
                outputChannel.appendLine('Bundled binary is executable');
            }
            catch {
                outputChannel.appendLine('WARNING: Bundled binary is NOT executable, trying to fix...');
                try {
                    fs.chmodSync(bundledPath, 0o755);
                    outputChannel.appendLine('Fixed executable permissions');
                }
                catch (e) {
                    outputChannel.appendLine(`Failed to fix permissions: ${e}`);
                }
            }
            serverCommand = bundledPath;
        }
        else {
            outputChannel.appendLine('Bundled binary NOT found, falling back to PATH');
            serverCommand = 'nimbyscript-lsp';
        }
    }
    outputChannel.appendLine(`Using server command: ${serverCommand}`);
    const serverOptions = {
        run: {
            command: serverCommand,
            transport: node_1.TransportKind.stdio
        },
        debug: {
            command: serverCommand,
            transport: node_1.TransportKind.stdio
        }
    };
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'nimbyscript' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.nimbyscript')
        },
        outputChannel: outputChannel,
        traceOutputChannel: outputChannel,
    };
    client = new node_1.LanguageClient('nimbyscript', 'NimbyScript Language Server', serverOptions, clientOptions);
    // Start the client with error handling
    outputChannel.appendLine('Starting language client...');
    client.start().then(() => {
        outputChannel.appendLine('Language client started successfully');
    }).catch((error) => {
        outputChannel.appendLine(`Failed to start language client: ${error}`);
        vscode.window.showErrorMessage(`NimbyScript LSP failed to start: ${error.message}`);
    });
    // Register status bar item
    const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    statusBarItem.text = '$(symbol-misc) NimbyScript';
    statusBarItem.tooltip = 'NimbyScript Language Server';
    statusBarItem.show();
    context.subscriptions.push(statusBarItem);
    // Update status on client state changes
    client.onDidChangeState((event) => {
        outputChannel.appendLine(`Client state changed: ${event.oldState} -> ${event.newState}`);
        switch (event.newState) {
            case 1: // Stopped
                outputChannel.appendLine('Client stopped');
                statusBarItem.text = '$(error) NimbyScript';
                statusBarItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground');
                break;
            case 2: // Starting
                outputChannel.appendLine('Client starting...');
                statusBarItem.text = '$(sync~spin) NimbyScript';
                statusBarItem.backgroundColor = undefined;
                break;
            case 3: // Running
                outputChannel.appendLine('Client running!');
                statusBarItem.text = '$(check) NimbyScript';
                statusBarItem.backgroundColor = undefined;
                break;
        }
    });
}
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
//# sourceMappingURL=extension.js.map