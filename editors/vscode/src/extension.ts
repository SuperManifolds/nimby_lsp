import * as path from 'path';
import * as fs from 'fs';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;

function getSettings() {
    const config = vscode.workspace.getConfiguration('nimbyscript');
    return {
        inlayHintsEnabled: config.get<boolean>('inlayHints.enabled', true),
        semanticTokensEnabled: config.get<boolean>('semanticTokens.enabled', true)
    };
}

export function activate(context: vscode.ExtensionContext) {
    // Create output channel for logging
    outputChannel = vscode.window.createOutputChannel('NimbyScript');
    outputChannel.appendLine('NimbyScript extension activating...');

    const config = vscode.workspace.getConfiguration('nimbyscript');

    // Determine server path
    let serverCommand = config.get<string>('server.path');
    outputChannel.appendLine(`Config server.path: ${serverCommand || '(not set)'}`);

    if (!serverCommand) {
        // Try to find bundled binary
        const bundledPath = context.asAbsolutePath(
            path.join('server', process.platform === 'win32'
                ? 'nimbyscript-lsp.exe'
                : 'nimbyscript-lsp')
        );
        outputChannel.appendLine(`Bundled path: ${bundledPath}`);

        // Check if bundled exists, otherwise use PATH
        if (fs.existsSync(bundledPath)) {
            outputChannel.appendLine('Bundled binary found!');

            // Check if it's executable
            try {
                fs.accessSync(bundledPath, fs.constants.X_OK);
                outputChannel.appendLine('Bundled binary is executable');
            } catch {
                outputChannel.appendLine('WARNING: Bundled binary is NOT executable, trying to fix...');
                try {
                    fs.chmodSync(bundledPath, 0o755);
                    outputChannel.appendLine('Fixed executable permissions');
                } catch (e) {
                    outputChannel.appendLine(`Failed to fix permissions: ${e}`);
                }
            }

            serverCommand = bundledPath;
        } else {
            outputChannel.appendLine('Bundled binary NOT found, falling back to PATH');
            serverCommand = 'nimbyscript-lsp';
        }
    }

    outputChannel.appendLine(`Using server command: ${serverCommand}`);

    const serverOptions: ServerOptions = {
        run: {
            command: serverCommand,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverCommand,
            transport: TransportKind.stdio
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'nimbyscript' }],
        initializationOptions: getSettings(),
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.nimbyscript'),
            configurationSection: 'nimbyscript'
        },
        outputChannel: outputChannel,
        traceOutputChannel: outputChannel,
    };

    client = new LanguageClient(
        'nimbyscript',
        'NimbyScript Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client with error handling
    outputChannel.appendLine('Starting language client...');
    client.start().then(() => {
        outputChannel.appendLine('Language client started successfully');
    }).catch((error) => {
        outputChannel.appendLine(`Failed to start language client: ${error}`);
        vscode.window.showErrorMessage(`NimbyScript LSP failed to start: ${error.message}`);
    });

    // Register status bar item
    const statusBarItem = vscode.window.createStatusBarItem(
        vscode.StatusBarAlignment.Right,
        100
    );
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

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
