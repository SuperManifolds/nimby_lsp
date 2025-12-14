import * as path from 'path';
import * as fs from 'fs';
import * as https from 'https';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;

const GITHUB_REPO = 'supermanifolds/nimby_lsp';

function getSettings() {
    const config = vscode.workspace.getConfiguration('nimbyscript');
    return {
        inlayHintsEnabled: config.get<boolean>('inlayHints.enabled', true),
        semanticTokensEnabled: config.get<boolean>('semanticTokens.enabled', true)
    };
}

function getPlatformTarget(): string {
    const platform = process.platform;
    const arch = process.arch;

    if (platform === 'darwin') {
        return arch === 'arm64' ? 'darwin-arm64' : 'darwin-x64';
    } else if (platform === 'win32') {
        return arch === 'arm64' ? 'win32-arm64' : 'win32-x64';
    } else {
        return arch === 'arm64' ? 'linux-arm64' : 'linux-x64';
    }
}

function getBinaryName(): string {
    return process.platform === 'win32' ? 'nimbyscript-lsp.exe' : 'nimbyscript-lsp';
}

function getDownloadUrl(version: string): string {
    const platform = getPlatformTarget();
    const ext = process.platform === 'win32' ? '.exe' : '';
    return `https://github.com/${GITHUB_REPO}/releases/download/v${version}/nimbyscript-lsp-${platform}${ext}`;
}

function getStoragePaths(context: vscode.ExtensionContext): { binaryPath: string; versionPath: string } {
    const storageDir = context.globalStorageUri.fsPath;
    return {
        binaryPath: path.join(storageDir, getBinaryName()),
        versionPath: path.join(storageDir, 'version.txt')
    };
}

function getStoredVersion(versionPath: string): string | undefined {
    try {
        if (fs.existsSync(versionPath)) {
            return fs.readFileSync(versionPath, 'utf-8').trim();
        }
    } catch {
        // Ignore errors
    }
    return undefined;
}

function storeVersion(versionPath: string, version: string): void {
    fs.writeFileSync(versionPath, version, 'utf-8');
}

async function downloadFile(
    url: string,
    destPath: string,
    progress: vscode.Progress<{ message?: string; increment?: number }>
): Promise<void> {
    return new Promise((resolve, reject) => {
        const makeRequest = (requestUrl: string, redirectCount: number) => {
            if (redirectCount > 5) {
                reject(new Error('Too many redirects'));
                return;
            }

            const urlObj = new URL(requestUrl);
            const options = {
                hostname: urlObj.hostname,
                path: urlObj.pathname + urlObj.search,
                headers: {
                    'User-Agent': 'VSCode-NimbyScript-Extension'
                }
            };

            https.get(options, (response) => {
                // Handle redirects
                if (response.statusCode === 301 || response.statusCode === 302 || response.statusCode === 307) {
                    const redirectUrl = response.headers.location;
                    if (redirectUrl) {
                        makeRequest(redirectUrl, redirectCount + 1);
                        return;
                    }
                }

                if (response.statusCode !== 200) {
                    reject(new Error(`Failed to download: HTTP ${response.statusCode}`));
                    return;
                }

                const totalSize = parseInt(response.headers['content-length'] || '0', 10);
                let downloadedSize = 0;

                // Ensure directory exists
                const dir = path.dirname(destPath);
                if (!fs.existsSync(dir)) {
                    fs.mkdirSync(dir, { recursive: true });
                }

                const fileStream = fs.createWriteStream(destPath);

                response.on('data', (chunk: Buffer) => {
                    downloadedSize += chunk.length;
                    if (totalSize > 0) {
                        const percent = Math.round((downloadedSize / totalSize) * 100);
                        progress.report({ message: `${percent}%` });
                    }
                });

                response.pipe(fileStream);

                fileStream.on('finish', () => {
                    fileStream.close();
                    resolve();
                });

                fileStream.on('error', (err) => {
                    fs.unlink(destPath, () => {}); // Clean up partial file
                    reject(err);
                });
            }).on('error', reject);
        };

        makeRequest(url, 0);
    });
}

async function ensureBinaryAvailable(context: vscode.ExtensionContext): Promise<string> {
    const config = vscode.workspace.getConfiguration('nimbyscript');
    const customPath = config.get<string>('server.path');

    // If user specified a custom path, use it
    if (customPath) {
        outputChannel.appendLine(`Using custom server path: ${customPath}`);
        return customPath;
    }

    // Check for bundled binary (used in local development with `make install`)
    const bundledPath = context.asAbsolutePath(
        path.join('server', getBinaryName())
    );
    if (fs.existsSync(bundledPath)) {
        outputChannel.appendLine(`Using bundled binary: ${bundledPath}`);
        return bundledPath;
    }

    // No bundled binary - download from GitHub releases
    const { binaryPath, versionPath } = getStoragePaths(context);
    const extensionVersion = context.extension.packageJSON.version as string;
    const storedVersion = getStoredVersion(versionPath);

    outputChannel.appendLine(`Extension version: ${extensionVersion}`);
    outputChannel.appendLine(`Stored binary version: ${storedVersion || '(none)'}`);
    outputChannel.appendLine(`Binary path: ${binaryPath}`);

    // Check if we need to download
    const binaryExists = fs.existsSync(binaryPath);
    const versionMatches = storedVersion === extensionVersion;

    if (binaryExists && versionMatches) {
        outputChannel.appendLine('Binary is up to date');
        return binaryPath;
    }

    // Need to download
    const downloadUrl = getDownloadUrl(extensionVersion);
    outputChannel.appendLine(`Downloading from: ${downloadUrl}`);

    await vscode.window.withProgress(
        {
            location: vscode.ProgressLocation.Notification,
            title: 'NimbyScript: Downloading language server...',
            cancellable: false
        },
        async (progress) => {
            try {
                await downloadFile(downloadUrl, binaryPath, progress);

                // Set executable permissions on Unix
                if (process.platform !== 'win32') {
                    fs.chmodSync(binaryPath, 0o755);
                    outputChannel.appendLine('Set executable permissions');
                }

                // Store version
                storeVersion(versionPath, extensionVersion);
                outputChannel.appendLine('Download complete');
            } catch (error) {
                const message = error instanceof Error ? error.message : String(error);
                outputChannel.appendLine(`Download failed: ${message}`);
                throw error;
            }
        }
    );

    return binaryPath;
}

export async function activate(context: vscode.ExtensionContext) {
    // Create output channel for logging
    outputChannel = vscode.window.createOutputChannel('NimbyScript');
    outputChannel.appendLine('NimbyScript extension activating...');

    // Register status bar item early
    const statusBarItem = vscode.window.createStatusBarItem(
        vscode.StatusBarAlignment.Right,
        100
    );
    statusBarItem.text = '$(sync~spin) NimbyScript';
    statusBarItem.tooltip = 'NimbyScript Language Server';
    statusBarItem.show();
    context.subscriptions.push(statusBarItem);

    let serverCommand: string;
    try {
        serverCommand = await ensureBinaryAvailable(context);
    } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        outputChannel.appendLine(`Failed to get server binary: ${message}`);
        statusBarItem.text = '$(error) NimbyScript';
        statusBarItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground');
        statusBarItem.tooltip = `Failed to download language server: ${message}`;

        const action = await vscode.window.showErrorMessage(
            `NimbyScript: Failed to download language server. ${message}`,
            'Retry',
            'Configure Path'
        );

        if (action === 'Retry') {
            // Retry by reloading the window
            vscode.commands.executeCommand('workbench.action.reloadWindow');
        } else if (action === 'Configure Path') {
            vscode.commands.executeCommand('workbench.action.openSettings', 'nimbyscript.server.path');
        }
        return;
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
