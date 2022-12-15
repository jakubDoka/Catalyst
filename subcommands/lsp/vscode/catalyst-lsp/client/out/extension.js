"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    if (vscode_1.workspace.workspaceFolders === undefined) {
        throw new Error("Extension works only on opened workspace.");
    }
    let cwd = vscode_1.workspace.workspaceFolders[0].uri.fsPath;
    let command = {
        command: "catalyst",
        args: ["lsp"],
        options: { cwd }
    };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    let serverOptions = {
        run: command,
        debug: command,
    };
    // Options to control the language client
    let clientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'Catalyst', pattern: "**.ctl" }],
        outputChannel: vscode_1.window.createOutputChannel('Catalyst'),
        revealOutputChannelOn: node_1.RevealOutputChannelOn.Error,
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: [
                vscode_1.workspace.createFileSystemWatcher('**/*.ctl'),
                vscode_1.workspace.createFileSystemWatcher('**/*.ctlm'),
            ]
        }
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient('catalystLsp', 'Catalyst LSP', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map