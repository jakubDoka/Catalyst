import * as path from 'path';
import { workspace, ExtensionContext, window } from 'vscode';

import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  if (workspace.workspaceFolders === undefined) {
    throw new Error("Extension works only on opened workspace.");
  }

  let cwd = workspace.workspaceFolders[0].uri.fsPath;

  let command: Executable = {
    command: "catalyst",
    args: ["lsp"],
    options: { cwd }
  };

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  let serverOptions: ServerOptions = {
    run: command,
    debug: command,
  };

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'Catalyst', pattern: "**.ctl" }],
    outputChannel: window.createOutputChannel('Catalyst'),
    revealOutputChannelOn: RevealOutputChannelOn.Error,
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: [
        workspace.createFileSystemWatcher('**/*.ctl'),
        workspace.createFileSystemWatcher('**/*.ctlm'),
      ]
    }
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    'catalystLsp',
    'Catalyst LSP',
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}