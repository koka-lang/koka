import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from 'vscode-languageclient';

import { scanForSDK } from './scan';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration('koka');
  // We can always create the client, as it does nothing as long as it is not started
  console.log(`Koka: language server enabled ${config.get('languageServer.enabled')}`)
  client = createClient(config);
  if (config.get('languageServer.enabled')) {
    context.subscriptions.push(client.start());
  }

  createCommands(context, config);
}

function createClient(config: vscode.WorkspaceConfiguration) {
  // TODO: Return all sdks and select default, but let user choose to switch between them
  const sdkPath = scanForSDK()
  const command = config.get('languageServer.command') as string || `${sdkPath} --language-server`
  console.log(`Koka: Language Server ${command} Workspace: ${vscode.workspace.workspaceFolders[0].uri.path}`)
  const serverOptions: ServerOptions = {
    command,
    options: {
      shell: true,
      cwd: config.get('languageServer.cwd') || vscode.workspace.workspaceFolders[0].uri.path,
    },
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'koka', scheme: 'file' }],
    outputChannelName: 'Koka',
    revealOutputChannelOn: RevealOutputChannelOn.Never,
  };
  const client = new LanguageClient(
    'Koka Language Client',
    serverOptions,
    clientOptions,
  );

  return client;
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

function createCommands(
  context: vscode.ExtensionContext,
  config: vscode.WorkspaceConfiguration,
) {
  context.subscriptions.push(
    vscode.commands.registerCommand('koka.restartLanguageServer', () => {
      if (!config.get('languageServer.enabled'))
        return vscode.window.showErrorMessage('Language server is not enabled');

      vscode.window.withProgress(
        {
          location: vscode.ProgressLocation.Notification,
          title: 'Koka',
          cancellable: false,
        },
        async (progress, token) => {
          progress.report({ message: 'Restarting language server' });
          // Right now this produces error in console
          // Bug is upstream: https://github.com/microsoft/vscode-languageserver-node/issues/878
          await client.stop();
          await client.start();
          progress.report({
            message: 'Language server restarted',
            increment: 100,
          });
          // Wait 3 second to allow user to read message
          await new Promise((resolve) => setTimeout(resolve, 3000));
        },
      );
    }),
  );
}
