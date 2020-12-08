import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn, ServerOptions, TransportKind } from 'vscode-languageclient';

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('koka');

    if (config.get('languageServer.enabled')) {
        const executablePath = config.get('languageServer.path') as string;
        const serverOptions: ServerOptions = {
            command: executablePath,
            args: ["--language-server"]
        };
        const clientOptions: LanguageClientOptions = {
            documentSelector: [
                { language: 'koka', scheme: 'file' }
            ],
            outputChannelName: 'Koka',
            revealOutputChannelOn: RevealOutputChannelOn.Never
        };
        const client = new LanguageClient(
            'Koka Language Client',
            serverOptions,
            clientOptions
        );

        context.subscriptions.push(client.start());
    }
}
