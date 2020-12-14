import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn, ServerOptions } from 'vscode-languageclient/node';

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('koka');

    if (config.get('languageServer.enabled')) {
        const serverOptions: ServerOptions = {
            command: config.get('languageServer.command'),
            options: {
                shell: true,
                cwd: config.get('languageServer.cwd') || null
            }
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
