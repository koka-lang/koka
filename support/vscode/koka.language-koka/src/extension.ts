import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, RevealOutputChannelOn, ServerOptions, TransportKind } from 'vscode-languageclient';

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('koka');
    const path = config.get('executablePath') as string;
    
    const serverOptions: ServerOptions = {
        command: path,
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
