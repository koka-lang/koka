import * as vscode from 'vscode'
import * as path from 'path'
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from 'vscode-languageclient'


import { KokaConfig, scanForSDK} from './workspace'
import { CancellationToken, CodeLens, DebugConfiguration, DebugConfigurationProvider, EventEmitter, ProviderResult, TextDocument, WorkspaceFolder } from 'vscode'
import { KokaDebugSession } from './debugger'

let client: LanguageClient

export function activate(context: vscode.ExtensionContext) {
  const vsConfig = vscode.workspace.getConfiguration('koka')
  // We can always create the client, as it does nothing as long as it is not started
  console.log(`Koka: language server enabled ${vsConfig.get('languageServer.enabled')}`)
  const {sdkPath, allSDKs} = scanForSDK()
  const config = new KokaConfig(vsConfig, sdkPath, allSDKs)
  client = createClient(config)
  if (vsConfig.get('languageServer.enabled')) {
    context.subscriptions.push(client.start())
  }

  createCommands(context, vsConfig, config)

  // Debug Adaptor stuff
	context.subscriptions.push(vscode.commands.registerCommand('extension.language-koka.getProgramName', c => {
		return vscode.window.showInputBox({
			placeHolder: "Please enter the name of a koka file in the workspace folder",
			value: path.relative(config.cwd, vscode.window.activeTextEditor?.document.fileName) || 'test.kk'
		})
	}))

	// register a configuration provider for 'koka' debug type
	const provider = new KokaRunConfigurationProvider()
	context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('koka', provider))

	// debug adapters can be run in different ways by using a vscode.DebugAdapterDescriptorFactory:
  // run the debug adapter as a separate process
  let factory = new InlineDebugAdapterFactory(config)

	context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('koka', factory))

  
  const codeLensProvider = new MainCodeLensProvider(config)
  context.subscriptions.push(vscode.languages.registerCodeLensProvider({language: "koka", scheme: "file" }, codeLensProvider))

}

function createClient(config: KokaConfig) {
  console.log(`Koka: Language Server ${config.langServerCommand} Workspace: ${config.cwd}`)
  const serverOptions: ServerOptions = {
    command: config.langServerCommand,
    options: {
      shell: true,
      cwd: config.cwd,
    },
  }
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: 'koka', scheme: 'file' }],
    outputChannelName: 'Koka',
    revealOutputChannelOn: RevealOutputChannelOn.Never,
  }
  const client = new LanguageClient(
    'Koka Language Client',
    serverOptions,
    clientOptions,
  )

  return client
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined
	}
	return client.stop()
}

function createCommands(
  context: vscode.ExtensionContext,
  config: vscode.WorkspaceConfiguration,
  kokaConfig: KokaConfig,
) {
  context.subscriptions.push(
    vscode.commands.registerCommand('koka.startWithoutDebugging', (resource: vscode.Uri) => {
      const launchConfig = 
				{
					name: `koka run: ${resource.path}`,
					request: "launch",
					type: "koka",
					program: resource.path,
				}
      console.log(`Launch config ${launchConfig}`)
			vscode.debug.startDebugging(vscode.workspace.getWorkspaceFolder(resource), launchConfig as vscode.DebugConfiguration)
		}),
    vscode.commands.registerCommand('koka.restartLanguageServer', () => {
      if (!config.get('languageServer.enabled'))
        return vscode.window.showErrorMessage('Language server is not enabled')

      vscode.window.withProgress(
        {
          location: vscode.ProgressLocation.Notification,
          title: 'Koka',
          cancellable: false,
        },
        async (progress, token) => {
          progress.report({ message: 'Restarting language server' })
          // Right now this produces error in console
          // Bug is upstream: https://github.com/microsoft/vscode-languageserver-node/issues/878
          await client.stop()
          await client.start()
          progress.report({
            message: 'Language server restarted',
            increment: 100,
          })
          // Wait 3 second to allow user to read message
          await new Promise((resolve) => setTimeout(resolve, 3000))
        },
      )
      vscode.window.createQuickPick
    }),
    vscode.commands.registerCommand('koka.selectSDK', async () => {
      const result = await vscode.window.showQuickPick(kokaConfig.allSDKs)
      kokaConfig.selectSDK(result)
      selectSDKMenuItem.tooltip = `${kokaConfig.sdkPath}`
      // TODO: Reinitialize langauge server
    }),
    vscode.commands.registerCommand('koka.selectTarget', async () => {
      const result = await vscode.window.showQuickPick(['C', 'WASM', 'JS', 'C#'])
      kokaConfig.selectTarget(result)
      selectCompileTarget.text = `Koka Backend: ${kokaConfig.target}`
    })
  )

	// create a new status bar item that we can now manage
	const selectSDKMenuItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100)
	selectSDKMenuItem.command = 'koka.selectSDK'
	context.subscriptions.push(selectSDKMenuItem)
  selectSDKMenuItem.show()
  selectSDKMenuItem.text = `Koka SDK`
  selectSDKMenuItem.tooltip = `${kokaConfig.sdkPath}`

	// create a new status bar item that we can now manage
	const selectCompileTarget = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100)
	selectCompileTarget.command = 'koka.selectTarget'
	context.subscriptions.push(selectCompileTarget)
  selectCompileTarget.show()
  selectCompileTarget.text = `Koka Backend: ${kokaConfig.target}`

}


class KokaRunConfigurationProvider implements DebugConfigurationProvider {

	/**
	 * Massage a debug configuration just before a debug session is being launched,
	 * e.g. add all missing attributes to the debug configuration.
	 */
	resolveDebugConfiguration(folder: WorkspaceFolder | undefined, config: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {
		// if launch.json is missing or empty
		if (!config.type && !config.request && !config.name) {
			const editor = vscode.window.activeTextEditor
			if (editor && editor.document.languageId === 'koka') {
				config.type = 'koka'
				config.name = 'Launch'
				config.request = 'launch'
				config.program = '${file}'
				config.stopOnEntry = true
			}
		}

		if (!config.program) {
			return vscode.window.showInformationMessage("Cannot find a program to debug").then(_ => {
				return undefined	// abort launch
			})
		}

		return config
	}
}

class InlineDebugAdapterFactory implements vscode.DebugAdapterDescriptorFactory {

  constructor(private readonly config: KokaConfig){}

	createDebugAdapterDescriptor(_session: vscode.DebugSession): ProviderResult<vscode.DebugAdapterDescriptor> {
		return new vscode.DebugAdapterInlineImplementation(new KokaDebugSession(this.config))
	}
}


class MainCodeLensProvider implements vscode.CodeLensProvider {
	private onDidChangeCodeLensesEmitter: EventEmitter<void> = new EventEmitter<void>()

	constructor(private readonly config: KokaConfig) {}
  
  public async provideCodeLenses(document: TextDocument, token: CancellationToken): Promise<CodeLens[] | undefined> {
		const doc = document.getText()
    const main = doc.indexOf('main')
    if (main < 0){
      return []
    }
		return [this.createCodeLens(document, main)]
	}

	private createCodeLens(document: TextDocument, offset: number): CodeLens {
		return new CodeLens(
			toRange(document, offset, 'main'.length),
			{
				arguments: [document.uri],
				command: "koka.startWithoutDebugging",
				title: `Run ${path.relative(this.config.cwd, document.uri.path)}`,
			}
		)
	}

}

function toRange(document: TextDocument, offset: number, length: number): vscode.Range {
	return new vscode.Range(document.positionAt(offset), document.positionAt(offset + length))
}