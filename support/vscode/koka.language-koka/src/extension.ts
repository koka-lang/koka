import * as vscode from 'vscode'
import * as path from 'path'

import { KokaConfig, downloadSDK, scanForSDK, uninstallSDK } from './workspace'
import { CancellationToken, DebugConfiguration, DebugConfigurationProvider, ProviderResult, WorkspaceFolder } from 'vscode'
import { KokaDebugSession } from './debugger'
import { KokaLanguageServer } from './lang-server'
import { MainCodeLensProvider } from './code-lens'

let languageServer: KokaLanguageServer;

export async function deactivate() { }

export async function activate(context: vscode.ExtensionContext) {
  const vsConfig = vscode.workspace.getConfiguration('koka') // All configuration parameters are prefixed with koka
  console.log(`Koka: language server enabled ${vsConfig.get('languageServer.enabled')}`)

  // Create commands that do not depend on the language server
  createBasicCommands(context, vsConfig);
  console.log(context.globalStorageUri);
  if (!vsConfig.get('languageServer.enabled')) {
    return
  }

  const sdk = await scanForSDK(context, vsConfig)
  if (!sdk){
    return;
  }
  const { sdkPath, allSDKs } = sdk
  const kokaConfig = new KokaConfig(vsConfig, sdkPath, allSDKs)
  if (!kokaConfig.command) {
    vscode.window.showInformationMessage(`Koka SDK not functional: tried initializing from path: ${kokaConfig.sdkPath}\n All SDKs: ${allSDKs}`)
    return // No use initializing the rest of the extension's features
  }

  languageServer = new KokaLanguageServer(context)
  await languageServer.start(kokaConfig, context)

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

  // Register debug adapter
  registerDebugConfiguration(context, kokaConfig)
  // Initialize commands
  createCommands(context, vsConfig, kokaConfig, selectSDKMenuItem, selectCompileTarget)
  // Code lens (run and test)
  context.subscriptions.push(
    vscode.languages.registerCodeLensProvider({ language: "koka", scheme: "file" }, new MainCodeLensProvider(kokaConfig))
  )
}

// These commands do not depend on the language server
function createBasicCommands(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration) {
  context.subscriptions.push(
    // SDK management
    vscode.commands.registerCommand('koka.downloadLatest', async () => {
      // Reset the download flag
      await context.globalState.update('koka-download', true)
      downloadSDK(context, config, true, undefined)
    }),
    vscode.commands.registerCommand('koka.uninstall', () => {
      uninstallSDK(context)
    })
  )
}

// Register's some things that are needed for debugging
function registerDebugConfiguration(context: vscode.ExtensionContext, kokaConfig: KokaConfig) {
  context.subscriptions.push(
    // register a configuration provider for 'koka' debug type
    vscode.debug.registerDebugConfigurationProvider('koka', new KokaRunConfigurationProvider()),
    // run tests / run main
    vscode.debug.registerDebugAdapterDescriptorFactory('koka', new InlineDebugAdapterFactory(kokaConfig))
  )
}

// Create all of the commands that can be used via the vscode api
function createCommands(
  context: vscode.ExtensionContext,
  config: vscode.WorkspaceConfiguration,
  kokaConfig: KokaConfig,
  selectSDKMenuItem: vscode.StatusBarItem,
  selectCompileTarget: vscode.StatusBarItem,
) {
  context.subscriptions.push(
    vscode.commands.registerCommand('koka.selectSDK', async () => {
      const sdk = await scanForSDK(context, config)
      if (!sdk) {
        return;
      }
      const { sdkPath, allSDKs } = sdk
      kokaConfig.allSDKs = allSDKs
      const result = await vscode.window.showQuickPick(kokaConfig.allSDKs)
      if (result) kokaConfig.selectSDK(result)
      selectSDKMenuItem.tooltip = `${kokaConfig.sdkPath}`
      await vscode.commands.executeCommand('koka.restartLanguageServer')
    }),
    // Language Server management
    vscode.commands.registerCommand('koka.showLSPOutput', async () => {
      languageServer.showOutputChannel()
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
          await languageServer.dispose()
          const languageServerIdx = context.subscriptions.indexOf(languageServer)
          if (languageServerIdx != -1) {
            context.subscriptions.splice(languageServerIdx, 1)
          }         
          const sdk = await scanForSDK(context, config)
          if (!sdk) {
            return;
          }
          const { sdkPath, allSDKs } = sdk
          const newConfig = new KokaConfig(config, sdkPath, allSDKs)
          languageServer = new KokaLanguageServer(context)
          await languageServer.start(newConfig, context)

          progress.report({
            message: 'Language server restarted',
            increment: 100,
          })
          // Wait 2 seconds to allow user to read message
          await new Promise((resolve) => setTimeout(resolve, 2000))
        },
      )
    }),
    // Configuration
    vscode.commands.registerCommand('koka.selectTarget', async () => {
      const result = await vscode.window.showQuickPick(['C', 'WASM', 'JS', 'C#'])
      if (result) kokaConfig.selectTarget(result)
      selectCompileTarget.text = `Koka Backend: ${kokaConfig.target}`
    }),
    // Debug Adaptor stuff
    vscode.commands.registerCommand('extension.language-koka.getProgramName', c => {
      return vscode.window.showInputBox({
        placeHolder: "Please enter the name of a koka file in the workspace folder",
        value: path.relative(config.cwd, vscode.window.activeTextEditor?.document.fileName || '') || 'test.kk'
      })
    }),
    // Start a program given just a path
    vscode.commands.registerCommand('koka.startWithoutDebugging', (resource: vscode.Uri, compilerArgs?: string, programArgs?: string[]) => {
      const launchConfig =
      {
        name: `koka run: ${resource.path}`,
        request: "launch",
        type: "koka",
        program: resource.fsPath,
        compilerArgs,
        programArgs
      }
      console.log(`Launch config ${launchConfig}`)
      vscode.debug.startDebugging(vscode.workspace.getWorkspaceFolder(resource), launchConfig as vscode.DebugConfiguration)
    }),
    // Start a program given a path and a function name
    vscode.commands.registerCommand('koka.interpretExpression', (resource: vscode.Uri, functionName: string, compilerArgs?: string, programArgs?: string[]) => {
      const launchConfig =
      {
        name: `koka run: ${resource.path}`,
        request: "launch",
        type: "koka",
        program: resource.fsPath,
        functionName,
        compilerArgs,
        programArgs
      }
      console.log(`Launch config ${launchConfig}`)
      vscode.debug.startDebugging(vscode.workspace.getWorkspaceFolder(resource), launchConfig as vscode.DebugConfiguration)
    }),
  )

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

  constructor(private readonly config: KokaConfig) { }

  createDebugAdapterDescriptor(_session: vscode.DebugSession): ProviderResult<vscode.DebugAdapterDescriptor> {
    if (languageServer.languageClient)
      return new vscode.DebugAdapterInlineImplementation(new KokaDebugSession(this.config, languageServer.languageClient))
  }
}