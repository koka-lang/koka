/*---------------------------------------------------------------------------
Copyright 2023, Tim Whiting, Fredrik Wieczerkowski, Daan Leijen.

This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
import * as vscode from 'vscode'
import * as path from 'path'

import { KokaConfig, openSamples } from './workspace'
import { CancellationToken, DebugConfiguration, DebugConfigurationProvider, ProviderResult, WorkspaceFolder } from 'vscode'
import { KokaDebugSession } from './debugger'
import { KokaLanguageServer } from './lang-server'
import { MainCodeLensProvider } from './code-lens'
import { start } from 'repl'
import { FailureHandlingKind } from 'vscode-languageclient'

let languageServer : KokaLanguageServer = null;


export async function deactivate() { }

export async function activate(context: vscode.ExtensionContext) {
  const vsConfig = vscode.workspace.getConfiguration('koka') // All configuration parameters are prefixed with koka
  console.log(`Koka: language server enabled ${vsConfig.get('languageServer.enabled')}`)

  // Create commands that do not depend on the language server
  const kokaConfig = new KokaConfig(vsConfig)
  createBasicCommands(context, vsConfig, kokaConfig);
  console.log("Koka global storage: " + context.globalStorageUri.path);
  if (!vsConfig.get('languageServer.enabled')) {
    return
  }

  // start the language service
  await startLanguageServer(context,vsConfig,kokaConfig,true /* allow install */)
  if (!languageServer || !kokaConfig.isValid()) return;
  console.log( "Koka: language server started")

  const selectSDKMenuItem = null; // Do not show for now as it is too intrusive as it as always visible
  /*
  // create a new status bar item to select the Koka backend target// create a new status bar item to select the Koka compiler
  const selectSDKMenuItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100)
  selectSDKMenuItem.command = 'koka.selectSDK'
  context.subscriptions.push(selectSDKMenuItem)
  selectSDKMenuItem.show()
  selectSDKMenuItem.text = `Koka SDK`
  selectSDKMenuItem.tooltip = `${kokaConfig.sdkPath}`
  */

  const selectCompileTarget = null; // Do not show for now and only use C from vscode (as not all targets work without further tool installation)
  /*
  // create a new status bar item to select the Koka backend target
  const selectCompileTarget = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100)
  selectCompileTarget.command = 'koka.selectTarget'
  context.subscriptions.push(selectCompileTarget)
  selectCompileTarget.show()
  selectCompileTarget.text = `Koka Backend: ${kokaConfig.target}`
  */

  // Open samples?
  // This happens after the first time the Koka compiler is found (either already installed, or just installed)
  const open = await context.globalState.get('koka-opened-samples')
  console.log("Koka: opened samples before: " + open);
  if (open !== "yes") {
    await context.globalState.update('koka-opened-samples',"yes")
    await openSamples(context,kokaConfig);
  }

  // Register debug adapter
  registerDebugConfiguration(context, kokaConfig)

  // Initialize commands
  createCommands(context, vsConfig, kokaConfig, selectSDKMenuItem, selectCompileTarget )

  // Code lens (run debug | optimized)
  context.subscriptions.push(
    vscode.languages.registerCodeLensProvider({ language: "koka", scheme: "file" }, new MainCodeLensProvider(kokaConfig))
  )
}

async function restartLanguageServer( context : vscode.ExtensionContext, vsConfig: vscode.WorkspaceConfiguration, kokaConfig : KokaConfig ) : Promise<Boolean> {
  return startLanguageServer(context,vsConfig,kokaConfig,false /* don't allow install */)  // stops existing service if needed
}

async function startLanguageServer( context : vscode.ExtensionContext,
                                    vsConfig: vscode.WorkspaceConfiguration,
                                    kokaConfig : KokaConfig,
                                    allowInstall : Boolean) : Promise<Boolean>
{
  // stop existing server if already running
  if (languageServer) {
    await stopLanguageServer(context)
  }

  if (!kokaConfig.isValid()) {
    // update compiler paths and potentially install a fresh compiler
    await kokaConfig.updateCompilerPaths(context,vsConfig,allowInstall)
    if (!kokaConfig.isValid()) {
      console.log(`Koka: compiler is not functional: tried initializing from path(s): ${kokaConfig.compilerPaths.join(", ")}`)
      return false
    }
  }

  // and start the new one
  languageServer = new KokaLanguageServer(context)
  await languageServer.start(kokaConfig, context)
  return true
}


async function stopLanguageServer( context : vscode.ExtensionContext ) {
  if (!languageServer) return
  await languageServer.dispose()
  const languageServerIdx = context.subscriptions.indexOf(languageServer)
  if (languageServerIdx != -1) {
    context.subscriptions.splice(languageServerIdx, 1)
  }
  languageServer = null;
}


// These commands do not depend on the language server
function createBasicCommands(context: vscode.ExtensionContext, vsConfig: vscode.WorkspaceConfiguration, kokaConfig : KokaConfig) {
  context.subscriptions.push(
    // Install latest Koka
    vscode.commands.registerCommand('koka.installCompiler', async () => {
      await stopLanguageServer(context)
      await kokaConfig.installCompiler(context,vsConfig)
      await vscode.commands.executeCommand("koka.restartLanguageServer")  // shows progress
      await vscode.commands.executeCommand("koka.openSamples")
      // await startLanguageServer(context,vsConfig,kokaConfig,false)
    }),

    // Uninstall
    vscode.commands.registerCommand('koka.uninstallCompiler', async () => {
      await stopLanguageServer(context)
      await kokaConfig.uninstallCompiler(context,vsConfig)
      await vscode.commands.executeCommand("koka.restartLanguageServer")  // shows progress
      // await startLanguageServer(context,vsConfig,kokaConfig,false)
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
  vsConfig: vscode.WorkspaceConfiguration,
  kokaConfig: KokaConfig,
  selectSDKMenuItem: vscode.StatusBarItem,   // can be null
  selectCompileTarget: vscode.StatusBarItem, // can be null
) {
  // select SDK
  context.subscriptions.push(
    vscode.commands.registerCommand('koka.selectCompiler', async () => {
      kokaConfig.updateCompilerPaths(context,vsConfig,false);  // update with latest found paths
      const path = await vscode.window.showQuickPick(kokaConfig.compilerPaths)
      if (path) {
        kokaConfig.updateCompilerPath(path)
      }
      if (selectSDKMenuItem) {
        selectSDKMenuItem.tooltip = `${path}`
      }
      await vscode.commands.executeCommand('koka.restartLanguageServer')
      // await restartLanguageServer(context,vsConfig,kokaConfig)
    }),

    vscode.commands.registerCommand('koka.selectTarget', async () => {
      const result = await vscode.window.showQuickPick(['c', 'c32', 'c64c', 'jsnode', 'wasm'])
      if (result) {
        kokaConfig.selectTarget(result)
      }
      if (selectCompileTarget) {
        selectCompileTarget.text = `Koka Target: ${kokaConfig.target}`
      }
    }),

    // Open samples
    vscode.commands.registerCommand('koka.openSamples', () => {
      openSamples(context, kokaConfig)
    }),

    // Restart language server
    vscode.commands.registerCommand('koka.restartLanguageServer', () => {
      if (!vsConfig.get('languageServer.enabled'))
        return vscode.window.showErrorMessage('Language server is not enabled')
      vscode.window.withProgress(
        {
          location: vscode.ProgressLocation.Notification,
          title: 'Koka',
          cancellable: false,
        },
        async (progress, token) => {
          progress.report({ message: 'Restarting language server' })
          await restartLanguageServer(context,vsConfig,kokaConfig)
          progress.report({
            message: 'Language server restarted',
            increment: 100,
          })
          // Wait 2 seconds to allow user to read message
          await new Promise((resolve) => setTimeout(resolve, 2000))
        },
      )
    }),

    // Debug Adaptor
    vscode.commands.registerCommand('extension.language-koka.getProgramName', c => {
      return vscode.window.showInputBox({
        placeHolder: "Please enter the name of a koka file in the workspace folder",
        value: path.relative(vsConfig.cwd, vscode.window.activeTextEditor?.document.fileName || '') || 'test.kk'
      })
    }),

    // Start a program given just a path
    vscode.commands.registerCommand('koka.runMain', (resource: vscode.Uri, compilerArgs?: string, programArgs?: string[]) => {
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
    vscode.commands.registerCommand('koka.runFunction', (resource: vscode.Uri, functionName: string, compilerArgs?: string, programArgs?: string[]) => {
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

    // Show LSP output
    vscode.commands.registerCommand('koka.showLSPOutput', async () => {
      languageServer.showOutputChannel()
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