/*---------------------------------------------------------------------------
Copyright 2023, Tim Whiting, Fredrik Wieczerkowski, Daan Leijen.

This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
import * as path from "path"
import * as fs from "fs"
import * as vscode from "vscode"
import * as semver from "semver"
import { home, expandHome } from "./platform-paths"
import { VersionManager } from "./version-manager"


// Configuration
export class KokaConfig {
  constructor(private context: vscode.ExtensionContext, private vsConfig: vscode.WorkspaceConfiguration) {
    this.target = "c"
    this.refreshConfig()
    const extVersion = this.context.extension.packageJSON.version as string ?? "1.0.0"
    this.extensionVersion = semver.coerce(extVersion)?.format() ?? "1.0.0"
    this.versionManager = new VersionManager(this.context, this.vsConfig);
  }
  versionManager: VersionManager
  enableDebugExtension!: boolean
  autoFocusTerminal!: boolean        // focus on the terminal automatically on errors?
  target!: string                    // backend target (c,c32,c64c,wasm,jsnode)
  cwd!: string                       // current working directory for compiler / running the output
  includeDirs!: string[]             // compiler include directories
  extensionVersion!: string          // Version of the extension
  latestCompilerVersion!: string     // Latest known version of the compiler (used at build time of the extension)

  compilerArgs!: string[]            // extra arguments to pass

  // Configuration options for the language server
  // Inlay Hints Options
  showInferredTypes!: boolean
  showImplicitArguments!: boolean
  showFullQualifiers!: boolean

  getLanguageServerArgs(): string[] {
    return ["--language-server", "--buildtag=vscode","--target=" + this.target, ...this.includeDirs.map((d) => `-i${d}`), ...this.compilerArgs]
  }

  refreshConfig(): void {
    this.enableDebugExtension = this.vsConfig.get('dev.debugExtension') as boolean
    this.cwd = expandHome(this.vsConfig.get('languageServer.workingDirectory')!) as string
    if (!this.cwd) {
      if (vscode.workspace.workspaceFolders)
        this.cwd = vscode.workspace.workspaceFolders[0].uri.fsPath
      else if (vscode.workspace.workspaceFile)
        this.cwd = path.dirname(vscode.workspace.workspaceFile.fsPath)
      else
        this.cwd = home
    }
    if (vscode.workspace.workspaceFolders) {
      this.includeDirs = vscode.workspace.workspaceFolders.map((f) => f.uri.fsPath) // Support multiple folder workspaces
    } else {
      this.includeDirs = [this.cwd] // Default to the folder surrounding the file or HOME when no file opened
    }
    this.compilerArgs = this.vsConfig.get('languageServer.compilerArguments') as string[] || []
    this.autoFocusTerminal = this.vsConfig.get('languageServer.autoFocusTerminal') as boolean ?? false;
    this.showImplicitArguments = this.vsConfig.get('languageServer.inlayHints.showImplicitArguments') as boolean ?? false;
    this.showInferredTypes = this.vsConfig.get('languageServer.inlayHints.showInferredTypes') as boolean ?? false;
    this.showFullQualifiers = this.vsConfig.get('languageServer.inlayHints.showFullQualifiers') as boolean ?? false;
  }

  selectTarget(t: string) {
    if (!['c', 'c32', 'c64', 'c64c', 'jsnode', 'jsweb', 'wasm', 'wasm32', 'wasm64', 'wasmweb'].includes(t)) {
      return
    }
    this.target = t
  }

  /*-------------------------------------------------
    open samples directory
  -------------------------------------------------*/

  // open the samples directory in a separate window
  async openSamples() {
    const samplesDir = await this.getSamplesDir(this.versionManager.compilerPath, this.versionManager.compilerVersion)
    if (samplesDir) {
      vscode.commands.executeCommand('vscode.openFolder', vscode.Uri.file(samplesDir), { forceNewWindow: true })
    }
  }

  async getSamplesDir(compilerPath: string, compilerVersion: string): Promise<string> {
    const samplesDir = path.join(this.context.globalStorageUri.fsPath, `v${compilerVersion}`, "samples")
    if (fs.existsSync(samplesDir)) return samplesDir

    // create a samples directory by copying the compiler installed samples
    if (!compilerPath || !fs.existsSync(compilerPath)) {
      vscode.window.showErrorMessage('Can only open Koka samples once the compiler is installed')
      return ""
    }

    const examples = this.versionManager.getCompilerSamplesDir(compilerPath, compilerVersion)
    console.log("Koka: getSamplesDir: examples path: " + examples)
    if (!examples || !fs.existsSync(examples)) return ""

    fs.mkdirSync(samplesDir, { recursive: true })
    fs.cp(examples, samplesDir, { recursive: true }, async (err) => {
      if (err) {
        vscode.window.showErrorMessage(`Unable to copy Koka samples. (to ${samplesDir} from ${examples})`)
      }
      console.log(`Koka: getSamplesDir: copied examples to ${samplesDir} (from ${examples})`)
    })
    return samplesDir
  }
}