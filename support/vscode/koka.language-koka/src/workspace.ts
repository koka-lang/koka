/*---------------------------------------------------------------------------
Copyright 2023, Tim Whiting, Fredrik Wieczerkowski, Daan Leijen.

This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
import * as path from "path"
import * as fs from "fs"
import * as vs from "vscode"
import * as os from "os"
import * as vscode from "vscode"
import * as child_process from "child_process"
import * as semver from "semver"


// Constants
const home            = os.homedir();
const kokaExeName     = (os.platform() === "win32" ? "koka.exe" : "koka")
const defaultShell    = (os.platform() === "win32" ? "C:\\Windows\\System32\\cmd.exe" : null)
const binaryPlatforms = ["win32-x64","darwin-arm64","darwin-x64","linux-x64"]

// Development: set kokaDevDir to a non-empty string to (un)install from a local bundle instead of github
const kokaDevDir    = ""
                      //"c:/users/daan/dev/koka"
                      // "/Users/daan/dev/koka-dev"

// Configuration
export class KokaConfig {
  constructor(context : vscode.ExtensionContext, vsConfig : vscode.WorkspaceConfiguration) {
    this.compilerPaths = []
    this.compilerPath = ""
    this.compilerVersion = "1.0.0"
    this.languageServerArgs = []
    this.target = "c"
    this.refreshConfig(vsConfig)
    const extVersion = context.extension.packageJSON.version as string ?? "1.0.0"
    this.extensionVersion = semver.coerce(extVersion).format()
    const latestCompVersion = context.extension.packageJSON.compilerVersion as string ?? "1.0.0"
    this.latestCompilerVersion = semver.coerce(latestCompVersion).format()
  }

  enableDebugExtension: boolean
  languageServerArgs: string[]
  autoFocusTerminal: boolean        // focus on the terminal automatically on errors?
  target: string                    // backend target (c,c32,c64c,wasm,jsnode)
  cwd: string                       // working directory for running the compiler
  developmentPath : string          // root path to a development repository of the compiler
  extensionVersion: string          // Version of the extension
  latestCompilerVersion: string     // Latest known version of the compiler (used at build time of the extension)

  compilerPath: string              // path in use for the compiler
  compilerVersion : string          // version of that compiler
  compilerArgs: string[]            // extra arguments to pass
  compilerPaths: string[]           // all found paths to koka compilers in the system

  // Configuration options for the language server
  // Inlay Hints Options
  showInferredTypes: boolean
  showImplicitArguments: boolean
  showFullQualifiers: boolean

  refreshConfig(vsConfig: vscode.WorkspaceConfiguration): void {
    this.enableDebugExtension = vsConfig.get('dev.debugExtension') as boolean
    this.developmentPath = vsConfig.get('dev.developmentPath') as string ?? ""
    this.cwd = vsConfig.get('languageServer.workingDirectory') as string || vscode.workspace.workspaceFolders[0]?.uri?.fsPath || home // TODO: use better default?
    this.compilerArgs = vsConfig.get('languageServer.compilerArguments') as string[] || []
    this.autoFocusTerminal = vsConfig.get('languageServer.autoFocusTerminal') as boolean ?? false;
    this.showImplicitArguments = vsConfig.get('languageServer.inlayHints.showImplicitArguments') as boolean ?? false;
    this.showInferredTypes = vsConfig.get('languageServer.inlayHints.showInferredTypes') as boolean ?? false;
    this.showFullQualifiers = vsConfig.get('languageServer.inlayHints.showFullQualifiers') as boolean ?? false;
  }

  // Does the compiler path point to a valid compiler?
  hasValidCompiler() : Boolean {
    if (this.compilerPath && fs.existsSync(this.compilerPath))
      return true
    else
      return false
  }

  // Search for Koka compilers along user specified path, the PATH, and development paths.
  // and possibly prompt the use to install the koka compiler if it is not found or out of date
  async updateCompilerPaths(context: vscode.ExtensionContext, vsConfig: vscode.WorkspaceConfiguration, allowInstall : Boolean) : Promise<Boolean> {
    let paths = []
    if (allowInstall) paths = await findInstallCompilerPaths(context, vsConfig, this.latestCompilerVersion, this.developmentPath)
                 else paths = findCompilerPaths(vsConfig, this.developmentPath)

    if (paths.length === 0) {
      console.log("Koka: cannot find a compiler")
      return false;
    }
    this.compilerPaths = paths;
    return this.setCompilerPath(this.compilerPaths[0])
  }

  // Set an explicit path to use for the Koka compiler
  setCompilerPath(path: string) : Boolean {
    if (!path || !fs.existsSync(path)) {
      console.log(`Koka: compiler not found at: ${path}`)
      return false
    }
    // Test if we can execute
    const compilerVersion = getCompilerVersion(path)
    if (!compilerVersion) {
      console.log(`Koka: unable to get compiler version of: ${path}`)
      return false
    }
    this.compilerVersion = compilerVersion
    this.compilerPath = path
    this.languageServerArgs = ["--language-server", "--buildtag=vscode", `-i${this.cwd}`, ...this.compilerArgs]
    return true
  }

  selectTarget(t: string) {
    if (!["c", "c32", "c64c", "jsnode", "wasm"].includes(t)) {
      return
    }
    this.target = t
  }

  // install the latest Koka compiler
  async installCompiler(context: vscode.ExtensionContext, vsConfig: vscode.WorkspaceConfiguration) : Promise<Boolean> {
    await installKoka(context, vsConfig, "", this.latestCompilerVersion, this.developmentPath, true /* force */)
    // todo: use instead `this.updateCompilerPath(pathToTheJustInstalledCompiler)`
    return this.updateCompilerPaths(context,vsConfig,false)
  }

  // uninstall the latest Koka compiler
  async uninstallCompiler(context: vscode.ExtensionContext, vsConfig: vscode.WorkspaceConfiguration) {
    await uninstallKoka(context)
    return this.updateCompilerPaths(context,vsConfig,false)
  }

  // open the samples directory in a separate window
  async openSamples(context : vscode.ExtensionContext) {
    openSamplesDir(context,this.compilerPath,this.compilerVersion)
  }

}

/*-------------------------------------------------
  find compiler paths
-------------------------------------------------*/

async function findInstallCompilerPaths(context: vscode.ExtensionContext,
                                               vsConfig: vscode.WorkspaceConfiguration,
                                               latestCompilerVersion : string,
                                               developmentPath : string): Promise<string[]> {
  const paths = findCompilerPaths(vsConfig, developmentPath)
  if (paths.length === 0) {
    console.log('Koka: unable to find an installed Koka compiler')
    const reason = "The Koka compiler cannot be not found in the PATH"
    await installKoka(context, vsConfig, reason, latestCompilerVersion, developmentPath, false)
    return findCompilerPaths(vsConfig, developmentPath)
  }

  const defaultPath = paths[0]
  const compilerVersion = getCompilerVersion(defaultPath) ?? "1.0.0"
  if (semver.lt(compilerVersion, latestCompilerVersion) ) {
    const reason = `The currently installed Koka compiler is version ${compilerVersion} while the latest is ${latestCompilerVersion}`
    await installKoka(context, vsConfig, reason, latestCompilerVersion, developmentPath, false)
    return findCompilerPaths(vsConfig, developmentPath)
  }

  console.log("Koka: using Koka compiler at: " + defaultPath);
  return paths
}

function findCompilerPaths(vsConfig: vscode.WorkspaceConfiguration, developmentPath : string) : string[] {
  let compPaths = []

  // add user configured path?
  const userCompiler = (vsConfig.get('languageServer.compiler') as string || "")
  if (userCompiler) {
    if (!fs.existsSync(userCompiler)) {
      //vs.window.showInformationMessage(`Koka: configured compiler path does not exist: ${userCompiler}`)
      console.log('Koka: cannot find user configured compiler: ' + userCompiler)
    }
    else {
      compPaths.push(userCompiler);
    }
  }

  // check developer path
  if (developmentPath && fs.existsSync(developmentPath)) {
    let cmdGetInstallRoot = 'stack path --local-install-root'

    // Linux ghcup installation does not show up in vscode's process.PATH,
    // ensure stack uses the correct ghc by sourcing the ghcup env script
    if (!["win32"].includes(os.platform())) {
      const ghcEnv = `${home}/.ghcup/env`
      if (fs.existsSync(ghcEnv)) {
        cmdGetInstallRoot = `${process.env.SHELL} -c "source ${ghcEnv} && stack path --local-install-root"`
      }
    }

    const result = child_process.execSync(cmdGetInstallRoot, { cwd: developmentPath, env: process.env })
    const exePath = path.join( result.toString().trim(), 'bin', kokaExeName )
    if (fs.existsSync(exePath)) {
      // vs.window.showInformationMessage(`Koka: found developer build at: ${devp}`)
      console.log("Koka: found development build of koka at " + exePath)
      compPaths.push(exePath)
    }
    else {
      console.log("Koka: developer environment found, but no binary was built")
    }
  }

  // check PATH and local binary installation directories
  const paths  = ((process.env.PATH as string) || "").split(path.delimiter)
  if (process.env.XDG_BIN_DIR) paths.push(process.env.XDG_BIN_DIR)
  paths.push(path.join(home, '.local', 'bin'))

  for (const p of paths) {
    if (fs.existsSync(path.join(p, kokaExeName))) {
      console.log("Koka: found Koka executable at: " + p)
      compPaths.push(path.join(p, kokaExeName))
    }
  }

  return compPaths
}


function getCompilerVersion(compilerPath: string) : string {
  const options = { env: process.env }
  let version = ""
  try {
    const buf = child_process.execSync(`"${compilerPath}" --version`, options) // can throw
    if (buf) {
      const versionRegex = /version: ([0-9]+\.[0-9]+.[0-9]+)/g;
      const match = versionRegex.exec(buf.toString())
      version = match[1].toString()
      console.log("Koka: found installed version " + version)
    }
  }
  catch(err) { }
  return version
}


/*-------------------------------------------------
  install
-------------------------------------------------*/

async function installKoka(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration,
                           reason: string, latestCompilerVersion: string,
                           developmentPath : string,
                           force : boolean) {
  // only prompt once for a download for each new extension version
  if (!force) {
    const latestInstalled = await context.globalState.get('koka-latest-installed-compiler') as string ?? "1.0.0"
    console.log(`Koka: latest installed compiler: ${latestInstalled}, latest known compiler is ${latestCompilerVersion}`)
    if (semver.gte(latestInstalled, latestCompilerVersion)) {
      return
    }
  }

  // check platform
  let warning = ""
  const platform = `${os.platform()}-${os.arch()}`
  if (!binaryPlatforms.includes(platform)) {
    warning = `Unfortunately, it looks like your platform ${platform} does not have a binary installer -- see <https://github.com/koka-lang/koka> for build instructions.  `
  }

  // ask the user to install
  const decision = await vscode.window.showInformationMessage(
    `${(reason ? reason + ".  \n" : "")}${warning}Would you like to download and install the latest Koka compiler?`,
    { }, // modal: true },
    'Yes',
    'No'
  )
  if (decision == 'No') {
    // pretend it is installed and don't auto prompt again in the future (until the extension is updated)
    await context.globalState.update('koka-latest-installed-compiler', latestCompilerVersion)
    return
  }
  else if (decision != 'Yes') { // cancel
    return
  }

  // download and install in a terminal
  let shellCmd = ""
  const flags = "--vscode"  // TODO: add `--force` to force all default actions? (like installing clang on windows if needed)
  if (os.platform() === "win32") {
    if (kokaDevDir) {
      const kokaBundle = getKokaBundleDir(kokaDevDir,latestCompilerVersion)
      shellCmd = `${kokaDevDir}/util/install.bat ${flags} ${kokaBundle} && exit`
    }
    else {
      const tmpDir = (process.env.TMP || process.env.TEMP || "%HOMEDRIVE%%HOMEPATH%")
      shellCmd = `curl -sSL -o "${tmpDir}\\install-koka.bat" https://github.com/koka-lang/koka/releases/latest/download/install.bat && "${tmpDir}\\install-koka.bat" ${flags} && exit`
    }
  }
  else {
    if (kokaDevDir) {
      const kokaBundle = getKokaBundleDir(kokaDevDir,latestCompilerVersion)
      shellCmd = `${kokaDevDir}/util/install.sh ${flags} ${kokaBundle} && exit`
    }
    else {
      shellCmd = `curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh -s -- ${flags} && exit`
    }
  }
  console.log(`Koka: install: ${shellCmd}`)
  const term = vscode.window.createTerminal({ name: "Install Koka", cwd: home, shellPath: defaultShell, isTransient: true, message: "Installing Koka" })
  term.sendText(shellCmd)
  term.show()
  let dispose: vscode.Disposable | undefined = undefined
  const result = await new Promise<string[]>((resolve, reject) => {
    dispose = vscode.window.onDidCloseTerminal(async (t) => {
      // installation is done
      // todo: should we get the installation path directly from the install script instead of rescanning?
      console.log("Koka: terminal install is done")
      if (t === term) {
        const paths = findCompilerPaths(config, developmentPath); // rescan to find the just installed compiler
        let message = ""
        if (paths.length>0) {
          // TODO: we cannot be sure the first path entry is the newly installed compiler.
          message = "Koka installed successfully"
          await context.globalState.update('koka-latest-installed-compiler', latestCompilerVersion)
        }
        else {
          message = "Koka installation finished but unable to find the installed compiler"
        }
        console.log(message)
        resolve( paths )
        await vscode.window.showInformationMessage(message)
      }
    })
  })
  dispose?.dispose()
  return result;
}

function getKokaBundleDir( kokaDir : string, version : string ) : string {
  const kokaBundleBase= `${kokaDir}/bundle/v${version}/koka-v${version}`
  const kokaBundle    = (os.platform() == "win32"
                       ? `${kokaBundleBase}-windows-x64.tar.gz`
                       : (os.platform() == "darwin"
                           ? `${kokaBundleBase}-macos-arm64.tar.gz`
                           : `${kokaBundleBase}-linux-x64.tar.gz`))
  return kokaBundle
}


/*-------------------------------------------------
  Uninstall
-------------------------------------------------*/

async function uninstallKoka(context: vscode.ExtensionContext) {
  const decision = await vscode.window.showInformationMessage(
    `Uninstalling the system Koka compiler, continue?`,
    { modal: true },
    'Yes',
    'No'
  )
  if (decision != 'Yes') {  // can be Cancel
    return
  }

  let shellCmd = ""
  const flags = "--uninstall --vscode"  // don't add --force as the default is to _not_ uninstall
  if (os.platform() === "win32") {
    if (kokaDevDir) {
      shellCmd = `"${kokaDevDir}/util/install.bat" ${flags}`
    }
    else {
      const tmpDir = (process.env.TMP || process.env.TEMP || "%HOMEDRIVE%%HOMEPATH%")
      shellCmd = `curl -sSL -o "${tmpDir}\\install-koka.bat" https://github.com/koka-lang/koka/releases/latest/download/install.bat && "${tmpDir}\\install-koka.bat" ${flags}`
    }
  }
  else {
    if (kokaDevDir) {
      shellCmd = `${kokaDevDir}/util/install.sh ${flags}`
    }
    else {
      shellCmd = `curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh -s -- ${flags}`
    }
  }
  const term = vscode.window.createTerminal({ name: "Uninstall Koka", cwd: home, shellPath: defaultShell, isTransient: true, message: "Uninstalling Koka, you can close the terminal when done" })
  term.sendText(shellCmd)
  term.show()
}


/*-------------------------------------------------
  open samples directory
-------------------------------------------------*/

async function openSamplesDir(context: vscode.ExtensionContext, compilerPath : string, compilerVersion : string)
{
  const samplesDir = await getSamplesDir(context,compilerPath,compilerVersion)
  if (samplesDir) {
    vscode.commands.executeCommand('vscode.openFolder', vscode.Uri.file(samplesDir), {forceNewWindow : true})
  }
}

async function getSamplesDir(context: vscode.ExtensionContext, compilerPath : string, compilerVersion : string ) : Promise<string>
{
  const samplesDir = path.join(context.globalStorageUri.fsPath, `v${compilerVersion}`, "samples")
  if (fs.existsSync(samplesDir)) return samplesDir

  // create a samples directory by copying the compiler installed samples
  if (!compilerPath || !fs.existsSync(compilerPath)) {
    vscode.window.showErrorMessage('Can only open Koka samples once the compiler is installed')
    return ""
  }

  const examples = getCompilerSamplesDir(compilerPath, compilerVersion)
  console.log("Koka: getSamplesDir: examples path: " + examples)
  if (!examples || !fs.existsSync(examples)) return ""

  fs.mkdirSync(samplesDir,{recursive:true})
  fs.cp(examples, samplesDir, {recursive:true}, async (err) => {
    if (err) {
      vscode.window.showErrorMessage(`Unable to copy Koka samples. (to ${samplesDir} from ${examples})`)
      return "";
    }
    console.log(`Koka: getSamplesDir: copied examples to ${samplesDir} (from ${examples})`)
    return samplesDir
  })
}

function getCompilerSamplesDir(compilerPath: string, compilerVersion : string): string {
  if (compilerPath.includes(".stack-work")) {
    const root = compilerPath.substring(0, compilerPath.indexOf(".stack-work"))  // <root>/.stack-work/.../bin/koka
    return path.join(root, "samples")
  }
  else {
    const root     = path.dirname(path.dirname(compilerPath))  // <root>/bin/koka
    const ver      = getCompilerVersion(compilerPath) || compilerVersion;
    const examples = path.join(root, "share", "koka", `v${ver}`, "lib", "samples")
    return examples;
  }
}
