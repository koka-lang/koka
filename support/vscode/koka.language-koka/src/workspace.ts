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

interface SDKs { sdkPath: string, allSDKs: string[] }

// Constants
const latestVersion = "2.4.3"
const home          = os.homedir();
const kokaExeName   = (os.platform() === "win32" ? "koka.exe" : "koka")
const defaultShell  = (os.platform() === "win32" ? "C:\\Windows\\System32\\cmd.exe" : null)

// Development: set kokaDevDir to a non-empty string to (un)install from a local bundle instead of github
const kokaDevDir    = // ""
                      "c:/users/daan/dev/koka-ls"
                      // "/Users/daan/dev/koka-ls"
const kokaBundleBase= `${kokaDevDir}/bundle/v${latestVersion}/koka-v${latestVersion}`
const kokaBundle    = (os.platform() == "win32"
                       ? `${kokaBundleBase}-windows-x64.tar.gz`
                       : (os.platform() == "darwin"
                           ? `${kokaBundleBase}-macos-arm64.tar.gz`
                           : `${kokaBundleBase}-linux-x64.tar.gz`))


// Configuration
export class KokaConfig {
  constructor(config : vscode.WorkspaceConfiguration) {
    this.enableDebugExtension = config.get('debugExtension') as boolean
    this.compilerPaths = []
    this.compilerPath = ""
    this.languageServerArgs = []
    this.target = "C"
    this.cwd = config.get('languageServer.cwd') as string || vscode.workspace.workspaceFolders![0].uri.fsPath
    this.compilerArgs = config.get('languageServer.compilerArgs') as string[] || []
    this.autoFocusTerminal = config.get('languageServer.autoFocusTerminal') as boolean ?? false;
  }
  compilerPaths: string[]
  enableDebugExtension: boolean
  compilerPath: string
  languageServerArgs: string[]
  compilerArgs: string[]
  autoFocusTerminal: boolean
  target: string
  cwd: string

  isValid() : Boolean {
    return !!(this.compilerPaths && this.compilerPaths.length > 0 && this.compilerPath)
  }

  async updateCompilerPaths(context: vscode.ExtensionContext, vsConfig: vscode.WorkspaceConfiguration, allowInstall : Boolean) : Promise<Boolean> {
    let paths = []
    if (allowInstall) paths = await findInstallCompilerPaths(context,vsConfig)
                 else paths = findCompilerPaths(vsConfig)

    if (paths.length === 0) {
      console.log("Koka: cannot find a compiler")
      return false;
    }
    this.compilerPaths = paths;
    this.updateCompilerPath(this.compilerPaths[0])
    return this.isValid()
  }

  updateCompilerPath(path: string) {
    if (!path || !fs.existsSync(path)) {
      console.log(`Koka compiler not found at: ${path}`)
      this.compilerPath = ""
      return
    }
    // Test we can execute the sdk command
    fs.accessSync(path, fs.constants.X_OK)
    this.compilerPath = path
    this.languageServerArgs = ["--language-server", `-i${this.cwd}`, ...this.compilerArgs]
  }

  selectTarget(t: string) {
    if (!["c", "c32", "c64c", "jsnode", "wasm"].includes(t)) {
      return
    }
    this.target = t
  }

  async installCompiler(context: vscode.ExtensionContext, vsConfig: vscode.WorkspaceConfiguration) : Promise<Boolean> {
    const paths = findCompilerPaths(vsConfig)
    await installKoka(context,vsConfig,"",true /* force */)
    return this.updateCompilerPaths(context,vsConfig,false)
  }

  async uninstallCompiler(context: vscode.ExtensionContext, vsConfig: vscode.WorkspaceConfiguration) {
    await uninstallKoka(context)
    return this.updateCompilerPaths(context,vsConfig,false)
  }

}


export async function findInstallCompilerPaths(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration): Promise<string[]> {
  const paths = findCompilerPaths(config)
  if (paths.length === 0) {
    console.log('Koka: unable to find an installed Koka compiler')
    const reason = "The Koka compiler cannot be not found in the PATH"
    await installKoka(context, config, reason, false)
    return findCompilerPaths(config)
  }

  const defaultPath = paths[0]
  const version = getKokaVersion(defaultPath)
  if (semver.lt(version, latestVersion) ) {
    const reason = `The currently installed Koka compiler is version ${version} while the latest is ${latestVersion}`
    await installKoka(context, config, reason, false)
    return findCompilerPaths(config)
  }

  console.log("Koka: using Koka compiler at: " + defaultPath);
  return paths
}

function findCompilerPaths(config: vscode.WorkspaceConfiguration) : string[] {
  let compPaths = []

  // add user configured path?
  const compilerConfig = (config.get('languageServer.compiler') as string || "")
  if (compilerConfig) {
    if (!fs.existsSync(compilerConfig)) {
      vs.window.showInformationMessage(`Koka: configured compiler path does not exist: ${compilerConfig}`)
      //console.log('Koka: cannot find configured compiler: ' + exeConfig)
    }
    else {
      compPaths.push(compilerConfig);
    }
  }

  // check developer paths
  const devPaths = [path.join(home, 'koka')] // , path.join(home,'dev','koka')]
  for( const devp of devPaths) {
    if (fs.existsSync(devp)) {
      let cmdGetInstallRoot = 'stack path --local-install-root'

      // Linux ghcup installation does not show up in vscode's process.PATH,
      // ensure stack uses the correct ghc by sourcing the ghcup env script
      const ghc = `${home}/.ghcup/env`
      if (fs.existsSync(ghc)) {
        cmdGetInstallRoot = `${process.env.SHELL} -c "source ${ghc} && stack path --local-install-root"`
      }

      const result = child_process.execSync(cmdGetInstallRoot, { cwd: devp, env: process.env })
      const exePath = path.join( result.toString().trim(), 'bin', kokaExeName )
      if (fs.existsSync(exePath)) {
        vs.window.showInformationMessage(`Koka: found developer build at: ${devp}`)
        //console.log("Koka: Using dev build of koka at " + exePath)
        compPaths.push(exePath)
      }
      else {
        console.log("Koka: developer environment found, but no binary was built")
      }
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

  return compPaths.reverse()
}


function getKokaVersion(exePath: string) : string {
  const options = { env: process.env }
  const result = child_process.execSync(`${exePath} --version`, options)
  const versionRegex = /version: ([0-9]+\.[0-9]+.[0-9]+)/g;
  const match = versionRegex.exec(result.toString())
  console.log("Koka: found installed version " + match[1].toString())
  return match[1].toString();
}

async function installKoka(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration,
                                        reason: string, force : boolean) {
  // only prompt once for a download for each new extension version
  if (!force) {
    const response = await context.globalState.get('koka-latest-installed-version')
    if (typeof response === 'string' && response && semver.eq(response, latestVersion)){
      return
    }
  }
  const decision = await vscode.window.showInformationMessage(
    `${(reason ? reason + ".  \n" : "")}Would you like to download and install the latest Koka compiler?`,
    { }, // modal: true },
    'Yes',
    'No'
  )
  if (decision == 'No') {
    await context.globalState.update('koka-latest-installed-version', latestVersion)
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
      shellCmd = `${kokaDevDir}/util/install.bat ${flags} ${kokaBundle} && exit`
    }
    else {
      const tmpDir = (process.env.TMP || process.env.TEMP || "%HOMEDRIVE%%HOMEPATH%")
      shellCmd = `curl -sSL -o "${tmpDir}\\install-koka.bat" https://github.com/koka-lang/koka/releases/latest/download/install.bat && "${tmpDir}\\install-koka.bat" ${flags} && exit`
    }
  }
  else {
    if (kokaDevDir) {
      shellCmd = `${kokaDevDir}/util/install.sh ${flags} ${kokaBundle} && exit`
    }
    else {
      shellCmd = `curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh -s -- ${flags} && exit`
    }
  }
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
        await context.globalState.update('koka-opened-samples',"no")  // open samples later on

        const paths = findCompilerPaths(config); // rescan to find the just installed compiler
        const message = (paths.length>0 ? `Koka installed successfully at ${paths[0]}`
                            : `Koka install finished but unable to find the installed compiler`)
        console.log(message)
        resolve( paths )
        await vscode.window.showInformationMessage(message)
      }
    })
  })
  dispose?.dispose()
  return result;
}

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
      shellCmd = "curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh -s -- ${flags}"
    }
  }
  const term = vscode.window.createTerminal({ name: "Uninstall Koka", cwd: home, shellPath: defaultShell, isTransient: true, message: "Uninstalling Koka, you can close the terminal when done" })
  term.sendText(shellCmd)
  term.show()
}



function getSamplesDir(exePath: string): string {
  if (exePath.includes(".stack-work")) {
    const root = exePath.substring(0, exePath.indexOf(".stack-work"))  // <root>/.stack-work/.../bin/koka
    return path.join(root, "samples")
  }
  else {
    const root     = path.dirname(path.dirname(exePath))  // <root>/bin/koka
    const version  = getKokaVersion(exePath) || latestVersion;
    const examples = path.join(root, "share", "koka", `v${version}`, "lib", "samples")
    return examples;
  }
}

export async function openSamples(context: vscode.ExtensionContext, kokaConfig: KokaConfig) {
  const samplesDir: string = await context.globalState.get(`koka-samples-${latestVersion}`)
  console.log("Koka: openSamples: " + samplesDir)
  if (!samplesDir || !fs.existsSync(samplesDir)) {
    const exePath = kokaConfig.compilerPath
    if (exePath){
      const examples = getSamplesDir(exePath)
      console.log("Koka: openSamples: examples path: " + examples)
      if (fs.existsSync(examples)) {
        let dest = path.join(context.globalStorageUri.fsPath, "samples")
        console.log("Koka: openSamples: dest path: " + dest)
        if (!fs.existsSync(dest)) {
          fs.mkdirSync(dest,{recursive:true})
        }
        fs.cp(examples, dest, {recursive:true}, async (err) => {
          if (!err){
            console.log("Koka: openSamples: copied examples path: " + examples)
            await context.globalState.update(`koka-samples-${latestVersion}`, dest)
          } else {
            vscode.window.showErrorMessage(`Unable to copy Koka samples to ${dest}`)
            return;
          }
        })
      } else {
        vscode.window.showErrorMessage(`Unable to find the Koka samples at ${examples}`)
        return;
      }
    } else {
      vscode.window.showErrorMessage(`Unable to find an installed Koka compiler`)
      return;
    }
  }

  // and open the folder
  vscode.commands.executeCommand('vscode.openFolder', vscode.Uri.file(samplesDir), {forceNewWindow : true})
}
