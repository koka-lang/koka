import * as path from "path"
import * as fs from "fs"
import * as vs from "vscode"
import * as os from "os"
import * as vscode from "vscode"
import * as child_process from "child_process"
import * as semver from "semver"

interface SDKs { sdkPath: string, allSDKs: string[] }

const latestVersion = "2.4.3"
const home          = os.homedir();
const kokaExeName   = (os.platform() === "win32" ? "koka.exe" : "koka")
const defaultShell  = (os.platform() === "win32" ? "C:\\Windows\\System32\\cmd.exe" : null)

export async function scanForSDK(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration): Promise<SDKs | undefined> {
  const sdk = findSDK(config);
  if (sdk.sdkPath === "") {
    console.log('Koka: unable to find an installed Koka compiler')
    const message = "The Koka compiler in not found in the PATH or in ~/.local/bin"
    return await downloadKoka(context, config, message, false, undefined)
  }
  else {
    const version = getKokaVersion(sdk.sdkPath)
    if (semver.lt(version, latestVersion) ) {
      const message = "The currently installed Koka compiler is version ${version} while the latest is ${latestVersion}"
      return await downloadKoka(context, config, message, true, sdk)
    }
  }
  console.log("Koka: using Koka at " + sdk.sdkPath);
  return sdk
}

function findSDK(config: vscode.WorkspaceConfiguration) : SDKs {
  const exePaths = findExePaths(config)
  const defaultPath = (exePaths[0] || "")
  return { sdkPath: defaultPath, allSDKs: exePaths }
}

function findExePaths(config: vscode.WorkspaceConfiguration) : string[] {
  let exePaths = []

  // add user configured path?
  const exeConfig = (config.get('languageServer.compiler') as string || "")
  if (exeConfig) {
    if (!fs.existsSync(exeConfig)) {
      console.log('Koka: cannot find configured compiler: ' + exeConfig);
    }
    else {
      exePaths.push(exeConfig);
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
        vs.window.showInformationMessage("Koka developer build found at " + devp)
        console.log("Koka: Using dev build of koka at " + exePath)
        exePaths.push(exePath)
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
      console.log("Koka: koka executable at " + p)
      const exePath = path.join(p, kokaExeName)
      exePaths.push(exePath)
    }
  }

  return exePaths.reverse()
}


function getKokaVersion(kokaPath: string): string {
  const options = { env: process.env }
  const result = child_process.execSync(`${kokaPath} --version`, options)
  const versionRegex = /version: ([0-9]+\.[0-9]+.[0-9]+)/g;
  const match = versionRegex.exec(result.toString())
  console.log("Koka: found installed version " + match[1].toString())
  return match[1].toString();
}

export async function downloadKoka(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration,
                                   reason: string, upgrade: boolean, old: (SDKs|undefined)): Promise<SDKs | undefined> {
  // only prompt once for a download for each new extension version
  const response = context.globalState.get('koka-download') as string
  if (response && semver.eq(response, latestVersion)){
    return old;
  }
  const decision = await vscode.window.showInformationMessage(
    `${(reason ? reason + ".\\n" : "")}Would you like to download and install the latest Koka compiler?`,
    { modal: true },
    'Yes',
    'No'
  )
  if (decision == 'No') {
    await context.globalState.update('koka-download', latestVersion)
    return old;
  }

  // download and install in a terminal
  let command = "curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh && exit"
  if (os.platform() === "win32") {
    const tmpDir = (process.env.TMP || process.env.TEMP || "%HOMEDRIVE%%HOMEPATH%")
    command = `curl -sSL -o ${tmpDir}\\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && ${tmpDir}\\install-koka.bat && exit`
  }
  const term = vscode.window.createTerminal({ name: "Install Koka", cwd: home, shellPath: defaultShell, isTransient: true, message: "Installing Koka" })
  term.sendText(command)
  term.show()
  let dispose: vscode.Disposable | undefined = undefined
  const result = await new Promise<SDKs|undefined>((resolve, reject) => {
    dispose = vscode.window.onDidCloseTerminal(async (t) => {
      // installation is done
      // todo: should we get the installation path directly from the install script instead of rescanning?
      if (t === term) {
        console.log("Koka: installation finished")
        // const sdk = await scanForSDK(context, config); // scan again for the just installed SDK
        const sdk = findSDK(config);
        if (sdk.sdkPath){
          resolve(sdk);
          openSamples(context, config);
        } else {
          resolve(undefined)  // todo: can we avoid undefined?
        }
      }
    })
  })
  dispose?.dispose()
  return result;
}

function getExamplesFolder(sdkPath: string): string {
  if (sdkPath.includes(".stack-work")){
    const sdkRoot = sdkPath.substring(0, sdkPath.indexOf(".stack-work"))
    return path.join(sdkRoot, "samples")
  }
  else {
    const sdkRoot = path.dirname(path.dirname(sdkPath))
    const examples = path.join(sdkRoot, "share", "koka", `v${latestVersion}`, "lib", "samples")
    return examples;
  }
}

export async function openSamples(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration) {
  const samplesFolder: string = await context.globalState.get(`koka-samples-${latestVersion}`)
  if (!samplesFolder) {
    const sdk = await scanForSDK(context, config);
    const {sdkPath} = sdk
    if (sdkPath){
      console.log(sdkPath)
      const examples = getExamplesFolder(sdkPath)
      if (fs.existsSync(examples)) {
        let dest = path.join(context.globalStorageUri.fsPath, "samples")
        fs.cp(examples, dest, {recursive:true}, async (err) => {
          if (!err){
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
  const examplesUri = vscode.Uri.file(samplesFolder)
  vscode.commands.executeCommand('vscode.openFolder', examplesUri, {forceNewWindow : true})
}

export async function uninstallKoka(context: vscode.ExtensionContext) {
  const decision = await vscode.window.showInformationMessage(
    `Uninstall the system Koka installation, continue?`,
    { modal: true },
    'Yes',
    'No'
  )
  if (decision == 'No') {
    return
  }
  let command = "curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh -s -- -u -f"
  if (os.platform() === "win32") {
    const tmpDir = (process.env.TMP || process.env.TEMP || "%HOMEDRIVE%%HOMEPATH%")
    command = `curl -sSL -o ${tmpDir}\\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && ${tmpDir}\\install-koka.bat -u -f`
  }
  const term = vscode.window.createTerminal({ name: "Uninstall Koka", cwd: home, shellPath: defaultShell, isTransient: true, message: "Uninstalling Koka, you can close the terminal when done" })
  term.sendText(command)
  term.show()
}


export class KokaConfig {
  constructor(config: vscode.WorkspaceConfiguration, sdkPath: string, allSDKs: string[]) {
    this.config = config
    this.debugExtension = config.get('debugExtension') as boolean
    this.defaultSDK = sdkPath
    this.sdkPath = sdkPath
    this.allSDKs = allSDKs
    this.cwd = config.get('languageServer.cwd') as string || vscode.workspace.workspaceFolders![0].uri.fsPath
    this.langServerArgs = []
    this.additionalArgs = config.get('languageServer.compilerArgs') as string[] || []
    this.selectSDK(this.sdkPath)
    this.target = "C"
    this.autoFocusTerminal = config.get('languageServer.autoFocusTerminal') as boolean ?? false;
  }
  defaultSDK: string
  sdkPath: string
  allSDKs: string[]
  config: vscode.WorkspaceConfiguration
  debugExtension: boolean
  command?: string | null
  langServerArgs: string[]
  additionalArgs: string[]
  autoFocusTerminal: boolean
  target: string
  cwd: string

  selectSDK(path: string) {
    if (!fs.existsSync(path)) {
      console.log(`Koka compiler not found at this location ${path}`)
      this.command = null
      return
    }
    // Test we can execute the sdk command
    fs.accessSync(path, fs.constants.X_OK)
    this.command = this.sdkPath
    this.langServerArgs = ["--language-server", `-i${this.cwd}`, ...this.additionalArgs]
  }

  selectTarget(t: string) {
    if (!["C", "JS", "WASM", "C#"].includes(t)) {
      return
    }
    this.target = t
  }
}