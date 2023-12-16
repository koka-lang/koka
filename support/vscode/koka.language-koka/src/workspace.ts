import * as path from "path"
import * as fs from "fs"
import * as vs from "vscode"
import * as os from "os"
import * as vscode from "vscode"
import * as child_process from "child_process"
import * as semver from "semver"

interface SDKs { sdkPath: string, allSDKs: string[] }
const kokaExeName = os.platform() === "win32" ? "koka.exe" : "koka"
const latestVersion = "2.4.3"

const home = os.homedir();
export async function scanForSDK(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration): Promise<SDKs | undefined> {
  const processPath = (process.env.PATH as string) || ""
  const paths = processPath.split(path.delimiter).filter((p) => p)

  const dev = path.join(home, 'koka')
  let defaultSDK = ""
  let allSDKs = []
  if (fs.existsSync(dev)) {

    let command = 'stack path --local-install-root'
    const ghc = `${home}/.ghcup/env`
    if (fs.existsSync(ghc)) {
      // Linux ghcup installation does not show up in vscode's process.PATH,
      // ensure stack uses the correct ghc by sourcing the ghcup env script
      command = `${process.env.SHELL} -c "source ${ghc} && stack path --local-install-root"`
    }

    const options = { cwd: dev, env: process.env }
    const result = child_process.execSync(command, options)
    const devPath = result.toString().trim();
    // Prioritize dev
    const sdkPath = path.join(devPath, 'bin', kokaExeName)
    if (fs.existsSync(sdkPath)) {
      vs.window.showInformationMessage("Koka dev SDK found!")
      console.log("Koka: Using dev build of koka at " + devPath)
      defaultSDK = sdkPath
      allSDKs.push(defaultSDK)
    } else {
      vs.window.showInformationMessage("Koka dev environment found, but no built SDK")
    }
  }

  const local = path.join(home, '.local/bin')
  for (const p of [local].concat(paths)) {
    if (fs.existsSync(path.join(p, kokaExeName))) {
      console.log("Koka: Found build of koka at " + p)
      const sdkPath = path.join(p, kokaExeName)
      allSDKs.push(sdkPath)
      if (defaultSDK === "") {
        vs.window.showInformationMessage(`Using Koka SDK at ${p}`)
        defaultSDK = sdkPath
      }
    }
  }
  defaultSDK = config.get('languageServer.compiler') as string || defaultSDK
  if (defaultSDK === "" && !config.get('languageServer.compiler')) {
    console.log('Koka: No Koka SDK found')
    vs.window.showWarningMessage("Koka SDK not found on path or in ~/.local/bin")
    return await downloadSDK(context, config, false, undefined)
  } else if (semver.lt(getSDKVersion(defaultSDK), latestVersion) ) {
    return await downloadSDK(context, config, true, {sdkPath: defaultSDK, allSDKs: allSDKs })
  }
  return { sdkPath: defaultSDK, allSDKs: allSDKs }
}

function getSDKVersion(sdkPath: string): string {
  const options = { env: process.env }
  const result = child_process.execSync(`${sdkPath} --version`, options)
  const versionRegex = /version: ([0-9]+\.[0-9]+.[0-9]+)/g;
  const match = versionRegex.exec(result.toString())
  console.log("Koka: Found version " + match[1].toString())
  return match[1].toString();
}

export async function downloadSDK(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration, upgrade: boolean, old: (SDKs|undefined)): Promise<SDKs | undefined> {
  const response = context.globalState.get('koka-download')
  if (response === false){
    return old;
  }
  const decision = await vscode.window.showInformationMessage(
    `${upgrade ? "There is an update for koka available\n\n" : ""}Download and Install Koka, continue?`,
    { modal: true },
    'Yes',
    'No'
  )
  if (decision == 'No') {
    await context.globalState.update('koka-download', false)
    return old;
  }
  let command = "curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh && exit"
  if (os.platform() === "win32") {
    command = "curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && %tmp%\install-koka.bat && exit"
  }
  const term = vscode.window.createTerminal({ name: "Install Koka", cwd: home, shellPath: DefaultShellPath, isTransient: true, message: "Installing Koka" })
  term.sendText(command)
  term.show()
  let dispose: vscode.Disposable | undefined = undefined;
  const result = await new Promise<SDKs|undefined>((resolve, reject) => {
    let finished = false; 
    // Race between a 30 second timeout on watching terminals
    // and the terminal finishing installation
    setTimeout(() => {
      if (!finished){
        console.log("Koka: Installation timed out")
        resolve(undefined);
        finished = true;
      }
    }, 30000);
    dispose = vscode.window.onDidCloseTerminal(async (t) => {
      console.log("Terminal closed")
      if (t === term) {
        console.log("Koka: Installation finished")
        const sdk = await scanForSDK(context, config);
        if (!finished){
          const {sdkPath, allSDKs} = sdk
          if (sdkPath){
            console.log(path.join(sdkPath))
            const sdkRoot = path.dirname(path.dirname(sdkPath))
            const examples = path.join(sdkRoot, "share", "koka", `v${latestVersion}`, "lib", "samples")
            if (fs.existsSync(examples)) {
              let dest = path.join(context.globalStorageUri.fsPath, "samples")
              fs.cp(examples, dest, {recursive:true}, async (err) => {
                if (!err){
                  const decision = await vscode.window.showInformationMessage(
                    `Open Koka's latest samples folder?`,
                    { modal: true },
                    'Yes',
                    'Yes (new window)',
                    'No'
                  )
                  if (decision == 'No') {
                    return;
                  }
                  const examplesUri = vscode.Uri.file(dest)
                  vscode.commands.executeCommand('vscode.openFolder', examplesUri, {forceNewWindow : decision == 'Yes (new window)'})
                }
              })
              
            }
            console.log(examples)
            resolve(sdk);
          } else {
            resolve(undefined)
          }
          finished = true;
        }
      }
    })
  })
  dispose?.dispose()
  return result;
}

export async function uninstallSDK(context: vscode.ExtensionContext) {
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
    command = "curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && %tmp%\install-koka.bat -u -f"
  }
  const term = vscode.window.createTerminal({ name: "Uninstall Koka", cwd: home, shellPath: DefaultShellPath, isTransient: true, message: "Uninstalling Koka, you can close the terminal when done" })
  term.sendText(command)
  term.show()
}

const DefaultShellPath = os.platform() === "win32" ? "C:\\Windows\\System32\\cmd.exe" : null

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
  }
  defaultSDK: string
  sdkPath: string
  allSDKs: string[]
  config: vscode.WorkspaceConfiguration
  debugExtension: boolean
  command?: string | null
  langServerArgs: string[]
  additionalArgs: string[]
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