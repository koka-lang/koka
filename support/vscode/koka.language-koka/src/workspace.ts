import * as path from "path"
import * as fs from "fs"
import * as vs from "vscode"
import * as os from "os"
import * as vscode from "vscode"
import * as child_process from "child_process"

interface SDKs { sdkPath: string, allSDKs: string[] }
const kokaExeName = os.platform() === "win32" ? "koka.exe" : "koka"

const home = os.homedir();
export function scanForSDK(config: vscode.WorkspaceConfiguration): SDKs | undefined {
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
  if (defaultSDK === "" && !config.get('languageServer.kokaExecutable')) {
    console.log('Koka: No Koka SDK found')
    vs.window.showWarningMessage("Koka SDK not found on path or in ~/.local/bin")
    downloadSDK()
  } 
  return { sdkPath: defaultSDK, allSDKs: allSDKs }
}

export async function downloadSDK() {
  const decision = await vscode.window.showInformationMessage(
    `Download and Install the lastest Koka, continue?`,
    { modal: true },
    'Yes',
    'No'
  )
  if (decision == 'No'){
    return
  }
  let command = "curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh"
  if (os.platform() === "win32") {
    command = "curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && %tmp%\install-koka.bat"
  }
  const term = vscode.window.createTerminal({name: "Install Koka", cwd: home, shellPath: DefaultShellPath, isTransient: true, message: "Installing Koka, restart your editor when finished"}) 
  term.sendText(command)
  term.show()
}

export async function uninstallSDK() {
  const decision = await vscode.window.showInformationMessage(
    `Uninstall the system Koka installation, continue?`,
    { modal: true },
    'Yes',
    'No'
  )
  if (decision == 'No'){
    return
  }
  let command = "curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh -s -- -u -f"
  if (os.platform() === "win32") {
    command = "curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/latest/download/install.bat && %tmp%\install-koka.bat -u -f"
  }
  const term = vscode.window.createTerminal({name: "Uninstall Koka", cwd: home, shellPath: DefaultShellPath, isTransient: true, message: "Uninstalling Koka, you can close the terminal when done"}) 
  term.sendText(command)
  term.show()
}

const DefaultShellPath = os.platform() === "win32" ? "C:\Windows\System32\cmd.exe" : null

export class KokaConfig {
  constructor(config: vscode.WorkspaceConfiguration, sdkPath: string, allSDKs: string[]) {
    this.config = config
    this.debugExtension = config.get('debugExtension') as boolean
    this.defaultSDK = sdkPath
    this.sdkPath = config.get('languageServer.kokaExecutable') as string || sdkPath
    this.allSDKs = allSDKs
    this.cwd = config.get('languageServer.cwd') as string || vscode.workspace.workspaceFolders![0].uri.fsPath
    this.langServerArgs = []
    this.additionalArgs = config.get('languageServer.additionalArgs') as string[] || []
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
      console.log(`Koka executable not found at this location ${path}`)
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