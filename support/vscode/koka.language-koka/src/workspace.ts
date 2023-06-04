import * as path from "path"
import * as fs from "fs"
import * as vs from "vscode"
import * as os from "os"
import * as vscode from "vscode"

interface SDKs {sdkPath: string, allSDKs: string[]}

export function scanForSDK(): SDKs {
  const processPath = (process.env.PATH as string ) || ""
  const paths = processPath.split(path.delimiter).filter((p) => p)

  const dev = path.join(process.env.HOME, 'koka', '.stack-work', 'install', os.arch().replace('x64', 'x86_64') + "-" + os.platform().replace('darwin', 'osx'))
  let defaultSDK = ""
  let allSDKs = []
  if (fs.existsSync(dev)){
    // Prioritize dev
    const devPath = findDevKokaSDK(dev)
    if (fs.existsSync(devPath)){
      vs.window.showInformationMessage("Koka dev SDK found!")
      console.log("Koka: Found dev build of koka at " + devPath)
      defaultSDK = path.join(devPath, kokaExeName)
      allSDKs.push(defaultSDK)
    }
  }
  
  const local = path.join(process.env.HOME,'.local/bin')
  for (const p of [local].concat(paths)){
    if (fs.existsSync(path.join(p, kokaExeName))){
      vs.window.showInformationMessage(`Using Koka SDK at ${p}`)
      console.log("Koka: Found build of koka at " + p)
      const sdkPath = path.join(p, kokaExeName)
      allSDKs.push(sdkPath)
      if (defaultSDK === ""){
        defaultSDK = sdkPath
      }
    }
  }
  if (defaultSDK === "") {
    console.log('Koka: No Koka SDK found')
    vs.window.showWarningMessage("Koka SDK not found on path or in ~/.local/bin")
  } else {
    return {sdkPath: defaultSDK, allSDKs: allSDKs}
  }
} 

// Recursively find the file bin/koka ordered by latest modification time
function findDevKokaSDK(dir: string): string {
  const files = fs.readdirSync(dir)
  const koka = files.filter((f) => f.endsWith(kokaExeName))
  if (koka.length > 0) {
    return dir
  }
  const dirs = files.filter((f) => fs.statSync(path.join(dir, f)).isDirectory())
  const sorted = dirs
    .map((d) => ({ path: findDevKokaSDK(path.join(dir, d)), mtime: fs.statSync(path.join(dir, d)).mtime }))
    .filter((d) => d.path)
    .sort((a, b) => a.mtime.getUTCMilliseconds() - b.mtime.getUTCMilliseconds())
  return sorted[0]?.path
}

const kokaExeName = os.platform() === "win32" ? "koka.exe" : "koka"


export class KokaConfig {
  constructor(config: vscode.WorkspaceConfiguration, sdkPath: string, allSDKs: string[]) {
    this.config = config
    this.defaultSDK = sdkPath
    this.sdkPath = sdkPath
    this.allSDKs = allSDKs
    this.cwd = config.get('languageServer.cwd') as string || vscode.workspace.workspaceFolders[0].uri.path
    this.selectSDK(sdkPath)
    this.target = "C"
  }
  defaultSDK: string
  sdkPath: string
  allSDKs: string[]
  config: vscode.WorkspaceConfiguration
  command: string
  langServerCommand: string
  target: string
  cwd: string

  selectSDK(path: string){
    if (!fs.existsSync(path)){
      return
    }
    this.command = this.config.get('languageServer.command') as string || `${this.sdkPath}`
    this.langServerCommand = `${this.command} --language-server -i${this.cwd}`
  }

  selectTarget(t: string){
    if(!["C", "JS", "WASM", "C#"].includes(t)){
      return
    }
    this.target = t
  }
}