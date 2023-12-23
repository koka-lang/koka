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

// Development: set kokaDevDir to (un)install from a local bundle instead of github
const kokaDevDir    = "c:/users/daan/dev/koka-ls"
const kokaBundleBase= `${kokaDevDir}/bundle/v${latestVersion}/koka-v${latestVersion}`
const kokaBundle    = (os.platform() == "win32"
                       ? `${kokaBundleBase}-windows-x64.tar.gz`
                       : (os.platform() == "darwin"
                           ? `${kokaBundleBase}-osx-arm64.tar.gz`
                           : `${kokaBundleBase}-linux-x64.tar.gz`))


// Configuration
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


export async function findInstallSDK(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration): Promise<SDKs | undefined> {
  const sdk = findSDK(config);
  if (sdk.sdkPath === "") {
    console.log('Koka: unable to find an installed Koka compiler')
    const message = "The Koka compiler cannot be not found in the PATH"
    return await downloadKoka(context, config, message, false, undefined)
  }
  else {
    const version = getKokaVersion(sdk.sdkPath)
    if (semver.lt(version, latestVersion) ) {
      const message = `The currently installed Koka compiler is version ${version} while the latest is ${latestVersion}`
      return await downloadKoka(context, config, message, true, sdk)
    }
  }
  console.log("Koka: using Koka compiler at: " + sdk.sdkPath);
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
      vs.window.showInformationMessage(`Koka: configured compiler path does not exist: ${exeConfig}`)
      //console.log('Koka: cannot find configured compiler: ' + exeConfig)
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
        vs.window.showInformationMessage(`Koka: found developer build at: ${devp}`)
        //console.log("Koka: Using dev build of koka at " + exePath)
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
      console.log("Koka: found Koka executable at: " + p)
      const exePath = path.join(p, kokaExeName)
      exePaths.push(exePath)
    }
  }

  return exePaths.reverse()
}


function getKokaVersion(exePath: string) : string {
  const options = { env: process.env }
  const result = child_process.execSync(`${exePath} --version`, options)
  const versionRegex = /version: ([0-9]+\.[0-9]+.[0-9]+)/g;
  const match = versionRegex.exec(result.toString())
  console.log("Koka: found installed version " + match[1].toString())
  return match[1].toString();
}

export async function downloadKoka(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration,
                                   reason: string, upgrade: boolean, old: (SDKs|undefined)): Promise<SDKs | undefined> {
  // only prompt once for a download for each new extension version
  let response = await context.globalState.get('koka-download')
  if (typeof response !== 'string') {
    response = ""
  }
  else if (response && semver.eq(response, latestVersion)){
    return old;
  }
  const decision = await vscode.window.showInformationMessage(
    `${(reason ? reason + ".  \n" : "")}Would you like to download and install the latest Koka compiler?`,
    { }, // modal: true },
    'Yes',
    'No'
  )
  if (decision == 'No') {
    await context.globalState.update('koka-download', latestVersion)
    return old;
  }
  else if (decision != 'Yes') { // cancel
    return old;
  }

  // download and install in a terminal
  let command = ""
  const flags = "--vscode"  // TODO: add `--force` to force all default actions? (like installing clang on windows if needed)
  if (os.platform() === "win32") {
    if (kokaDevDir) {
      const kokadev = "c:/users/daan/dev/koka-ls"
      command = `${kokaDevDir}/util/install.bat ${flags} ${kokaBundle} && exit`
    }
    else {
      const tmpDir = (process.env.TMP || process.env.TEMP || "%HOMEDRIVE%%HOMEPATH%")
      command = `curl -sSL -o "${tmpDir}\\install-koka.bat" https://github.com/koka-lang/koka/releases/latest/download/install.bat && "${tmpDir}\\install-koka.bat" ${flags} && exit`
    }
  }
  else {
    if (kokaDevDir) {
      const kokadev = "c:/users/daan/dev/koka-ls"
      command = `${kokaDevDir}/util/install.sh ${flags} ${kokaBundle} && exit`
    }
    else {
      command = `curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh -s -- ${flags} && exit`
    }
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
        await context.globalState.update('koka-open-samples',"")  // open samples later on
        // const sdk = await findInstallSDK(context, config); // scan again for the just installed SDK
        const sdk = findSDK(config);
        const message = (sdk.sdkPath ? `Koka installed successfully at ${sdk.sdkPath}`
                                      : `Koka install finished but unable to find the installed compiler`)
        console.log(message)
        resolve( sdk.sdkPath ? sdk : undefined )  // todo: can we avoid undefined?
        await vscode.window.showInformationMessage(message)
      }
    })
  })
  dispose?.dispose()
  return result;
}

export async function uninstallKoka(context: vscode.ExtensionContext) {
  const decision = await vscode.window.showInformationMessage(
    `Uninstalling the system Koka compiler, continue?`,
    { modal: true },
    'Yes',
    'No'
  )
  if (decision != 'Yes') {  // can be Cancel
    return
  }

  let command = ""
  const flags = "--uninstall --force --vscode"
  if (os.platform() === "win32") {
    if (kokaDevDir) {
      const kokadev = "c:/users/daan/dev/koka-ls"
      command = `"${kokaDevDir}/util/install.bat" ${flags}`
    }
    else {
      const tmpDir = (process.env.TMP || process.env.TEMP || "%HOMEDRIVE%%HOMEPATH%")
      command = `curl -sSL -o "${tmpDir}\\install-koka.bat" https://github.com/koka-lang/koka/releases/latest/download/install.bat && "${tmpDir}\\install-koka.bat" ${flags}`
    }
  }
  else {
    if (kokaDevDir) {
      const kokadev = "c:/users/daan/dev/koka-ls"
      command = `${kokaDevDir}/util/install.sh ${flags}`
    }
    else {
      command = "curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh -s -- ${flags}"
    }
  }
  const term = vscode.window.createTerminal({ name: "Uninstall Koka", cwd: home, shellPath: defaultShell, isTransient: true, message: "Uninstalling Koka, you can close the terminal when done" })
  term.sendText(command)
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

export async function openSamples(context: vscode.ExtensionContext, config: vscode.WorkspaceConfiguration) {
  const samplesDir: string = await context.globalState.get(`koka-samples-${latestVersion}`)
  console.log("Koka: openSamples: " + samplesDir)
  if (!samplesDir || !fs.existsSync(samplesDir)) {
    const sdkPath = findSDK(config).sdkPath
    if (sdkPath){
      const examples = getSamplesDir(sdkPath)
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
