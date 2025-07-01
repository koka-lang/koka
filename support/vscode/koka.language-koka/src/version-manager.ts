
import fetch from 'node-fetch' // Use node-fetch to fetch the latest Koka compiler release
import * as path from "path"
import * as fs from "fs"
import * as vscode from "vscode"
import * as child_process from "child_process"
import * as semver from "semver"
import { binaryPlatforms, defaultShell, expandHome, home, kokaExeName, platform, targetPlatform } from "./platform-paths"

// Development: set kokaDevDir to a non-empty string to (un)install from a local bundle instead of github
const kokaDevDir = ""
//"c:/users/daan/dev/koka"
// "/Users/daan/dev/koka-dev"

// Represents a release of the Koka compiler as found on GitHub
export class KokaRelease {
  constructor(public downloadUrl: string, public version: string, public isPrerelease: boolean, public date : Date) {}
}

const kokaDefaultRelease = new KokaRelease("","3.1.2",false,new Date("2024-05-30"));

// Manages discovering, installing, and managing Koka compiler versions
export class VersionManager {
  constructor(private context: vscode.ExtensionContext, private vsConfig: vscode.WorkspaceConfiguration) {
    this.usePrereleases = vsConfig.get('dev.usePrereleaseCompilers') as boolean ?? false;
    this.developmentPath = expandHome(this.vsConfig.get('dev.developmentPath') as string ?? "")
  }

  usePrereleases: boolean = false; // whether to use prerelease compilers

  developmentPath: string               // root path to a development repository of the compiler

  compilerPath: string = ""             // path in use for the compiler
  compilerVersion: string = "1.0.0"     // version of that compiler
  compilerPaths: string[] = []          // all found paths to koka compilers in the system

  releases: KokaRelease[] = [kokaDefaultRelease];

  getLatestCompilerRelease() : KokaRelease {
    return this.releases.find(r => !r.isPrerelease || this.usePrereleases) ?? kokaDefaultRelease;
  }

  getLatestCompilerReleaseVersion() : string {
    return this.getLatestCompilerRelease().version;
  }

  async updateLatestKokaReleases() : Promise<void> {
    try {
      const response = await fetch('https://api.github.com/repos/koka-lang/koka/releases'); // Fetch all releases
      if (!response.ok) { // Check for HTTP errors
        console.error('Error fetching Koka releases: ', response.statusText);
        return;
      }
      const releasesJson : any = await response.json(); // Parse the JSON response
      if (!releasesJson) return;
      this.releases = releasesJson.map(release => {
        const asset = release.assets.find(a => a.name.includes(targetPlatform));
        if (asset) {
          // this gets the version from the tagname (without a preceding 'v')
          const version : string = semver.coerce(release.tag_name, {includePrerelease: true}).format();
          if (version && semver.gte(version,"2.4.0")) {
            return new KokaRelease(asset.browser_download_url, version, release.prerelease, new Date(release.created_at));
          }
        }
        return null;
      }).filter(release => release !== null) as KokaRelease[]; // Find the latest releases for the current platform
    } catch (err) {
      console.error('Error fetching Koka releases: ', err);
    }
  }


  /*-------------------------------------------------
    install
  -------------------------------------------------*/

  async installedVersion() : Promise<string> {
    return await this.context.globalState.get('koka-latest-installed-compiler') as string ?? "1.0.0";
  }

  async setInstalledVersion(version: string) : Promise<void> {
    await this.context.globalState.update('koka-latest-installed-compiler', version);
  }

  // force indicates that the user has explicitly requested to install
  async installKoka(
    reason: string,
    developmentPath: string,
    targetVersion : string
  ): Promise<string[]>
  {
    var force = true;
    var targetRelease : KokaRelease;
    if (!targetVersion || targetVersion == "latest") {
      force = false;
      targetRelease = this.getLatestCompilerRelease();
    }
    else {
      force = true;
      targetRelease = this.releases.find(r => r.version === targetVersion);
      if (!targetRelease) { targetRelease = this.getLatestCompilerRelease(); }
    }

    // only prompt once for a download for each new extension version
    if (!force) {
      const latestInstalled = await this.installedVersion();
      console.log(`Koka: latest installed compiler: ${latestInstalled}, latest released compiler is ${targetRelease.version}`)
      if (semver.gte(latestInstalled, targetRelease.version)) {
        return
      }
    }

    // check platform
    let warning = ""
    if (!binaryPlatforms.includes(targetPlatform)) {
      warning = `Unfortunately, it looks like your platform ${targetPlatform} does not have a binary installer -- see <https://github.com/koka-lang/koka> for build instructions.  `
    }

    if (!force) {
      // ask the user to install
      const decision = await vscode.window.showInformationMessage(
        `${(reason ? reason + ".  \n" : "")}${warning}Would you like to download and install the latest Koka compiler?`,
        {}, // modal: true },
        'Yes',
        'No'
      )
      if (decision == 'No') {
        // pretend it is installed and don't auto prompt again in the future (until a more recent version is released)
        await this.setInstalledVersion(targetRelease.version);
        return
      } else if (decision != 'Yes') { // cancel
        return
      }
    }

    // download and install in a terminal
    let shellCmd = ""
    let flags = "--vscode"
    let version = targetRelease.version;
    let idx = version.indexOf('-');
    if (idx >= 0) {
      // this is a tagged version, like v3.1.3-alpha1; in that case extract the actual version and pass to the installer
      version = targetRelease.version.substring(0,idx);
      flags += ` --version=${version} ${targetRelease.downloadUrl}`
    }
    else {
      // regular version: the installer can figure out the download url
      flags += ` --version=${targetRelease.version}`
    }

    // note: we always use the latest installer to install any earlier versions as well
    if (platform === "windows") {
      if (kokaDevDir) {
        const kokaBundle = this.getKokaBundleDir(kokaDevDir, targetRelease.version)
        shellCmd = `${kokaDevDir}/util/install.bat ${flags} ${kokaBundle} && exit`
      }
      else {
        const tmpDir = (process.env.TMP || process.env.TEMP || "%HOMEDRIVE%%HOMEPATH%")
        shellCmd = `curl -sSL -o "${tmpDir}\\install-koka.bat" https://github.com/koka-lang/koka/releases/latest/download/install.bat && "${tmpDir}\\install-koka.bat" ${flags} && exit`
      }
    }
    else {
      if (kokaDevDir) {
        const kokaBundle = this.getKokaBundleDir(kokaDevDir, targetRelease.version)
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
          const paths = this.findCompilerPaths(developmentPath); // rescan to find the just installed compiler
          let message = ""
          if (paths.length > 0) {
            // TODO: we cannot be sure the first path entry is the newly installed compiler.
            const defaultPath = paths[0]
            const compilerVersion = this.getCompilerVersion(defaultPath) ?? "1.0.0"
            if (semver.eq(compilerVersion,version)) {
              message = "Koka installed successfully"
              await this.setInstalledVersion(targetRelease.version);
            }
            else {
              message = `Koka may not have installed successfully: current version ${compilerVersion}, while the install version was ${targetRelease.version}`
            }
          }
          else {
            message = "Koka installation finished, but unable to find the installed compiler"
          }
          console.log(message)
          resolve(paths)
          await vscode.window.showInformationMessage(message)
        }
      })
    })
    dispose?.dispose()
    return result;
  }


  getKokaBundleDir(kokaDir: string, version: string): string {
    const kokaBundleBase = `${kokaDir}/bundle/v${version}/koka-v${version}`
    const kokaBundle = `${kokaBundleBase}-${targetPlatform}.tar.gz`
    return kokaBundle
  }


  /*-------------------------------------------------
    Uninstall
  -------------------------------------------------*/

  async uninstallKoka() {
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
    if (platform === "windows") {
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
    find compiler paths
  -------------------------------------------------*/

  async findInstallCompilerPaths(developmentPath: string): Promise<string[]> {
    const paths = this.findCompilerPaths(developmentPath)

    await this.updateLatestKokaReleases();

    if (paths.length === 0) {
      console.log('Koka: unable to find an installed Koka compiler')
      const reason = "The Koka compiler cannot be not found in the PATH"
      await this.installKoka(reason, developmentPath, "latest")
      return this.findCompilerPaths(developmentPath)
    }

    const defaultPath = paths[0]
    const compilerVersion = this.getCompilerVersion(defaultPath) ?? "1.0.0"
    const latestCompilerVersion = this.getLatestCompilerReleaseVersion();
    if (semver.lt(compilerVersion, latestCompilerVersion)) {
      const reason = `The currently installed Koka compiler is version ${compilerVersion} while the latest is ${latestCompilerVersion}`
      await this.installKoka(reason, developmentPath, "latest")
      return this.findCompilerPaths(developmentPath)
    }


    console.log("Koka: using Koka compiler at: " + defaultPath);
    return paths
  }

  findCompilerPaths(developmentPath: string): string[] {
    let compPaths = []

    // add user configured path?
    const userCompiler = expandHome(this.vsConfig.get('languageServer.compiler') as string || "")
    if (userCompiler) {
      if (!fs.existsSync(userCompiler)) {
        vscode.window.showInformationMessage(`Koka: configured compiler path does not exist: ${userCompiler}`)
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
      if (platform!="windows") {
        const ghcEnv = `${home}/.ghcup/env`
        if (fs.existsSync(ghcEnv)) {
          cmdGetInstallRoot = `${process.env.SHELL} -c "source ${ghcEnv} && stack path --local-install-root"`
        }
      }

      const result = child_process.execSync(cmdGetInstallRoot, { cwd: developmentPath, env: process.env })
      const exePath = path.join(result.toString().trim(), 'bin', kokaExeName)
      if (fs.existsSync(exePath)) {
        // vscode.window.showInformationMessage(`Koka: found developer build at: ${exePath}`)
        console.log("Koka: found development build of koka at " + exePath)
        compPaths.push(exePath)
      }
      else {
        // vscode.window.showInformationMessage(`Koka: cannot find developer build at: ${exePath}`)
        console.log("Koka: developer environment found, but no binary was built")
      }
    }

    // check PATH and local binary installation directories
    const paths = ((process.env.PATH as string) || "").split(path.delimiter)
    if (process.env.XDG_BIN_DIR) paths.push(process.env.XDG_BIN_DIR)
    paths.push(path.join(home, '.local', 'bin'))
    if (platform==="windows" && process.env.LOCALAPPDATA) {
      paths.push(path.join(process.env.LOCALAPPDATA,"koka","bin"))
    }

    for (const p of paths) {
      if (fs.existsSync(path.join(p, kokaExeName))) {
        console.log("Koka: found Koka executable at: " + p)
        compPaths.push(path.join(p, kokaExeName))
      }
    }

    return compPaths
  }


  getCompilerVersion(compilerPath: string): string {
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
    catch (err) { }
    return version
  }


  getCompilerSamplesDir(compilerPath: string, compilerVersion: string): string {
    if (compilerPath.includes(".stack-work")) {
      const root = compilerPath.substring(0, compilerPath.indexOf(".stack-work"))  // <root>/.stack-work/.../bin/koka
      return path.join(root, "samples")
    }
    else {
      const root = path.dirname(path.dirname(compilerPath))  // <root>/bin/koka
      const ver = this.getCompilerVersion(compilerPath) || compilerVersion;
      const examples = path.join(root, "share", "koka", `v${ver}`, "lib", "samples")
      return examples;
    }
  }


  // Does the compiler path point to a valid compiler?
  hasValidCompiler(): Boolean {
    if (this.compilerPath && fs.existsSync(this.compilerPath))
      return true
    else
      return false
  }

  // Search for Koka compilers along user specified path, the PATH, and development paths.
  // and possibly prompt the use to install the koka compiler if it is not found or out of date
  async updateCompilerPaths(allowInstall: Boolean): Promise<Boolean> {
    let paths = []
    if (allowInstall) paths = await this.findInstallCompilerPaths(this.developmentPath)
    else paths = this.findCompilerPaths(this.developmentPath)

    if (paths.length === 0) {
      console.log("Koka: cannot find a compiler")
      return false;
    }
    this.compilerPaths = paths;
    return this.setCompilerPath(this.compilerPaths[0])
  }

  // Set an explicit path to use for the Koka compiler
  setCompilerPath(path: string): Boolean {
    if (!path || !fs.existsSync(path)) {
      console.log(`Koka: compiler not found at: ${path}`)
      return false
    }
    // Test if we can execute
    const compilerVersion = this.getCompilerVersion(path)
    if (!compilerVersion) {
      console.log(`Koka: unable to get compiler version of: ${path}`)
      return false
    }
    this.compilerVersion = compilerVersion
    this.compilerPath = path
    return true
  }

  // install the latest Koka compiler
  async installCompiler(targetVersion : string): Promise<Boolean> {
    await this.installKoka("", this.developmentPath, targetVersion)
    // todo: use instead `this.updateCompilerPath(pathToTheJustInstalledCompiler)`
    return this.updateCompilerPaths(false)
  }

  // uninstall the latest Koka compiler
  async uninstallCompiler(): Promise<Boolean> {
    await this.uninstallKoka()
    return this.updateCompilerPaths(false)
  }
}