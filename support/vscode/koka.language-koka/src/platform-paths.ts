
import * as os from "os"
// Constants
export const home = os.homedir();
export const kokaExeName = (os.platform() === "win32" ? "koka.exe" : "koka")
export const defaultShell = (os.platform() === "win32" ? "C:\\Windows\\System32\\cmd.exe" : null)
export const binaryPlatforms = ["windows-x64", "macos-arm64", "macos-x64", "linux-x64", "linux-arm64"]
export const platform = osGetPlatform()
export const arch = os.arch()
export const targetPlatform = `${platform}-${arch}`

function osGetPlatform() : string {
  var platform = os.platform()
  if (platform=="win32") return "windows"
  else if (platform=="darwin") return "macos"
  else return platform
}

export function expandHome( path : string ) : string {
  return (path ? path.replace("~",home) : "");
}
