// Configuration for `npm test` (`vscode-test`, see https://github.com/microsoft/vscode-test-cli).
// KOKA_COMPILER is the compiler under test, e.g. `$(stack path --local-install-root)/bin/koka`.
import { defineConfig } from "@vscode/test-cli"
import * as fs from "fs"
import * as os from "os"
import * as path from "path"

const compiler = process.env.KOKA_COMPILER
if (!compiler || !fs.existsSync(compiler)) {
  throw new Error(`KOKA_COMPILER must name the koka executable to test (is: ${compiler})`)
}

// A fresh workspace of two folders whose settings select the compiler under test.
// Each folder is opened through a symbolic link (a junction on Windows), so the paths
// in the editor differ from the real paths the compiler uses (koka-lang/koka#970, #810).
function linkedFolder(name) {
  const real = fs.mkdtempSync(path.join(fs.realpathSync(os.tmpdir()), `koka-vscode-${name}-`))
  fs.symlinkSync(real, real + "-link", "junction")
  return real + "-link"
}
const folders   = [linkedFolder("test"), linkedFolder("lib")]
const workspace = path.join(path.dirname(folders[0]), path.basename(folders[0]) + ".code-workspace")
fs.writeFileSync(workspace, JSON.stringify({
  folders: folders.map(folder => ({ path: folder })),
  settings: { "koka.languageServer.compiler": compiler },
}, null, 2))

// VS Code's IPC socket lives in the user data directory and its path must stay
// short (103 characters on macOS), so it is not under this (deep) directory
const userData = fs.mkdtempSync(path.join(os.tmpdir(), "koka-vscode-user-"))

export default defineConfig({
  files: "out/test/**/*.test.js",
  workspaceFolder: workspace,
  launchArgs: ["--disable-extensions", `--user-data-dir=${userData}`],
  mocha: { ui: "tdd", timeout: 20 * 60 * 1000 },
})
