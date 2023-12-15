
import * as vscode from "vscode"
import * as child_process from "child_process"
import { AddressInfo, Server, createServer } from 'net'

import {
  DidChangeConfigurationNotification,
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  StreamInfo,
} from 'vscode-languageclient/node'
import { KokaConfig } from "./workspace"

let stderrOutputChannel: vscode.OutputChannel
let stdoutOutputChannel: vscode.OutputChannel
let firstRun = true

export class KokaLanguageServer {
  languageClient?: LanguageClient
  languageServerProcess?: child_process.ChildProcess
  socketServer?: Server
  outputChannel?: vscode.OutputChannel
  lspWriteEmitter: vscode.EventEmitter<string> = new vscode.EventEmitter<string>();
  lspPty?: vscode.Pseudoterminal
  lspTerminal?: vscode.Terminal

  constructor(context: vscode.ExtensionContext) {
    if (firstRun) {
      stderrOutputChannel = vscode.window.createOutputChannel('Koka Language Server Stderr')
      stdoutOutputChannel = vscode.window.createOutputChannel('Koka Language Server Stdout')
      context.subscriptions.push(stderrOutputChannel)
      context.subscriptions.push(stdoutOutputChannel)
      firstRun = false;
    }
  }

  showOutputChannel() {
    if (!this.lspTerminal?.exitStatus) {
      this.outputChannel?.show()
    } else if (this.lspPty) {
      this.lspTerminal = vscode.window.createTerminal({
        name: 'Koka Language Server',
        pty: this.lspPty,
        isTransient: true
      })
      this.lspTerminal.show()
    }
  }

  async start(config: KokaConfig, context: vscode.ExtensionContext) {
    console.log(`Koka: Language Server ${config.command} ${config.langServerArgs.join(" ")} Workspace: ${config.cwd}`)
    let self = this;
    function serverOptions(): Promise<StreamInfo> {
      return new Promise((resolve, reject) => {
        let timeout = setTimeout(() => {
          reject("Server took too long to connect")
        }, 3000)
        self.socketServer = createServer((s) => {
          console.log("Got Connection to Client")
          clearTimeout(timeout)
          resolve({ writer: s, reader: s })
        }).listen(0, "127.0.0.1", () => {
          const port = (self.socketServer!.address() as AddressInfo).port
          console.log(`Starting language server in ${config.cwd} on port ${port}`)
          self.languageServerProcess = child_process.spawn(config.command, [...config.langServerArgs, `--lsport=${port}`], {
            cwd: config.cwd,
            env: process.env,
          })
          if (config.debugExtension) {
            self.languageServerProcess?.stderr?.on('data', (data) => {
              // console.log(data.toString())
              stderrOutputChannel.append(`${data.toString()}`)
            })
            self.languageServerProcess?.stdout?.on('data', (data) => {
              // console.log(data.toString())
              stdoutOutputChannel.append(`${data.toString()}`)
            })
          }
        })
      })
    }
    // This issue: https://github.com/microsoft/vscode/issues/571
    // This sample: https://github.com/ShMcK/vscode-pseudoterminal/blob/master/src/extension.ts
    this.lspPty = {
      onDidWrite: (listener) => this.lspWriteEmitter.event((e) => listener(e.replace('\r\n', '\n').replace('\n', '\r\n'))),
      open: () => { },
      close: () => { }
    };
    this.lspTerminal = vscode.window.createTerminal({
      name: 'Koka Language Server',
      pty: this.lspPty,
      isTransient: true
    })
    this.outputChannel = {
      name: 'Koka Language Server',
      append: (value: string) => this.lspWriteEmitter.fire(value),
      appendLine: (value: string) => {
        this.lspWriteEmitter.fire(value)
        this.lspWriteEmitter.fire('\r\n')
      },
      clear: () => {
        this.lspWriteEmitter.fire("\x1b[2J\x1b[3J\x1b[;H")
      },
      show: () => this.lspTerminal?.show(),
      hide: () => this.lspTerminal?.hide(),
      dispose: () => {
        this.lspTerminal?.dispose()
        this.lspWriteEmitter.dispose()
        this.lspPty?.close()
      },
      replace: (v) => {
        this.lspWriteEmitter.fire("\x1b[2J\x1b[3J\x1b[;H")
        this.lspWriteEmitter.fire(v)
      },

    };
    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ language: 'koka', scheme: 'file' }],
      outputChannel: this.outputChannel,
      revealOutputChannelOn: RevealOutputChannelOn.Never,
      markdown: {
        isTrusted: true,
        supportHtml: true,
      }
    }
    this.languageClient = new LanguageClient(
      'Koka Language Client',
      serverOptions,
      clientOptions,
    )
    context.subscriptions.push(this)

    await this.languageClient.start()
    let isDark = vscode.window.activeColorTheme.kind == vscode.ColorThemeKind.Dark
    this.languageClient.sendNotification(DidChangeConfigurationNotification.type, { settings: { colors: { mode: isDark ? "dark" : "light" } } })
    return this.languageClient
  }

  async dispose() {
    try {
      await this.languageClient?.stop()
      await this.languageClient?.dispose()
      const result = this.languageServerProcess?.kill('SIGINT')
      if (!result) {
        console.log("Failed to end language server with SIGINT, trying SIGTERM")
        this.languageServerProcess?.kill()
      }
      this.socketServer?.close()
      // TODO: Does the terminal need to be disposed or is that handled by disposing the client
    } catch {
      // Ignore for now, the process should automatically die when the server / client closes the connection
    }
  }
}