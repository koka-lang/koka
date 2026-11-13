/*---------------------------------------------------------------------------
Copyright 2026, Tim Whiting.

This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
import * as assert from "assert"
import * as child_process from "child_process"
import * as fs from "fs"
import * as path from "path"
import * as vscode from "vscode"

// The error moves from line 2 to line 3 and then goes away, so a diagnostic computed
// from the saved file (line 2) never matches a later state of the buffer.
const errorOnLine2 = 'pub fun main()\n  val answer : int = "bad"\n  val other : int = 1\n  println(answer + other)\n'
const errorOnLine3 = 'pub fun main()\n  val answer : int = 1\n  val other : int = "worse"\n  println(answer + other)\n'
const noErrors     = 'pub fun main()\n  val answer : int = 1\n  val other : int = 2\n  println(answer + other)\n'

const firstCheckTimeout = 5 * 60 * 1000  // the first check also builds the standard library
const checkTimeout      = 60 * 1000

// The workspace has two folders, each opened through a symbolic link (`.vscode-test.mjs`)
function workspaceFile(name: string, contents: string, folder = 0): string {
  const file = path.join(vscode.workspace.workspaceFolders![folder].uri.fsPath, name)
  fs.writeFileSync(file, contents)
  return file
}

const sameLines = (xs: number[], ys: number[]) => JSON.stringify(xs) === JSON.stringify(ys)

suite("Diagnostics", () => {

  // koka-lang/koka#970: the server checked the saved file instead of the buffer when the
  // editor's path differs from the real path (a symbolic link, or backslashes on Windows)
  test("follow unsaved edits", async () => {
    const doc = await vscode.workspace.openTextDocument(workspaceFile("edits.kk", errorOnLine2))
    await vscode.window.showTextDocument(doc)
    await editorErrors(doc.uri, [2], firstCheckTimeout)
    await replaceText(doc, errorOnLine3)
    await editorErrors(doc.uri, [3], checkTimeout)
    await replaceText(doc, noErrors)
    await editorErrors(doc.uri, [], checkTimeout)
    assert.ok(doc.isDirty, "the edits are never saved")
  })

  // koka-lang/koka#929: a clear was published with the version of the last errors,
  // which a client that checks versions drops (VS Code does not check them)
  test("are published with the current document version", async () => {
    const file   = workspaceFile("versions.kk", errorOnLine2)
    const server = new LspClient(process.env.KOKA_COMPILER!, path.dirname(file))
    try {
      await server.initialize()
      const uri = vscode.Uri.file(file).toString()
      server.notify("textDocument/didOpen", { textDocument: { uri, languageId: "koka", version: 1, text: errorOnLine2 } })
      await server.published(uri, 1, [2], firstCheckTimeout)
      server.notify("textDocument/didChange", { textDocument: { uri, version: 2 }, contentChanges: [{ text: errorOnLine3 }] })
      await server.published(uri, 2, [3], checkTimeout)
      server.notify("textDocument/didChange", { textDocument: { uri, version: 3 }, contentChanges: [{ text: noErrors }] })
      await server.published(uri, 3, [], checkTimeout)
    }
    finally {
      server.dispose()
    }
  })

  // An imported module in the other workspace folder is not open: its errors are still
  // published, under the path in that workspace folder rather than its real path
  test("of a closed module use the workspace folder's path", async () => {
    const broken = workspaceFile("wsbroken.kk", 'pub fun answer() : int\n  val x : int = "bad"\n  x\n', 1)
    const doc = await vscode.workspace.openTextDocument(workspaceFile("uses-broken.kk", "import wsbroken\n\npub fun main()\n  println(answer())\n"))
    await vscode.window.showTextDocument(doc)
    await editorErrors(vscode.Uri.file(broken), [2], checkTimeout)
  })

  // Go to definition in a module of the other workspace folder
  test("definitions use the workspace folder's path", async () => {
    const lib = workspaceFile("wslib.kk", "pub fun answer() : int\n  42\n", 1)
    const doc = await vscode.workspace.openTextDocument(workspaceFile("uses-lib.kk", "import wslib\n\npub fun main()\n  println(answer())\n"))
    await vscode.window.showTextDocument(doc)
    await editorErrors(doc.uri, [], checkTimeout)
    const defs = await vscode.commands.executeCommand<(vscode.Location | vscode.LocationLink)[]>(
                   "vscode.executeDefinitionProvider", doc.uri, new vscode.Position(3, 11))
    const targets = defs.map(d => ("targetUri" in d ? d.targetUri : d.uri).toString())
    assert.deepStrictEqual(targets, [vscode.Uri.file(lib).toString()])
  })
})

async function replaceText(doc: vscode.TextDocument, text: string) {
  const edit = new vscode.WorkspaceEdit()
  edit.replace(doc.uri, new vscode.Range(0, 0, doc.lineCount, 0), text)
  assert.ok(await vscode.workspace.applyEdit(edit))
}

// Wait until the Koka errors in the editor are on exactly these (1-based) lines
function editorErrors(uri: vscode.Uri, lines: number[], timeout: number): Promise<void> {
  const current = () => vscode.languages.getDiagnostics(uri)
    .filter(d => d.source === "koka" && d.severity === vscode.DiagnosticSeverity.Error)
    .map(d => d.range.start.line + 1).sort()
  return new Promise((resolve, reject) => {
    const check = () => { if (sameLines(current(), lines)) { done(); resolve() } }
    const sub   = vscode.languages.onDidChangeDiagnostics(e => { if (e.uris.some(u => u.toString() === uri.toString())) check() })
    const timer = setTimeout(() => { done(); reject(new Error(`expected errors on lines [${lines}], have [${current()}]`)) }, timeout)
    const done  = () => { sub.dispose(); clearTimeout(timer) }
    check()
  })
}

// A minimal LSP client over stdio that sees the version of each publishDiagnostics,
// which the VS Code API does not expose
class LspClient {
  private proc: child_process.ChildProcess
  private buffer = Buffer.alloc(0)
  private nextId = 0
  private listeners: ((msg: any) => void)[] = []
  private publishes: string[] = []

  constructor(compiler: string, dir: string) {
    assert.ok(compiler, "KOKA_COMPILER is set")
    this.proc = child_process.spawn(compiler, ["--language-server", "--buildtag=vscode", `-i${dir}`, "--lsstdio"], { cwd: dir })
    this.proc.stdout!.on("data", (data: Buffer) => this.receive(data))
  }

  private receive(data: Buffer) {
    this.buffer = Buffer.concat([this.buffer, data])
    for (;;) {
      const header = this.buffer.indexOf("\r\n\r\n")
      if (header < 0) return
      const length = Number(/Content-Length: (\d+)/i.exec(this.buffer.subarray(0, header).toString())![1])
      if (this.buffer.length < header + 4 + length) return
      const msg = JSON.parse(this.buffer.subarray(header + 4, header + 4 + length).toString())
      this.buffer = this.buffer.subarray(header + 4 + length)
      if (msg.id !== undefined && msg.method) this.send({ jsonrpc: "2.0", id: msg.id, result: null })  // e.g. progress requests
      this.listeners.forEach(l => l(msg))
    }
  }

  private send(msg: object) {
    const json = JSON.stringify(msg)
    this.proc.stdin!.write(`Content-Length: ${Buffer.byteLength(json)}\r\n\r\n${json}`)
  }

  notify(method: string, params: object) {
    this.send({ jsonrpc: "2.0", method, params })
  }

  initialize(): Promise<void> {
    const id = ++this.nextId
    const reply = this.next(msg => msg.id === id && !msg.method, checkTimeout, "initialize")
    this.send({ jsonrpc: "2.0", id, method: "initialize", params: { processId: process.pid, rootUri: null, capabilities: {} } })
    return reply.then(() => this.notify("initialized", {}))
  }

  // Wait for diagnostics of `uri` at `version` with errors on exactly these lines
  published(uri: string, version: number, lines: number[], timeout: number): Promise<void> {
    return this.next(msg => {
      if (msg.method !== "textDocument/publishDiagnostics") return false
      // URIs are compared parsed: the server may spell `file:///c%3A/...` as `file:///C:/...`
      const sameUri = vscode.Uri.parse(msg.params.uri).toString() === vscode.Uri.parse(uri).toString()
      const errors  = msg.params.diagnostics.filter((d: any) => d.severity === 1).map((d: any) => d.range.start.line + 1).sort()
      this.publishes.push(`${sameUri ? "" : msg.params.uri + " "}v${msg.params.version}: [${errors}]`)
      return sameUri && msg.params.version === version && sameLines(errors, lines)
    }, timeout, `version ${version} with errors on lines [${lines}]`)
  }

  private next(pred: (msg: any) => boolean, timeout: number, what: string): Promise<void> {
    return new Promise((resolve, reject) => {
      const listener = (msg: any) => { if (pred(msg)) { done(); resolve() } }
      const timer = setTimeout(() => {
        done(); reject(new Error(`expected ${what}, published: ${this.publishes.join(", ") || "nothing"}`))
      }, timeout)
      const done = () => { this.listeners = this.listeners.filter(l => l !== listener); clearTimeout(timer) }
      this.listeners.push(listener)
    })
  }

  dispose() {
    this.proc.kill()
  }
}
