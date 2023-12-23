/*---------------------------------------------------------------------------
Copyright 2023, Tim Whiting, Daan Leijen

This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
import path = require("path");
import * as vscode from "vscode"
import { KokaConfig } from "./workspace";

export class MainCodeLensProvider implements vscode.CodeLensProvider {
  private onDidChangeCodeLensesEmitter: vscode.EventEmitter<void> = new vscode.EventEmitter<void>()

  constructor(private readonly config: KokaConfig) { }

  public async provideCodeLenses(document: vscode.TextDocument, token: vscode.CancellationToken): Promise<vscode.CodeLens[] | undefined> {
    const doc = document.getText()
    const hasModuleDecl = doc.match(/^module\b/);
    const re_canRun = (hasModuleDecl ?
                       /(?:(?<=\n)|^)(?:pub\s+)fun\s+(main|test[\w-]*|example[\w-]*)\(\s*\)/g :  // all must be pub
                       /(?:(?<=\n)|^)(?:pub\s+)?fun\s+(main|test[\w-]*|example[\w-]*)\(\s*\)/g); // pub is default
    let lenses = [];
    let match = null;
    console.log("Koka: Scanning document for main and test function");
    while (match = re_canRun.exec(doc)) {
      if (match[1] === "main") {
        lenses.push(...this.createMainCodeLens(document, match.index, match[0].length))
      }
      else {
        lenses.push(...this.createTestCodeLens(document, match.index, match[1], match[0].length))
      }
    }
    return lenses
  }

  private createMainCodeLens(document: vscode.TextDocument, offset: number, len: number): vscode.CodeLens[] {
    return [new vscode.CodeLens(
      toRange(document, offset, len),
      {
        arguments: [document.uri],
        command: "koka.runMain",
        title: "run debug", // `Run ${path.relative(this.config.cwd, document.uri.fsPath)} (debug)`,
        tooltip: "Compile and run in debug mode"
      },
    ), new vscode.CodeLens(
      toRange(document, offset, len),
      {
        arguments: [document.uri, "-O2", ["--kktime"]],
        command: "koka.runMain",
        title: `optimized`, // `Run ${path.relative(this.config.cwd, document.uri.fsPath)} (release)`,
        tooltip: "Compile with flag -O2\nRun executable with flag --kktime"
      },
    ),
    ]
  }

  private createTestCodeLens(document: vscode.TextDocument, offset: number, functionName: string, len: number): vscode.CodeLens[] {
    return [new vscode.CodeLens(
      toRange(document, offset, len),
      {
        arguments: [document.uri, functionName],
        command: "koka.runFunction",
        title: "run debug", //`Run ${functionName} (debug)`,
        tooltip: "Compile and run in debug mode"
      }
    ),
    new vscode.CodeLens(
      toRange(document, offset, len),
      {
        arguments: [document.uri, functionName, "-O2", ["--kktime"]],
        command: "koka.runFunction",
        title: `optimized`, // `Run ${functionName} (release)`,
        tooltip: "Compile with flag -O2\nRun executable with flag --kktime"
      }
    )
    ]
  }
}

function toRange(document: vscode.TextDocument, offset: number, length: number): vscode.Range {
  return new vscode.Range(document.positionAt(offset), document.positionAt(offset + length))
}