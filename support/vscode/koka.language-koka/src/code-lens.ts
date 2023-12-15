import path = require("path");
import * as vscode from "vscode"
import { KokaConfig } from "./workspace";

export class MainCodeLensProvider implements vscode.CodeLensProvider {
  private onDidChangeCodeLensesEmitter: vscode.EventEmitter<void> = new vscode.EventEmitter<void>()

  constructor(private readonly config: KokaConfig) { }

  public async provideCodeLenses(document: vscode.TextDocument, token: vscode.CancellationToken): Promise<vscode.CodeLens[] | undefined> {
    const doc = document.getText()
    const re_main = /((?<=\n)|^)((pub\s+)?fun\s+main\(\))/g;
    const re_test = /((?<=\n)|^)((pub\s+)?fun\s+(test[\w-]*)\(\))/g;
    const re_example = /((?<=\n)|^)((pub\s+)?fun\s+(example[\w-]*)\(\))/g;
    let lenses = [];
    let match = null;
    let has_main = false;
    console.log("Koka: Scanning document for main and test function");
    while (match = re_main.exec(doc)) {
      if (has_main){
        console.log("Koka: Found multiple main functions. This is not supported in the compiler.")
        return [];
      }
      has_main = true;
      lenses.push(this.createMainCodeLens(document, match.index, match[0].length))
    }
    while (match = re_test.exec(doc)) {
      if (has_main){
        console.log("Koka: Found both a main and a test function. Only the main function will be runnable via code lens.")
        break;
      }
      lenses.push(this.createTestCodeLens(document, match.index, match[4], match[0].length))
    }
    while (match = re_example.exec(doc)) {
      if (has_main){
        console.log("Koka: Found both a main and an example function. Only the main function will be runnable via code lens.")
        break;
      }
      lenses.push(this.createTestCodeLens(document, match.index, match[4], match[0].length))
    }
    return lenses
  }

  private createMainCodeLens(document: vscode.TextDocument, offset: number, len: number): vscode.CodeLens {
    return new vscode.CodeLens(
      toRange(document, offset, len),
      {
        arguments: [document.uri],
        command: "koka.startWithoutDebugging",
        title: `Run ${path.relative(this.config.cwd, document.uri.fsPath)}`,
      }
    )
  }

  private createTestCodeLens(document: vscode.TextDocument, offset: number, functionName: string, len: number): vscode.CodeLens {
    return new vscode.CodeLens(
      toRange(document, offset, len),
      {
        arguments: [document.uri, functionName],
        command: "koka.interpretExpression",
        title: `Run ${functionName}`,
      }
    )
  }
}

function toRange(document: vscode.TextDocument, offset: number, length: number): vscode.Range {
  return new vscode.Range(document.positionAt(offset), document.positionAt(offset + length))
}