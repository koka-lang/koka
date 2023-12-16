# Koka Syntax Highlighting and Language Server

Syntax highlighting and language server support for the
Koka programming language in Visual Studio Code.

Also includes language server support, and easy installation of the latest Koka SDK.

Visit <https://koka-lang.github.io> for more information.

## Language Server

The language server continously analyses the code to show
parse- and type errors, complete identifiers,
show type information on hover, and
can execute `main`, `test-xxx`, and `example-xxx` functions
directly in the debug console.

### Easy SDK Installation
Open the command panel in VSCode `(Ctrl/Cmd+Shift+P)` and run the `Koka: Download and Install Latest Version` command. (Start typing the command and it should surface to the top.)

If Koka doesn't detect an existing installation, it will prompt to run this command automatically.

### Customize

The extension shows "inlay hints" for inferred types of parameters
and local declarations. You can toggle inlay hints on- and
off in the editor inlay hints settings.
In the extension settings, you can also set the Koka compiler
path and specific compiler flags manually.

### Running Files

You can create custom run configurations in your `launch.json` file, just like any other programming language.
For koka you have the following options:
```json
{
  "type": "koka",
  "request": "launch",
  "program": "", // The path to the file you want to run
  "name": "", // The name as you want it to appear in the run configurations dropdown
  "functionName": "", // optional function name to run (must be a function that doesn't use any effects other than io effects and returns a showable value)
  "programArgs": [], // optional arguments you want to give to the compiled program
  "compilerArgs": "", // optional arguments you want to give to the compiler (e.g. --verbose or -O2)
}
```

Although it pulls up the debug panels, it doesn't support debugging yet.

Compilation progress will be shown in the language server Terminal window, but the debug console will be used to show the output of the program.


### Language Server Configuration

By default the language server support is enabled. To disable it, add the following to your `settings.json` file:
```json
{
  "koka.languageServer.enabled": false,
}
```

If you would like additional arguments to all invocations of the compiler, you can add them to your `settings.json` file:
```json
{
  "koka.languageServer.compilerArgs": ["--verbose"],
}
```

To change the current working directory for the language server (by default the first workspace folder), add the following to your `settings.json` file:
```json
{
  "koka.languageServer.cwd": "/path/to/working/directory",
}
``` 

### Supported Language Server Aspects
- [x] Diagnostics
- [x] Code completion
- [x] Hover information
- [x] Find definitions
- [x] Inlay hints
- [x] Document outline
- [x] Code folding ranges

## Token Classes

* `koka.conid`: constructors.
* `koka.op`: operators.
* `koka.id`: identifiers.
* `koka.id.decl`(`.function`|`.val`|`.var`): declarations.
* `koka.id.library`(`.resume`|`.finally`|...): control related library identifiers.
* `koka.moduleid`: module identifiers.
* `koka.keyword`: keywords.
* `koka.keyword.control`: control flow keywords.
* `koka.special`: reserved separators (`{};,` etc).
* `koka.special.dot`: the dot separator.
* `koka.number`: numbers.
* `koka.string`(`.invalid`|`.escape`|`.raw`): string literals.
* `koka.char`(`.invalid`|`.escape`): character literals.
* `koka.type`(`.kind`|`.special`|`.typevar`|`.typecon`): types.
* `koka.comment`(`.line`|`.block`): comments.
* `koka.comment.doc`(`.emph`|`.pre`|`.pre.type`|`.pre.block`): documentation inside a comment.

### Customize

You can customize the Koka syntax highlighting by editing
the `settings.json` file of VS Code (press `Ctrl/Cmd+Shift+P` and
select "Open Settings (JSON)" to open it).
Then add a [editor.tokenColorCustomizations](https://code.visualstudio.com/docs/getstarted/themes#_editor-syntax-highlighting)
entry, for example:
```json
"editor.tokenColorCustomizations": {
  "textMateRules": [
    { "scope": "koka.type",
      "settings": { "foreground": "#00B8B8" }
    },
    { "scope": "koka.conid, koka.number",
      "settings": { "foreground": "#a6c2a3" }
    },
    { "scope": "koka.comment.doc.pre",
      "settings": { "foreground": "#91ac91" }
    },
    { "scope": "koka.id.decl.function",
      "settings": { "foreground": "#cac199" }
    },
  ]
}
```

## Extension Commands

The language server opens an extra terminal which shows the output of the compiler. 
This can be closed, but a new terminal will be opened whenever the language server is restarted. 
You can also reopen it by running the `Koka: Show Language Server Output` command in the vscode command panel `(Ctrl/Cmd+Shift+P)`.

To uninstall the Koka SDK run the `Koka: Uninstall System SDK` command in the vscode command panel `(Ctrl/Cmd+Shift+P)`.
