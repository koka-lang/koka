# Koka Syntax Highlighting and Language Server

Syntax highlighting and language server support for the
Koka programming language in Visual Studio Code.

Visit <https://koka-lang.github.io> for more information.

# Language Server

The language server continously analyses the code to show
parse- and type errors, complete identifiers,
show type information on hover, and
can execute `main`, `test-xxx`, and `example-xxx` functions
directly in the debug console.

## Install the latest Koka compiler

Open the command panel in VSCode `(Ctrl/Cmd+Shift+P)` and run the
`Koka: Download and Install Latest Version` command (when you start
typing the command and it should surface to the top).
In the extension settings, you can also set the Koka compiler
path and specific compiler flags manually.

## Customization

The extension shows _inlay hints_ for inferred types of parameters
and local declarations. You can toggle the visibility of inlay hints
in the editor _inlay hints_ settings.

You can create custom launch configurations in your `launch.json` file.
The following settings are available:
```json
{
  "type": "koka",
  "request": "launch",
  "program": "",      // The path to the file you want to run
  "name": "",         // The name as you want it to appear in the run configurations dropdown
  "functionName": "", // optional function name to run
  "programArgs": [],  // optional arguments you want to give to the compiled program
  "compilerArgs": "", // optional arguments you want to give to the compiler (e.g. --verbose or -O2)
}
```

Compilation progress is shown in the language server _terminal_ window,
while the _debug console_ shows the output of the program.

## Supported Language Server Aspects

- [x] Diagnostics (parse and type errors)
- [x] Code completion
- [x] Hover information
- [x] Find definitions
- [x] Inlay hints (shows inferred types)
- [x] Document outline
- [x] Code folding ranges
- [x] Code Lenses (`run debug` and `run optimized`)


# Syntax Highlighting

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

## Customize

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
