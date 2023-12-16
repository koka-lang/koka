# Koka Syntax Highlighting and Language Server

Syntax highlighting and language server support for the
Koka programming language in Visual Studio Code.

Visit <https://koka-lang.github.io> for more information.

## Language Server

The language server continously analyses the code to show
parse- and type errors, complete identifiers,
show type information on hover, and
can execute `main`, `test-xxx`, and `example-xxx` functions
directly in the debug console.

### Customize

The extension shows "inlay hints" for inferred types of parameters
and local declarations. You can toggle inlay hints on- and
off in the editor inlay hints settings.
In the extension settings, you can also set the Koka compiler
path and specific compiler flags manually.


## Syntax Highlighting

### Token Classes

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

