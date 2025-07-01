# The VS Code Extension

This contains the sources for building the VS code extension (VSIX).

To build the extension first install nodejs/npm, then:

```
$ cd support/vscode/koka.language-koka
$ npm install
$ npm run build
```

and install the resulting `.vsix` extension either by right-clicking in VS code
and select `install extension`, or run:

```
$ code --install-extension language-koka-<version>.vsix
```

## Publishing

When publishing a new version of the extension to the markedplace, increase the version
number in `package.json` and run `npm run publish`.


## Debugging

When developing the extension, you can add the following `.vscode/launch.json` file:
```json
{
  "version": "0.2.0",
  "configurations": [
    { "args": [ "--extensionDevelopmentPath=${workspaceFolder}/support/vscode/koka.language-koka" ],
      "name": "Launch Extension",
      "outFiles": [ "${workspaceFolder}/support/vscode/koka.language-koka/dist/**/*.js" ],
      "request": "launch",
      "type": "extensionHost",
      "sourceMaps": true
    }
  ]
}
```
(note that when using `npm run tscbuild` you need to use the `out` directory instead of the `dist` directory).

You can now press `F5` to lauch a vs code editor with the extension enabled and you should be able
to set breakpoints etc. to debug the extension. 

Run `npm run watch` in a terminal to continuously update the extension when making changes.

