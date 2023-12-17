This contains the sources for building the VS code extension (VSIX).

To build the extension first install nodejs/npm, then:

> cd support/vscode/koka.language-koka
> npm run build
> npm run package

and install the resulting `.vsix` extension either by right-clicking in VS code
and select `install extension`, or run:

> code --install-extension language-koka-<version>.vsix

In the extension you may need to go to the settings and set the path
to the Koka executable. If you like to use the build version use:

> stack path --local-install-root

to get the local install root directory where `<local-install-root>/bin/koka` is
the path to local executable.
