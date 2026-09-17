## VS Code installation

It is recommended to install the binary compiler via the VS Code Koka extension. See the [getting started guide](https://koka-lang.github.io/koka/doc/book.html) for more information.

## Command-line installation

### Linux (x64, arm64) and macOS (x64, arm64)

Tested on macOS, Ubuntu, Debian, and should run on most unix systems. From a command prompt, run:
```
curl -sSL https://github.com/koka-lang/koka/releases/download/v3.2.3/install.sh | sh
```

### Windows (x64, arm64)

Open a `cmd` prompt and download and run the installer:
```
curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/download/v3.2.3/install.bat && %tmp%\install-koka.bat
```
This will also prompt to install the [Clang][llvm] compiler, the [Windows SDK][winSDK] if needed, and syntax highlighting for the [VS Code][vscode] editor.
On Windows arm64, we use the x64 Koka compiler (which runs emulated), but the generated code is native arm64.


### Install Location

The default install location is `/usr/local/bin`
(or `%APPDATA%\local` on Windows). For more control, you can pass a different prefix. For example:

`curl ... | sh -s -- --prefix=~/.local`
(or 
`curl ... && %tmp%\install-koka.bat --prefix=c:\programs\local`)

Similarly, you can uninstall Koka by passing `--uninstall`.

### Other Platforms

You need to build [from source](https://github.com/koka-lang/koka#build-from-source), however Koka has few dependencies and should build from source without problems on most common platforms.

[winSDK]: https://visualstudio.microsoft.com/downloads/
[llvm]: https://github.com/llvm/llvm-project/releases/latest
[vscode]: https://code.visualstudio.com/
