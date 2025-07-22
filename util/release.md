## VS Code installation

It is recommended to install the binary compiler via the VS Code Koka extension. See the <a href="https://koka-lang.github.io/koka/doc/book.html">getting started guide</a> for more information.


## Command Line Installation

### Linux (x64,arm64) and macOS (x64, arm64)

Tested on macOS, Ubuntu, Debian, and should run on most Linux distributions. From a command prompt, run:
```
curl -sSL https://github.com/koka-lang/koka/releases/download/v3.2.2/install.sh | sh
```
After install, run `koka` to verify if koka installed correctly.n

* For most installations this will ask for root access in order to install to `/usr/local/bin`. For more control, you can pass a different prefix. For example, installing to `~/.local` instead:  
  `curl -sSL https://github.com/koka-lang/koka/releases/download/v3.2.2/install.sh | sh -s -- --prefix=~/.local`

* To uninstall a version, use the `--uninstall` option:  
  `curl -sSL https://github.com/koka-lang/koka/releases/download/v3.2.2/install.sh | sh -s -- --uninstall`

### Windows (x64, arm64)

Open a `cmd` prompt and download and run the installer:
```
curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/download/v3.2.2/install.bat && %tmp%\install-koka.bat
```
This will also prompt to install the [`clang`][llvm] compiler and [Windows SDK][winSDK] if needed, and install syntax highlighting for the [VS Code][vscode] editor. After install, run `koka` to verify if koka installed correctly. 

* On Windows arm64, we use the x64 Koka compiler (which runs emulated), but the generated code is native arm64.

* On Windows, the default install is to the user profile at `%APPDATA%\local`. You can change the installation directory using `--prefix`. For example:  
  `curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/download/v3.2.2/install.bat && %tmp%\install-koka.bat --prefix=c:\programs\local`

* To uninstall a version, use the `--uninstall` option:  
  `curl -sSL -o %tmp%\install-koka.bat https://github.com/koka-lang/koka/releases/download/v3.2.2/install.bat && %tmp%\install-koka.bat --uninstall`

### Other platforms

You need to build [from source](https://github.com/koka-lang/koka#build-from-source), however Koka has few dependencies and should build from source without problems on most common platforms.

[winSDK]: https://visualstudio.microsoft.com/downloads/
[llvm]: https://github.com/llvm/llvm-project/releases/latest
[vscode]: https://code.visualstudio.com/