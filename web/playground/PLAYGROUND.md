# Koka Playground

Browser-based Koka compiler and editor with full LSP support (hover, completion,
diagnostics, inlay hints, go-to-definition). The compiler and LSP server run as
WASM in Web Workers.

## Architecture

```
┌──────────────────────────────────────┐
│  React Frontend                      │
│  Monaco editor + LSP client          │
└────┬────────────────────┬────────────┘
     │ LSP (JSON-RPC)     │ Compile & Run
┌────▼──────────┐   ┌─────▼────────────┐
│ LSP Worker    │   │ Compiler Worker  │
│ koka-lsp.wasm │   │ koka-playground  │
│ SharedArrayBuf│   │ .wasm (fallback) │
└───────────────┘   └──────────────────┘
```

The LSP handles type-checking, hover, completion, diagnostics, AND compilation.
Falls back to the standalone WASM compiler if the LSP is unavailable.

## Quick Start

### Prerequisites

- **GHC WASM 9.12** — `FLAVOUR=9.12 sh <(curl -sL https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/raw/master/bootstrap.sh)`
- **GHC 9.12** (native) + **stack** + **hpack** — for precompiling stdlib
- **Node.js 18+** — for the web frontend

### Build & Run

```bash
cd web/playground && npm install
cd ../..
stack run koka -- -e util/playground    # builds WASM, precompiles stdlib, deploys assets
cd web/playground && npx vite --host
```

Individual targets: `stack run koka -- -e util/playground -- --backend=wasm|lsp|precompile|deploy|web`

## Project Structure

```
src/
  Platform/wasm/         WASM-specific platform modules
  Platform/cpp/          Native platform modules
  Common/File.hs         Delegates IO to Platform.FileIO
  Main/
    playground/Main.hs   Standalone WASM compiler entry point
    langserver/          LSP server (shared native + WASM)

web/playground/
  src/
    main.tsx             React entry point
    editor/              Monaco setup, state, compiler integration, VFS
    components/          React UI components
    workers/             Web Worker entry points (LSP + standalone WASM)
    embed/               Embeddable <koka-editor> web component
  test/
    test-wasm.mjs        WASM compiler smoke test

util/
  playground.kk          Koka build script (WASM, precompile, deploy)

.github/workflows/
  playground.yml         CI: build + test (no deployment)
```

## LSP Communication

The LSP uses `SharedArrayBuffer` + `Atomics.wait` for stdin (requires
Cross-Origin Isolation via `coi-serviceworker`). The WASM LSP's `fd_read`
retries with `threadDelay` when empty, yielding to the GHC runtime.

Diagnostics are remapped from `file://` URIs to `inmemory://` URIs to
match Monaco's model scheme.

## Known Limitations

- **SharedArrayBuffer** requires Cross-Origin Isolation headers (handled by `coi-serviceworker`)
- **`setFileTime`** is a no-op on `browser_wasi_shim` (silently ignored)
- **No C compiler** — `gcc not found` warning is harmless
- **FBIP samples** — `rbtree.kk` and `rbtree-fbip.kk` have BigInt issues on WASM
