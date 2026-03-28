/**
 * flags.ts
 *
 * Shared compiler flags for both LSP and standalone WASM backends.
 * These match the WASI filesystem layout built by the workers.
 */

export const COMPILER_FLAGS = [
  '--sharedir=/share',
  '--libdir=/lib',
  '--target=js',
  '--builddir=/.koka',
  '--include=/share/lib',
  '--include=/',
  '--console=ansi',
] as const;
