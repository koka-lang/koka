function kk_exit(code) {
  if (globalThis.process && globalThis.process.exit) {
    process.exit(code) // nodejs
  } else {
    throw new Error(`Exited with code: ${code}`) // browser
  }
}