/**
 * Koka Editor — Embeddable code editor web component.
 *
 * Include this script on any page to enable <koka-editor> elements:
 *
 *   <script src="koka-editor.js"></script>
 *
 *   <koka-editor>
 *     fun main()
 *       println("Hello, Koka!")
 *   </koka-editor>
 *
 * Configuration (optional):
 *   <meta name="koka-wasm-url" content="https://cdn.example.com/koka-playground.wasm">
 *   <meta name="koka-stdlib-url" content="https://cdn.example.com/">
 *
 *   Or via JS:
 *   globalThis.kokaConfig = {
 *     wasmUrl: 'koka-playground.wasm',
 *     stdlibUrl: '',
 *   };
 */

// Register the <koka-editor> custom element
export { KokaEditorElement } from './koka-editor';
export { getSharedCompiler } from './shared-compiler';
