/**
 * koka-textmate.ts
 *
 * Loads the Koka TextMate grammar of the VS Code extension, shared by the
 * playground's Monaco tokenizer and the embeddable editor's highlighter.
 */

import { Registry, parseRawGrammar, type IGrammar } from 'vscode-textmate';
import { createOnigScanner, createOnigString, loadWASM } from 'vscode-oniguruma';
// Copied from support/vscode/koka.language-koka/syntaxes/koka.json before every
// dev and build run (`npm run grammar`), so it cannot drift from the extension's.
import kokaGrammarJson from './koka.tmLanguage.json';

let grammar: Promise<IGrammar> | null = null;

/** The Koka grammar, loaded once per page (oniguruma can be initialized only once). */
export function loadKokaGrammar(): Promise<IGrammar> {
  grammar ??= (async () => {
    const onigWasmUrl = new URL('vscode-oniguruma/release/onig.wasm', import.meta.url);
    const response = await fetch(onigWasmUrl.href);
    await loadWASM(await response.arrayBuffer());

    const registry = new Registry({
      onigLib: Promise.resolve({ createOnigScanner, createOnigString }),
      async loadGrammar(scopeName: string) {
        if (scopeName === 'source.koka') {
          return parseRawGrammar(JSON.stringify(kokaGrammarJson), 'koka.json');
        }
        return null;
      },
    });
    const g = await registry.loadGrammar('source.koka');
    if (!g) throw new Error('Failed to load Koka grammar');
    return g;
  })();
  return grammar;
}
