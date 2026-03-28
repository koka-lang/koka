/**
 * koka-lang.ts
 *
 * Registers Koka language in Monaco using the real TextMate grammar
 * from the VSCode extension (support/vscode/koka.language-koka/syntaxes/koka.json).
 *
 * Uses vscode-textmate + vscode-oniguruma for proper tokenization.
 * Falls back to a basic Monarch tokenizer if TextMate loading fails.
 *
 * Also exports shared regex patterns and name encoding utilities
 * used across the playground and embed components.
 */

import type * as Monaco from '@codingame/monaco-vscode-editor-api';
import { Registry, parseRawGrammar, type IGrammar } from 'vscode-textmate';
import { createOnigScanner, createOnigString, loadWASM } from 'vscode-oniguruma';
// The grammar JSON is copied from support/vscode/koka.language-koka/syntaxes/koka.json
import kokaGrammarJson from './koka.tmLanguage.json';

export const KOKA_LANGUAGE_ID = 'koka';

let tmGrammar: IGrammar | null = null;

export async function registerKokaLanguage(monacoInstance: typeof Monaco): Promise<void> {
  if (monacoInstance.languages.getLanguages().find((l) => l.id === KOKA_LANGUAGE_ID)) return;

  monacoInstance.languages.register({
    id: KOKA_LANGUAGE_ID,
    extensions: ['.kk', '.kki'],
    aliases: ['Koka', 'koka'],
  });

  // Language configuration (brackets, comments, folding)
  monacoInstance.languages.setLanguageConfiguration(KOKA_LANGUAGE_ID, {
    comments: { lineComment: '//', blockComment: ['/*', '*/'] },
    brackets: [['{', '}'], ['[', ']'], ['(', ')']],
    autoClosingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '<', close: '>' },
      { open: '"', close: '"', notIn: ['string'] },
      { open: "'", close: "'", notIn: ['string'] },
      { open: '`', close: '`', notIn: ['string'] },
    ],
    surroundingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '<', close: '>' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
    folding: { offSide: true },
    wordPattern: /[A-Za-z_](?:[0-9A-Za-z_/](?:-[A-Za-z_])?)*'*/,
    indentationRules: {
      increaseIndentPattern: /^\s*(?:fun|if|then|else|elif|match|handler|handle|with|do)\b.*$/,
      decreaseIndentPattern: /^\s*[}\])].*$/,
    },
  });

  // Try to load TextMate grammar; fall back to Monarch if it fails
  try {
    await setupTextMateTokenizer(monacoInstance);
  } catch (e) {
    console.warn('TextMate grammar loading failed, using Monarch fallback:', e);
    monacoInstance.languages.setMonarchTokensProvider(KOKA_LANGUAGE_ID, monarchFallback);
  }

  registerCompletions(monacoInstance);
}

// ── TextMate grammar setup ───────────────────────────────────────────────────

async function setupTextMateTokenizer(monacoInstance: typeof Monaco): Promise<void> {
  // Load oniguruma WASM
  const onigWasmUrl = new URL(
    'vscode-oniguruma/release/onig.wasm',
    import.meta.url,
  );
  const response = await fetch(onigWasmUrl.href);
  const wasmBuf = await response.arrayBuffer();
  await loadWASM(wasmBuf);

  // Create the TextMate registry
  const registry = new Registry({
    onigLib: Promise.resolve({
      createOnigScanner,
      createOnigString,
    }),
    async loadGrammar(scopeName: string) {
      if (scopeName === 'source.koka') {
        return parseRawGrammar(JSON.stringify(kokaGrammarJson), 'koka.json');
      }
      return null;
    },
  });

  tmGrammar = await registry.loadGrammar('source.koka');
  if (!tmGrammar) throw new Error('Failed to load Koka grammar');

  // Wire up Monaco to use TextMate tokenization
  monacoInstance.languages.setTokensProvider(KOKA_LANGUAGE_ID, {
    getInitialState(): Monaco.languages.IState {
      return new TMState(null);
    },
    tokenize(line: string, state: Monaco.languages.IState): Monaco.languages.ILineTokens {
      const tmState = state as TMState;
      const result = tmGrammar!.tokenizeLine(line, tmState.ruleStack);
      const tokens: Monaco.languages.IToken[] = result.tokens.map((t) => ({
        startIndex: t.startIndex,
        scopes: tmScopesToMonacoScope(t.scopes),
      }));
      return {
        tokens,
        endState: new TMState(result.ruleStack),
      };
    },
  });
}

/** Map TextMate scope names to Monaco token types */
function tmScopesToMonacoScope(scopes: string[]): string {
  // TextMate scopes can have multiple space-separated names per scope level.
  // Check ALL scope names, preferring semantic scopes over punctuation.
  // First pass: look for semantic tokens across all scopes
  for (let i = scopes.length - 1; i >= 0; i--) {
    const parts = scopes[i].split(/\s+/);
    for (const scope of parts) {
      if (scope.startsWith('comment')) return 'comment';
      if (scope.startsWith('string')) return 'string';
      if (scope.startsWith('constant.numeric')) return 'number';
      if (scope.startsWith('constant.character')) return 'string.char';
      if (scope.startsWith('keyword.control')) return 'keyword.control';
      if (scope.startsWith('keyword.declaration') || scope.startsWith('keyword.other'))
        return 'keyword.declaration';
      if (scope.startsWith('keyword')) return 'keyword';
      if (scope.startsWith('storage.type')) return 'type';
      if (scope.startsWith('entity.name.function')) return 'entity.name.function';
      if (scope.startsWith('entity.name.type')) return 'type.identifier';
      if (scope.startsWith('entity.name.tag')) return 'type.identifier';
      if (scope.startsWith('entity.name')) return 'identifier';
      if (scope.startsWith('variable.parameter')) return 'variable.parameter';
      if (scope.startsWith('variable')) return 'variable';
      if (scope.startsWith('support.type')) return 'type.identifier';
      if (scope.startsWith('markup.italic')) return 'comment';
      if (scope.startsWith('meta.type')) return 'type';
    }
  }
  // Second pass: punctuation (lowest priority)
  for (let i = scopes.length - 1; i >= 0; i--) {
    if (scopes[i].includes('punctuation')) return 'delimiter';
  }
  return '';
}

/** Wrapper for TextMate rule stack as Monaco IState */
class TMState implements Monaco.languages.IState {
  constructor(public readonly ruleStack: import('vscode-textmate').StateStack | null) {}

  clone(): TMState {
    return new TMState(this.ruleStack);
  }

  equals(other: Monaco.languages.IState): boolean {
    if (!(other instanceof TMState)) return false;
    if (!this.ruleStack && !other.ruleStack) return true;
    if (!this.ruleStack || !other.ruleStack) return false;
    return this.ruleStack.equals(other.ruleStack);
  }
}

// ── Monarch fallback ─────────────────────────────────────────────────────────

const monarchFallback: Monaco.languages.IMonarchLanguage = {
  defaultToken: '',
  keywords: [
    'fun', 'fn', 'val', 'var', 'type', 'effect', 'alias', 'struct', 'con',
    'forall', 'exists', 'some', 'with', 'in', 'match', 'return',
    'if', 'then', 'else', 'elif',
    'handler', 'handle', 'ctl', 'final', 'raw', 'override',
    'pub', 'abstract', 'extern', 'module', 'import', 'as',
    'open', 'extend', 'linear', 'value', 'reference', 'named', 'scoped',
    'inline', 'noinline', 'tail', 'rec', 'co', 'lazy',
    'fip', 'fbip', 'inject', 'mask', 'behind', 'unsafe',
    'initially', 'finally', 'resume',
    'interface', 'instance', 'yield', 'break', 'continue',
  ],
  tokenizer: {
    root: [
      [/\s+/, 'white'],
      [/\/\/.*$/, 'comment'],
      [/\/\*/, 'comment', '@blockComment'],
      [/"/, 'string', '@string'],
      [/'[^'\\]'/, 'string'],
      [/'\\.'/, 'string'],
      [/0[xX][0-9a-fA-F_]+/, 'number.hex'],
      [/[0-9][0-9_]*\.[0-9][0-9_]*(?:[eE][+-]?[0-9]+)?/, 'number.float'],
      [/[0-9][0-9_]*/, 'number'],
      [/[A-Z][a-zA-Z0-9_-]*/, 'type.identifier'],
      [/[a-z_][a-zA-Z0-9_-]*'*/, {
        cases: { '@keywords': 'keyword', '@default': 'identifier' },
      }],
      [/[=<>!&|+\-*/%^~:]+/, 'operator'],
      [/[{}()\[\]]/, '@brackets'],
      [/[,;.]/, 'delimiter'],
    ],
    string: [
      [/[^\\"]+/, 'string'],
      [/\\./, 'string.escape'],
      [/"/, 'string', '@pop'],
    ],
    blockComment: [
      [/[^/*]+/, 'comment'],
      [/\/\*/, 'comment', '@push'],
      [/\*\//, 'comment', '@pop'],
      [/[/*]/, 'comment'],
    ],
  },
};

// ── Completions ──────────────────────────────────────────────────────────────

function registerCompletions(monacoInstance: typeof Monaco): void {
  monacoInstance.languages.registerCompletionItemProvider(KOKA_LANGUAGE_ID, {
    provideCompletionItems(model, position) {
      const word = model.getWordUntilPosition(position);
      const range: Monaco.IRange = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endColumn: word.endColumn,
      };

      const snippets: Monaco.languages.CompletionItem[] = [
        snippet('fun', 'fun ${1:name}(${2:args})\n  ${0:body}', 'Define a function', range, monacoInstance),
        snippet('match', 'match ${1:expr}\n  ${2:pattern} -> ${0:result}', 'Pattern match', range, monacoInstance),
        snippet('effect', 'effect ${1:name}\n  ctl ${2:op}(${3:args}) : ${0:result}', 'Algebraic effect', range, monacoInstance),
        snippet('handler', 'handler\n  ctl ${1:op}(${2:args})\n    ${0:body}', 'Effect handler', range, monacoInstance),
        snippet('if', 'if ${1:cond} then\n  ${2:body}\nelse\n  ${0:alt}', 'If-then-else', range, monacoInstance),
        snippet('type', 'type ${1:name}\n  ${0:cons}', 'Data type', range, monacoInstance),
        snippet('struct', 'struct ${1:name}\n  ${2:field} : ${0:type}', 'Struct', range, monacoInstance),
      ];

      const keywords = monarchFallback.keywords!.map((kw: string) => ({
        label: kw,
        kind: monacoInstance.languages.CompletionItemKind.Keyword,
        insertText: kw,
        range,
      }));

      return { suggestions: [...snippets, ...keywords] };
    },
  });
}

function snippet(
  label: string, text: string, doc: string,
  range: Monaco.IRange, m: typeof Monaco,
): Monaco.languages.CompletionItem {
  return {
    label, insertText: text, documentation: doc, range,
    kind: m.languages.CompletionItemKind.Snippet,
    insertTextRules: m.languages.CompletionItemInsertTextRule.InsertAsSnippet,
  };
}

// ── Shared regex patterns ────────────────────────────────────────────────────

/**
 * Matches runnable Koka entry points: fun main(), fun test/...(), fun example/...().
 * Same pattern as support/vscode/koka.language-koka/src/code-lens.ts.
 */
export const ENTRY_FUNCTION_RE = /(?:pub\s+)?fun\s+(main|test\/?[\w-]*|example\/?[\w-]*)\(\s*\)/;

/**
 * Like ENTRY_FUNCTION_RE but with the global flag and a start-of-line anchor
 * for scanning all entry points in a document (used for code lenses).
 */
export const ENTRY_FUNCTION_RE_GLOBAL = /(?:(?<=\n)|^)(?:pub\s+)?fun\s+(main|test\/?[\w-]*|example\/?[\w-]*)\(\s*\)/g;

/**
 * Matches a Koka module declaration to extract the module name.
 */
export const MODULE_NAME_RE = /^\s*module\s+([a-zA-Z][a-zA-Z0-9_/-]*)/m;

// ── Koka name encoding utilities ─────────────────────────────────────────────

/**
 * Encode a Koka module path to a flat filename.
 * Used to find the generated .mjs file for a given module name.
 *
 * Examples: "std/core" → "std_core", "garsia-wachs" → "garsia_dash_wachs"
 */
export function kokaModuleToFilename(moduleName: string): string {
  let result = '';
  for (let i = 0; i < moduleName.length; i++) {
    const c = moduleName[i];
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) {
      result += c;
    } else if (c === '/') {
      result += '_';
    } else if (c === '-') {
      result += '_dash_';
    } else if (c === '_') {
      result += '__';
    } else if (c === '.') {
      result += '_dot_';
    } else {
      result += c;
    }
  }
  return result;
}

/**
 * Encode a Koka identifier as it appears in JS exports (isModule=false encoding).
 * Used to find the entry function name in the generated module.
 *
 * Examples: "test/fib" → "test_fs_fib", "my-func" → "my_func"
 */
export function kokaIdentToJsExport(name: string): string {
  let result = '';
  for (let i = 0; i < name.length; i++) {
    const c = name[i];
    const next = name[i + 1] ?? ' ';
    if (/[a-zA-Z0-9]/.test(c)) {
      result += c;
    } else if (c === '/') {
      result += '_fs_';
    } else if (c === '-' && /[a-zA-Z0-9]/.test(next)) {
      result += '_';
    } else if (c === '-') {
      result += '_dash_';
    } else if (c === '_') {
      result += '__';
    } else if (c === '.') {
      result += '_dot_';
    } else {
      result += c;
    }
  }
  return result;
}
