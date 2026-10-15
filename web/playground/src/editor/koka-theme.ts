/**
 * koka-theme.ts
 *
 * Token colors for Koka code, shared by the playground (Monaco) and the
 * embeddable editor (CodeMirror), so code looks as it does in VS Code with the
 * Koka extension under VS Code's default themes.
 */

import type * as Monaco from '@codingame/monaco-vscode-editor-api';

export type ThemeKind = 'dark' | 'light';

export interface TokenStyle {
  foreground?: string;
  fontStyle?: string;
}

// The style VS Code's default themes ("Dark 2026", "Light 2026", VS Code
// 1.138) give each scope the Koka grammar emits: following the theme's
// `include` chain, the single-scope rule whose selector is the longest dotted
// prefix of the scope, the later rule on a tie. Scopes no rule colors are
// absent.
const DARK_2026: Record<string, TokenStyle> = {
  'comment.block': { foreground: '#8b949e' },
  'comment.line.double-slash': { foreground: '#8b949e' },
  'constant.character.escape': { foreground: '#d7ba7d' },
  'constant.numeric': { foreground: '#b5cea8' },
  'constant.numeric.type': { foreground: '#b5cea8' },
  'entity.name': { foreground: '#ffa657' },
  'entity.name.function': { foreground: '#d2a8ff' },
  'entity.name.label': { foreground: '#c8c8c8' },
  'entity.name.tag.css.constant.other.constructor': { foreground: '#d7ba7d' },
  'entity.name.variable': { foreground: '#9cdcfe' },
  'invalid.keyword': { foreground: '#f44747' },
  'invalid.string.quoted.double': { foreground: '#f44747' },
  'invalid.string.quoted.single': { foreground: '#f44747' },
  'keyword': { foreground: '#ff7b72' },
  'keyword.control': { foreground: '#c586c0' },
  'keyword.declaration': { foreground: '#ff7b72' },
  'keyword.declaration.function': { foreground: '#ff7b72' },
  'keyword.declaration.import': { foreground: '#ff7b72' },
  'keyword.declaration.type': { foreground: '#ff7b72' },
  'keyword.other': { foreground: '#ff7b72' },
  'markup.italic': { foreground: '#c9d1d9', fontStyle: 'italic' },
  'meta.preprocessor': { foreground: '#569cd6' },
  'punctuation.definition.comment': { foreground: '#8b949e' },
  'string.quoted.double': { foreground: '#a5d6ff' },
  'string.quoted.double.regular': { foreground: '#a5d6ff' },
  'string.quoted.single': { foreground: '#a5d6ff' },
  'support.type': { foreground: '#4ec9b0' },
};

const LIGHT_2026: Record<string, TokenStyle> = {
  'comment.block': { foreground: '#6e7781' },
  'comment.line.double-slash': { foreground: '#6e7781' },
  'constant.character.escape': { foreground: '#ee0000' },
  'constant.numeric': { foreground: '#098658' },
  'constant.numeric.type': { foreground: '#098658' },
  'entity.name': { foreground: '#953800' },
  'entity.name.function': { foreground: '#8250df' },
  'entity.name.label': { foreground: '#000000' },
  'entity.name.tag.css.constant.other.constructor': { foreground: '#116329' },
  'entity.name.variable': { foreground: '#001080' },
  'invalid.keyword': { foreground: '#cd3131' },
  'invalid.string.quoted.double': { foreground: '#cd3131' },
  'invalid.string.quoted.single': { foreground: '#cd3131' },
  'keyword': { foreground: '#cf222e' },
  'keyword.control': { foreground: '#af00db' },
  'keyword.declaration': { foreground: '#cf222e' },
  'keyword.declaration.function': { foreground: '#cf222e' },
  'keyword.declaration.import': { foreground: '#cf222e' },
  'keyword.declaration.type': { foreground: '#cf222e' },
  'keyword.other': { foreground: '#cf222e' },
  'markup.italic': { foreground: '#1f2328', fontStyle: 'italic' },
  'meta.preprocessor': { foreground: '#0000ff' },
  'punctuation.definition.comment': { foreground: '#6e7781' },
  'string.quoted.double': { foreground: '#0a3069' },
  'string.quoted.double.regular': { foreground: '#0a3069' },
  'string.quoted.single': { foreground: '#0a3069' },
  'support.type': { foreground: '#267f99' },
};

// The Koka extension's `editor.tokenColorCustomizations`
// (support/vscode/koka.language-koka/package.json), which apply under every
// theme. They match by dotted prefix: `koka.comment.doc.pre` also colors
// `koka.comment.doc.pre.type`.
const KOKA_EXTENSION: Record<string, TokenStyle> = {
  'koka.type.typeparam': { foreground: '#aaaaaa' },
  'koka.param.implicit': { foreground: '#aaaaaa' },
  'koka.comment.doc.pre': { foreground: '#84a084' },
};

export const EDITOR_COLORS: Record<ThemeKind, { background: string; foreground: string }> = {
  dark: { background: '#121314', foreground: '#bbbebf' },
  light: { background: '#ffffff', foreground: '#202020' },
};

const STYLES: Record<ThemeKind, Record<string, TokenStyle>> = {
  dark: { ...DARK_2026, ...KOKA_EXTENSION },
  light: { ...LIGHT_2026, ...KOKA_EXTENSION },
};

/** The longest dotted prefix of `scope` that has a style under `kind`. */
function styleKey(scope: string, kind: ThemeKind): string | undefined {
  const styles = STYLES[kind];
  for (let s = scope; ; s = s.slice(0, s.lastIndexOf('.'))) {
    if (s in styles) return s;
    if (!s.includes('.')) return undefined;
  }
}

/**
 * The scope that colors a token whose TextMate scope stack is `scopes`: the
 * deepest scope name that has a style, which is the scope VS Code takes the
 * foreground from. A scope level may hold several space-separated names, the
 * last one deepest. Both themes style the same scopes, so the answer does not
 * depend on the theme.
 */
export function themedScope(scopes: string[]): string {
  for (let i = scopes.length - 1; i >= 0; i--) {
    const names = scopes[i].split(/\s+/);
    for (let j = names.length - 1; j >= 0; j--) {
      if (styleKey(names[j], 'dark') !== undefined) return names[j];
    }
  }
  return '';
}

export function tokenStyle(scope: string, kind: ThemeKind): TokenStyle | undefined {
  const key = styleKey(scope, kind);
  return key === undefined ? undefined : STYLES[kind][key];
}

/** Defines the Monaco theme for `kind` and returns its name. */
export function defineKokaTheme(monaco: typeof Monaco, kind: ThemeKind): string {
  const name = 'koka-' + kind;
  monaco.editor.defineTheme(name, {
    base: kind === 'dark' ? 'vs-dark' : 'vs',
    inherit: true,
    rules: Object.entries(STYLES[kind]).map(([token, st]) => ({
      token,
      foreground: st.foreground?.slice(1),
      fontStyle: st.fontStyle,
    })),
    colors: {
      'editor.background': EDITOR_COLORS[kind].background,
      'editor.foreground': EDITOR_COLORS[kind].foreground,
    },
  });
  return name;
}
