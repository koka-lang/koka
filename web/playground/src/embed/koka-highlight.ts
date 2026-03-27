/**
 * koka-highlight.ts
 *
 * Lightweight Koka syntax highlighting for CodeMirror 6.
 * Uses StreamLanguage for simple regex-based tokenization.
 */

import { StreamLanguage, type StreamParser } from '@codemirror/language';
import { tags as t } from '@lezer/highlight';

const kokaParser: StreamParser<{ inString: boolean; inBlockComment: number }> = {
  startState() {
    return { inString: false, inBlockComment: 0 };
  },

  token(stream, state) {
    // Block comment
    if (state.inBlockComment > 0) {
      if (stream.match('/*')) {
        state.inBlockComment++;
        return 'blockComment';
      }
      if (stream.match('*/')) {
        state.inBlockComment--;
        return 'blockComment';
      }
      stream.next();
      return 'blockComment';
    }

    // Start of block comment
    if (stream.match('/*')) {
      state.inBlockComment = 1;
      return 'blockComment';
    }

    // Line comment
    if (stream.match('//')) {
      stream.skipToEnd();
      return 'lineComment';
    }

    // String
    if (stream.match('"')) {
      while (!stream.eol()) {
        const ch = stream.next();
        if (ch === '\\') stream.next(); // skip escaped char
        else if (ch === '"') break;
      }
      return 'string';
    }

    // Character literal
    if (stream.match("'")) {
      while (!stream.eol()) {
        const ch = stream.next();
        if (ch === '\\') stream.next();
        else if (ch === "'") break;
      }
      return 'string';
    }

    // Numbers
    if (stream.match(/^0[xX][0-9a-fA-F][0-9a-fA-F_]*/)) return 'number';
    if (stream.match(/^[0-9][0-9_]*(\.[0-9][0-9_]*)?([eE][-+]?[0-9]+)?/)) return 'number';

    // Keywords
    if (stream.match(/^(fun|val|var|type|effect|struct|con|alias|module|import|pub|abstract|extern|inline|noinline|open|extend|co|rec|value|reference|linear|named|scoped|initially|finally|raw|ctl|final|return|match|with|handle|handler|mask|override|if|then|else|elif|fn|forall|some|exists|interface|instance|behind|in)\b/)) {
      return 'keyword';
    }

    // Type keywords
    if (stream.match(/^(int|string|bool|char|float64|float32|list|maybe|either|order|void|unit|io|div|exn|ndet|alloc|read|write|net|fsys|ui|console|total|pure|ssize_t)\b/)) {
      return 'typeName';
    }

    // Boolean/special values
    if (stream.match(/^(True|False|Nothing|Just|Left|Right|Ok|Error)\b/)) {
      return 'atom';
    }

    // Operators
    if (stream.match(/^[+\-*/%=<>!&|^~?:.$@#]+/)) {
      return 'operator';
    }

    // Type names (capitalized)
    if (stream.match(/^[A-Z][a-zA-Z0-9_]*/)) {
      return 'typeName';
    }

    // Identifiers
    if (stream.match(/^[a-z_][a-zA-Z0-9_-]*/)) {
      return 'variableName';
    }

    // Brackets
    if (stream.match(/^[{}()\[\]]/)) {
      return 'bracket';
    }

    // Skip unknown
    stream.next();
    return null;
  },
};

export const kokaLanguage = StreamLanguage.define(kokaParser);

/**
 * CodeMirror highlighting style tags for Koka tokens.
 */
export const kokaHighlightStyle = [
  { tag: t.keyword, color: '#c586c0' },
  { tag: t.typeName, color: '#4ec9b0' },
  { tag: t.variableName, color: '#9cdcfe' },
  { tag: t.string, color: '#ce9178' },
  { tag: t.number, color: '#b5cea8' },
  { tag: t.operator, color: '#d4d4d4' },
  { tag: t.lineComment, color: '#6a9955' },
  { tag: t.blockComment, color: '#6a9955' },
  { tag: t.atom, color: '#569cd6' },
  { tag: t.bracket, color: '#d4d4d4' },
];
