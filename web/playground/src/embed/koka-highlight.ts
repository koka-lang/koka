/**
 * koka-highlight.ts
 *
 * Koka syntax highlighting for CodeMirror 6 with the same TextMate grammar and
 * token colors as the playground, so an embedded example is colored as in
 * VS Code. The code shows uncolored until the grammar has loaded.
 */

import { RangeSetBuilder, StateEffect } from '@codemirror/state';
import { Decoration, ViewPlugin, type DecorationSet, type EditorView, type ViewUpdate } from '@codemirror/view';
import { INITIAL, type IGrammar, type StateStack } from 'vscode-textmate';
import { loadKokaGrammar } from '../editor/koka-textmate';
import { themedScope, tokenStyle, type ThemeKind } from '../editor/koka-theme';

const grammarLoaded = StateEffect.define<null>();

export function kokaHighlighting(kind: ThemeKind) {
  const marks = new Map<string, Decoration | null>();
  const markFor = (scope: string): Decoration | null => {
    if (!marks.has(scope)) {
      const st = tokenStyle(scope, kind);
      const css = (st?.foreground ? `color: ${st.foreground};` : '') +
                  (st?.fontStyle ? `font-style: ${st.fontStyle};` : '');
      marks.set(scope, css ? Decoration.mark({ attributes: { style: css } }) : null);
    }
    return marks.get(scope)!;
  };

  return ViewPlugin.fromClass(class {
    decorations: DecorationSet = Decoration.none;
    grammar: IGrammar | null = null;
    destroyed = false;

    constructor(view: EditorView) {
      loadKokaGrammar().then((g) => {
        if (this.destroyed) return;
        this.grammar = g;
        view.dispatch({ effects: grammarLoaded.of(null) });
      }).catch((err) => console.warn('[koka-editor] highlighting unavailable:', err));
    }

    update(u: ViewUpdate) {
      if (u.docChanged || u.transactions.some(tr => tr.effects.some(e => e.is(grammarLoaded)))) {
        this.decorations = this.build(u.view);
      }
    }

    destroy() {
      this.destroyed = true;
    }

    // Retokenizes the whole document: embedded examples are a few dozen lines,
    // and a line's tokens depend on the state at the end of the line before.
    build(view: EditorView): DecorationSet {
      if (!this.grammar) return Decoration.none;
      const builder = new RangeSetBuilder<Decoration>();
      const doc = view.state.doc;
      let state: StateStack = INITIAL;
      for (let i = 1; i <= doc.lines; i++) {
        const line = doc.line(i);
        const result = this.grammar.tokenizeLine(line.text, state);
        state = result.ruleStack;
        for (const t of result.tokens) {
          const end = Math.min(t.endIndex, line.length);
          if (end <= t.startIndex) continue;
          const mark = markFor(themedScope(t.scopes));
          if (mark) builder.add(line.from + t.startIndex, line.from + end, mark);
        }
      }
      return builder.finish();
    }
  }, { decorations: v => v.decorations });
}
