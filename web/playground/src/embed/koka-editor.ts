/**
 * koka-editor.ts
 *
 * A lightweight, embeddable Koka code editor web component.
 * Uses CodeMirror 6 for editing with Koka syntax highlighting.
 * Shares a single WASM compiler instance across all editors on the page.
 *
 * Usage:
 *   <koka-editor runnable>
 *     fun main()
 *       println("Hello!")
 *   </koka-editor>
 *
 *   <!-- Or with attribute -->
 *   <koka-editor code="fun main() println('hi')" runnable></koka-editor>
 *
 * Attributes:
 *   code       - Initial source code (alternative to text content)
 *   runnable   - Show "Run" button (auto-detected if fun main/test/example found)
 *   readonly   - Disable editing
 *   theme      - "dark" | "light" | "auto" (default: "auto")
 */

import { EditorState } from '@codemirror/state';
import { EditorView, keymap, lineNumbers, highlightActiveLine, drawSelection } from '@codemirror/view';
import { defaultKeymap, indentWithTab } from '@codemirror/commands';
import { syntaxHighlighting, HighlightStyle } from '@codemirror/language';
import { kokaLanguage, kokaHighlightStyle } from './koka-highlight';
import { getSharedCompiler, type SharedCompiler } from './shared-compiler';
import { ENTRY_FUNCTION_RE, MODULE_NAME_RE } from '../editor/koka-lang';

// ── Styles ──────────────────────────────────────────────────────────────────

const STYLES = `
:host {
  display: block;
  border: 1px solid var(--koka-border, #333);
  border-radius: 6px;
  overflow: hidden;
  font-family: 'Cascadia Code', 'Fira Code', 'Consolas', 'Courier New', monospace;
  font-size: 14px;
}

.editor-wrapper {
  position: relative;
}

.toolbar {
  display: flex;
  align-items: center;
  gap: 6px;
  padding: 4px 8px;
  background: var(--koka-toolbar-bg, #2d2d2d);
  border-bottom: 1px solid var(--koka-border, #333);
}

.toolbar button {
  padding: 3px 10px;
  border: 1px solid var(--koka-border, #555);
  border-radius: 4px;
  background: var(--koka-btn-bg, #3c3c3c);
  color: var(--koka-btn-color, #ccc);
  cursor: pointer;
  font-size: 12px;
  font-family: inherit;
}

.toolbar button:hover {
  background: var(--koka-btn-hover-bg, #505050);
}

.toolbar button:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.toolbar .spacer {
  flex: 1;
}

.toolbar .status {
  font-size: 11px;
  color: var(--koka-status-color, #888);
}

.output {
  border-top: 1px solid var(--koka-border, #333);
  background: var(--koka-output-bg, #1a1a1a);
  color: var(--koka-output-color, #ccc);
  padding: 8px 12px;
  font-size: 13px;
  white-space: pre-wrap;
  word-break: break-word;
  max-height: 200px;
  overflow-y: auto;
  display: none;
}

.output.visible {
  display: block;
}

.output .error {
  color: #f48771;
}

.output .info {
  color: #888;
}

/* Light theme overrides */
:host([theme="light"]) .toolbar,
:host(:not([theme="dark"])) .toolbar {
  background: var(--koka-toolbar-bg, inherit);
}

/* CodeMirror container */
.cm-container {
  min-height: 40px;
}

.cm-editor {
  max-height: 400px;
}

.cm-editor .cm-scroller {
  overflow: auto;
}
`;

// ── Web Component ───────────────────────────────────────────────────────────

export class KokaEditorElement extends HTMLElement {
  private editorView: EditorView | null = null;
  private outputEl: HTMLElement | null = null;
  private runBtn: HTMLButtonElement | null = null;
  private originalCode = '';
  private shadow: ShadowRoot;

  constructor() {
    super();
    this.shadow = this.attachShadow({ mode: 'open' });
  }

  static get observedAttributes() {
    return ['code', 'runnable', 'readonly', 'theme'];
  }

  connectedCallback() {
    // Get initial code from attribute or text content
    this.originalCode = this.getAttribute('code') || this.textContent?.trim() || '';
    // Clear text content (it's now in the editor)
    this.textContent = '';

    this.render();
  }

  disconnectedCallback() {
    this.editorView?.destroy();
  }

  private get isRunnable(): boolean {
    if (this.hasAttribute('runnable')) return true;
    return ENTRY_FUNCTION_RE.test(this.originalCode);
  }

  private get isReadonly(): boolean {
    return this.hasAttribute('readonly');
  }

  private get themeMode(): 'dark' | 'light' {
    const attr = this.getAttribute('theme');
    if (attr === 'dark') return 'dark';
    if (attr === 'light') return 'light';
    // Auto-detect from page
    return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
  }

  private render() {
    const isDark = this.themeMode === 'dark';

    // Build shadow DOM
    this.shadow.innerHTML = '';

    // Styles
    const style = document.createElement('style');
    style.textContent = STYLES;
    this.shadow.appendChild(style);

    const wrapper = document.createElement('div');
    wrapper.className = 'editor-wrapper';

    // Toolbar (only if runnable or editable)
    if (this.isRunnable || !this.isReadonly) {
      const toolbar = document.createElement('div');
      toolbar.className = 'toolbar';

      if (this.isRunnable) {
        this.runBtn = document.createElement('button');
        this.runBtn.textContent = '\u25B6 Run';
        this.runBtn.title = 'Compile and run';
        this.runBtn.addEventListener('click', () => this.run());
        toolbar.appendChild(this.runBtn);
      }

      const spacer = document.createElement('span');
      spacer.className = 'spacer';
      toolbar.appendChild(spacer);

      if (!this.isReadonly) {
        const resetBtn = document.createElement('button');
        resetBtn.textContent = '\u21BA Reset';
        resetBtn.title = 'Reset to original code';
        resetBtn.addEventListener('click', () => this.reset());
        toolbar.appendChild(resetBtn);
      }

      wrapper.appendChild(toolbar);
    }

    // CodeMirror container
    const cmContainer = document.createElement('div');
    cmContainer.className = 'cm-container';
    wrapper.appendChild(cmContainer);

    // Output area
    this.outputEl = document.createElement('div');
    this.outputEl.className = 'output';
    wrapper.appendChild(this.outputEl);

    this.shadow.appendChild(wrapper);

    // Create CodeMirror editor
    const highlight = HighlightStyle.define(kokaHighlightStyle);

    const darkTheme = EditorView.theme({
      '&': { backgroundColor: '#1e1e1e', color: '#d4d4d4' },
      '.cm-gutters': { backgroundColor: '#1e1e1e', color: '#858585', border: 'none' },
      '.cm-activeLineGutter': { backgroundColor: '#2a2a2a' },
      '.cm-activeLine': { backgroundColor: '#2a2d2e' },
      '&.cm-focused .cm-cursor': { borderLeftColor: '#aeafad' },
      '&.cm-focused .cm-selectionBackground, .cm-selectionBackground': { backgroundColor: '#264f78' },
    }, { dark: true });

    const lightTheme = EditorView.theme({
      '&': { backgroundColor: '#ffffff', color: '#1e1e1e' },
      '.cm-gutters': { backgroundColor: '#f5f5f5', color: '#999', border: 'none' },
      '.cm-activeLineGutter': { backgroundColor: '#e8e8e8' },
      '.cm-activeLine': { backgroundColor: '#f0f0f0' },
    });

    const extensions = [
      kokaLanguage,
      syntaxHighlighting(highlight),
      isDark ? darkTheme : lightTheme,
      lineNumbers(),
      highlightActiveLine(),
      drawSelection(),
      keymap.of([...defaultKeymap, indentWithTab]),
      EditorView.lineWrapping,
    ];

    if (this.isReadonly) {
      extensions.push(EditorState.readOnly.of(true));
    }

    this.editorView = new EditorView({
      state: EditorState.create({
        doc: this.originalCode,
        extensions,
      }),
      parent: cmContainer,
    });
  }

  /** Get current editor content */
  getCode(): string {
    return this.editorView?.state.doc.toString() ?? this.originalCode;
  }

  /** Reset editor to original code */
  reset() {
    if (this.editorView) {
      this.editorView.dispatch({
        changes: {
          from: 0,
          to: this.editorView.state.doc.length,
          insert: this.originalCode,
        },
      });
    }
    this.hideOutput();
  }

  /** Compile and run the current code */
  async run() {
    if (!this.runBtn || this.runBtn.disabled) return;

    this.runBtn.disabled = true;
    this.runBtn.textContent = '\u231B Running...';
    this.showOutput('Compiling...', 'info');

    try {
      const code = this.getCode();
      const moduleMatch = code.match(MODULE_NAME_RE);
      const moduleName = moduleMatch ? moduleMatch[1] : 'main';

      // Detect the entry point function
      const entryMatch = code.match(ENTRY_FUNCTION_RE);
      const entryName = entryMatch ? entryMatch[1] : 'main';

      const compiler = await getSharedCompiler();
      if (!compiler) {
        this.showOutput('WASM compiler not available.', 'error');
        return;
      }

      const extraArgs: string[] = [];
      if (entryName !== 'main') {
        extraArgs.push(`--main-entry=${entryName}`);
      }

      const result = await compiler.compile(moduleName, code, extraArgs);

      if (!result.success) {
        let errorMsg = 'Compilation failed.';
        try {
          const parsed = JSON.parse(result.stdout);
          errorMsg = (parsed.errors ?? []).join('\n') || errorMsg;
        } catch {
          errorMsg = result.stderr || result.stdout || errorMsg;
        }
        this.showOutput(errorMsg, 'error');
        return;
      }

      // Run the compiled output
      const output = await compiler.run(
        moduleName,
        result.generatedFiles,
        entryName !== 'main' ? entryName : undefined,
      );
      this.showOutput(output || '(no output)', 'stdout');

    } catch (err) {
      this.showOutput('Error: ' + (err instanceof Error ? err.message : String(err)), 'error');
    } finally {
      this.runBtn!.disabled = false;
      this.runBtn!.textContent = '\u25B6 Run';
    }
  }

  private showOutput(text: string, cls: 'stdout' | 'error' | 'info' = 'stdout') {
    if (!this.outputEl) return;
    this.outputEl.textContent = '';
    const span = document.createElement('span');
    span.className = cls;
    span.textContent = text;
    this.outputEl.appendChild(span);
    this.outputEl.classList.add('visible');
  }

  private hideOutput() {
    this.outputEl?.classList.remove('visible');
  }
}

// Register the custom element
if (!customElements.get('koka-editor')) {
  customElements.define('koka-editor', KokaEditorElement);
}
