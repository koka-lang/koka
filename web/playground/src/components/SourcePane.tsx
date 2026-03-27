import { useEffect } from 'react';
import { useMonaco } from '../editor/monaco-provider';
import { useMonacoEditor } from '../editor/use-monaco-editor';
import { usePlayground } from '../editor/context';
import { KOKA_LANGUAGE_ID, ENTRY_FUNCTION_RE_GLOBAL } from '../editor/koka-lang';
import { compileAndRun } from '../editor/actions';
import { DEFAULT_SOURCE } from '../editor/samples';

const EDITOR_OPTIONS = {
  fontSize: 14,
  fontFamily: "'Cascadia Code', 'Fira Code', 'Consolas', 'Courier New', monospace",
  fontLigatures: true,
  minimap: { enabled: false },
  scrollBeyondLastLine: false,
  renderLineHighlight: 'all' as const,
  lineNumbers: 'on' as const,
  glyphMargin: false,
  folding: true,
  fixedOverflowWidgets: true,
  value: DEFAULT_SOURCE,
  language: KOKA_LANGUAGE_ID,
  tabSize: 2,
  insertSpaces: true,
  wordWrap: 'off' as const,
};

export function SourcePane() {
  const monaco = useMonaco();
  const { state, dispatch, refs } = usePlayground();

  const { containerRef, editorRef } = useMonacoEditor({
    options: EDITOR_OPTIONS,
    onCreated: (editor) => {
      refs.current.sourceEditor = editor;

      // Open the default tab
      const uri = monaco.Uri.parse('inmemory://playground/main.kk');
      let model = monaco.editor.getModel(uri);
      if (!model) {
        model = monaco.editor.createModel(DEFAULT_SOURCE, KOKA_LANGUAGE_ID, uri);
      }
      editor.setModel(model);
      const id = 'tab-' + Math.random().toString(36).slice(2, 9);
      dispatch({
        type: 'OPEN_TAB',
        tab: { id, name: 'main.kk', path: 'main.kk', modelUri: uri.toString() },
      });
    },
  });

  // Switch model when active tab changes
  useEffect(() => {
    const editor = editorRef.current;
    if (!editor || !state.activeTabId) return;
    const tab = state.tabs.find(t => t.id === state.activeTabId);
    if (!tab) return;
    const model = monaco.editor.getModel(monaco.Uri.parse(tab.modelUri));
    if (model && editor.getModel() !== model) {
      editor.setModel(model);
    }
  }, [state.activeTabId, state.tabs, monaco, editorRef]);

  // Register code lens provider once
  useEffect(() => {
    const disposable = monaco.languages.registerCodeLensProvider(KOKA_LANGUAGE_ID, {
      provideCodeLenses(model) {
        const text = model.getValue();
        const re = new RegExp(ENTRY_FUNCTION_RE_GLOBAL.source, ENTRY_FUNCTION_RE_GLOBAL.flags);
        const lenses: ReturnType<typeof monaco.languages.registerCodeLensProvider> extends { provideCodeLenses: (m: any) => infer R } ? never : any[] = [];
        let match: RegExpExecArray | null;
        while ((match = re.exec(text)) !== null) {
          const pos = model.getPositionAt(match.index);
          lenses.push({
            range: {
              startLineNumber: pos.lineNumber,
              startColumn: pos.column,
              endLineNumber: pos.lineNumber,
              endColumn: pos.column + match[0].length,
            },
            command: {
              id: 'koka.compileAndRun',
              title: match[1] === 'main' ? '\u25B6 Run' : `\u25B6 Run ${match[1]}`,
              tooltip: 'Compile and run this function',
              arguments: [match[1]],
            },
          });
        }
        return { lenses, dispose() {} };
      },
    });
    return () => disposable.dispose();
  }, [monaco]);

  // Register vscode command for code lenses + keybindings
  useEffect(() => {
    const editor = editorRef.current;
    if (!editor) return;

    // Register the command that code lenses invoke
    let commandDisposable: { dispose(): void } | null = null;
    (async () => {
      const vscode = await import('vscode');
      commandDisposable = vscode.commands.registerCommand('koka.compileAndRun', (entryFunction?: string) => {
        void compileAndRun(dispatch, refs, state.backend, state.verbosity, entryFunction);
      });
    })();

    // Ctrl+Enter to compile & run
    editor.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
      () => { void compileAndRun(dispatch, refs, state.backend, state.verbosity); },
    );

    // Ctrl+B to toggle file browser
    editor.addCommand(
      monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyB,
      () => dispatch({ type: 'TOGGLE_FILE_BROWSER' }),
    );

    return () => { commandDisposable?.dispose(); };
  }, [monaco, editorRef, dispatch, refs, state.backend, state.verbosity]);

  return <div className="editor-container" id="editor-source" ref={containerRef} />;
}
