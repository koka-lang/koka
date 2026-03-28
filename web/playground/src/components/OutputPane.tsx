import { useEffect, useRef } from 'react';
import { usePlayground } from '../editor/context';
import { useMonacoEditor } from '../editor/use-monaco-editor';
import { ResizeHandle } from './ResizeHandle';

const JS_EDITOR_OPTIONS = {
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
  value: '',
  language: 'javascript',
  readOnly: true,
  wordWrap: 'off' as const,
};

function JsPane() {
  const { refs } = usePlayground();

  const { containerRef } = useMonacoEditor({
    options: JS_EDITOR_OPTIONS,
    onCreated: (editor) => {
      refs.current.jsEditor = editor;
    },
  });

  return <div className="editor-container" id="editor-js" ref={containerRef} />;
}

function ConsolePane() {
  const { state } = usePlayground();
  const containerRef = useRef<HTMLDivElement>(null);

  // Auto-scroll to bottom on new output
  useEffect(() => {
    if (containerRef.current) {
      containerRef.current.scrollTop = containerRef.current.scrollHeight;
    }
  }, [state.consoleLines.length]);

  return (
    <div id="console-output" ref={containerRef}>
      {state.consoleLines.map((line, i) => (
        <span key={i} className={`console-line ${line.cls}`}>{line.text}</span>
      ))}
    </div>
  );
}

export function OutputPane() {
  const { state, dispatch } = usePlayground();
  const consolePaneRef = useRef<HTMLDivElement>(null);
  const outputPaneRef = useRef<HTMLDivElement>(null);

  return (
    <div id="pane-output" ref={outputPaneRef}>
      {state.jsOutputVisible && (
        <>
          <div id="pane-js">
            <div className="pane-header">
              Compiled JavaScript
              <span className="pane-header-close" title="Hide JavaScript pane" onClick={() => dispatch({ type: 'SET_JS_OUTPUT_VISIBLE', visible: false })}>&times;</span>
            </div>
            <JsPane />
          </div>

          <ResizeHandle
            id="resize-handle-v"
            axis="y"
            targetRef={consolePaneRef}
            min={60}
            max={() => (outputPaneRef.current?.getBoundingClientRect().height ?? 400) - 80}
            invertDelta
          />
        </>
      )}

      <div id="pane-console" ref={consolePaneRef} className={state.jsOutputVisible ? '' : 'full-height'}>
        <div className="pane-header">
          Execution Output
          {!state.jsOutputVisible && (
            <span className="pane-header-action" title="Show compiled JavaScript" onClick={() => dispatch({ type: 'SET_JS_OUTPUT_VISIBLE', visible: true })}>JS</span>
          )}
        </div>
        <ConsolePane />
      </div>
    </div>
  );
}
