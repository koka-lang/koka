import { useEffect, useRef } from 'react';
import { usePlayground } from '../editor/context';
import { ResizeHandle } from './ResizeHandle';

export function CompilerLogPanel() {
  const { state, dispatch } = usePlayground();
  const logBodyRef = useRef<HTMLDivElement>(null);
  const logOutputRef = useRef<HTMLDivElement>(null);

  // Auto-scroll log to bottom on new entries
  useEffect(() => {
    if (logOutputRef.current) {
      logOutputRef.current.scrollTop = logOutputRef.current.scrollHeight;
    }
  }, [state.compilerLogHtml.length]);

  return (
    <div
      id="compiler-log-area"
      className={state.compilerLogCollapsed ? 'collapsed' : ''}
    >
      <ResizeHandle
        id="resize-handle-log"
        axis="y"
        targetRef={logBodyRef}
        min={40}
        max={500}
        invertDelta
        guard={() => !state.compilerLogCollapsed}
      />

      <div
        id="compiler-log-toggle"
        onClick={() => dispatch({ type: 'TOGGLE_COMPILER_LOG' })}
      >
        <span id="compiler-log-toggle-arrow">
          {state.compilerLogCollapsed ? '\u25B8' : '\u25BE'}
        </span>
        Compiler Log
        {state.progress && (
          <span className="compiler-log-progress">
            <span className="compiler-log-progress-msg">{state.progress.message ?? state.progress.title}</span>
            <span className="compiler-log-progress-pct">{state.progress.percentage != null ? `${state.progress.percentage}%` : ''}</span>
          </span>
        )}
      </div>

      <div id="compiler-log-body" ref={logBodyRef}>
        <div id="compiler-log-output" ref={logOutputRef}>
          {state.compilerLogHtml.map((html, i) => (
            <div key={i} dangerouslySetInnerHTML={{ __html: html }} />
          ))}
        </div>
      </div>
    </div>
  );
}
