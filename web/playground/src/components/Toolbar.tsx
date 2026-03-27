import { useState, useEffect, useRef } from 'react';
import { usePlayground, type BackendType } from '../editor/context';
import { compileAndRun, sendLspConfig } from '../editor/actions';

export function Toolbar() {
  const { state, dispatch, refs } = usePlayground();
  const [showSettings, setShowSettings] = useState(false);
  const settingsRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!showSettings) return;
    const handleClick = (e: MouseEvent) => {
      if (settingsRef.current && !settingsRef.current.contains(e.target as Node)) {
        setShowSettings(false);
      }
    };
    document.addEventListener('mousedown', handleClick);
    return () => document.removeEventListener('mousedown', handleClick);
  }, [showSettings]);

  const toggleInlayHint = (key: 'showImplicitArguments' | 'showInferredTypes' | 'showFullQualifiers') => {
    const updated = { ...state.inlayHints, [key]: !state.inlayHints[key] };
    dispatch({ type: 'SET_INLAY_HINTS', inlayHints: { [key]: !state.inlayHints[key] } });
    sendLspConfig(refs, updated);
  };

  return (
    <div id="toolbar">
      <button
        id="btn-filebrowser"
        title="Toggle file browser (Ctrl+B)"
        className={state.fileBrowserVisible ? 'active' : ''}
        onClick={() => dispatch({ type: 'TOGGLE_FILE_BROWSER' })}
      >
        &#x2630;
      </button>

      <span className="logo">Koka <span>Playground</span></span>

      <button
        id="btn-run"
        title="Compile and run the program (Ctrl+Enter)"
        disabled={state.status.kind === 'running' || state.status.kind === 'loading'}
        onClick={() => void compileAndRun(dispatch, refs, state.backend, state.verbosity)}
      >
        <span className="icon">&#x25B6;</span>
        Compile &amp; Run
      </button>

      <div id="status">
        <span id="status-dot" className={state.status.kind} />
        <span id="status-text">{state.status.text}</span>
      </div>

      <div className="spacer" />

      <div className="toolbar-settings-wrapper" ref={settingsRef}>
        <button
          className="toolbar-settings-btn"
          title="Settings"
          onClick={() => setShowSettings(!showSettings)}
        >
          &#x2699;
        </button>

        {showSettings && (
          <div className="toolbar-settings-panel" onClick={e => e.stopPropagation()}>
            <div className="settings-section">
              <div className="settings-title">Inlay Hints</div>
              <label className="settings-option">
                <input type="checkbox" checked={state.inlayHints.showImplicitArguments} onChange={() => toggleInlayHint('showImplicitArguments')} />
                Implicit arguments
              </label>
              <label className="settings-option">
                <input type="checkbox" checked={state.inlayHints.showInferredTypes} onChange={() => toggleInlayHint('showInferredTypes')} />
                Inferred types
              </label>
              <label className="settings-option">
                <input type="checkbox" checked={state.inlayHints.showFullQualifiers} onChange={() => toggleInlayHint('showFullQualifiers')} />
                Full qualifiers
              </label>
            </div>

            <div className="settings-section">
              <div className="settings-title">Compiler</div>
              <label className="settings-option">
                Backend:
                <select
                  value={state.backend}
                  onChange={e => dispatch({ type: 'SET_BACKEND', backend: e.target.value as BackendType })}
                >
                  <option value="lsp">LSP (WASM)</option>
                  <option value="wasm">Standalone WASM</option>
                </select>
              </label>
              <label className="settings-option">
                Verbose:
                <select
                  value={state.verbosity}
                  onChange={e => dispatch({ type: 'SET_VERBOSITY', verbosity: parseInt(e.target.value, 10) })}
                >
                  <option value="0">0 (quiet)</option>
                  <option value="1">1 (phases)</option>
                  <option value="2">2 (detail)</option>
                  <option value="3">3 (trace)</option>
                </select>
              </label>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
