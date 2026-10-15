/**
 * App.tsx
 *
 * Top-level layout for the Koka Playground.
 * Triggers stdlib preloading and LSP startup on mount.
 */

import { useEffect, useRef } from 'react';
import { usePlayground } from '../editor/context';
import { preloadStdlib, preloadSamplesDirectory, sendLspColors, startLsp } from '../editor/actions';
import { applyColorTheme } from '../editor/color-theme';
import { defineKokaTheme } from '../editor/koka-theme';
import { useMonaco } from '../editor/monaco-provider';
import { loadKokaSamples, EXCLUDED_SAMPLES, type FileEntry } from '../editor/samples';
import { Toolbar } from './Toolbar';
import { Workspace } from './Workspace';
import { CompilerLogPanel } from './CompilerLogPanel';

export function App() {
  const { state, dispatch, refs } = usePlayground();

  useEffect(() => {
    // Preload stdlib + samples into VFS, then start LSP
    void (async () => {
      await preloadStdlib(dispatch, refs);
      await preloadSamplesDirectory(dispatch, refs);
      void startLsp(dispatch, refs, state.inlayHints);
    })();

    // Load sample listing for the file browser
    void loadKokaSamples()
      .then((entries) => {
        const allEntry: FileEntry = {
          name: 'all.kk',
          path: 'all-web',
          type: 'file',
        };
        dispatch({
          type: 'UPDATE_SECTION',
          title: 'Samples',
          entries: [allEntry, ...entries.filter(e => !EXCLUDED_SAMPLES.has(e.path))],
        });
      })
      .catch(() => { /* manifest not available */ });
  }, [dispatch, refs]);

  // The theme in effect at load is applied before the first render; this
  // applies, and saves, the viewer's later choices.
  const monaco = useMonaco();
  const firstTheme = useRef(true);
  useEffect(() => {
    if (firstTheme.current) { firstTheme.current = false; return; }
    applyColorTheme(state.colorTheme, true);
    monaco.editor.setTheme(defineKokaTheme(monaco, state.colorTheme));
    void sendLspColors(refs, state.colorTheme);
  }, [state.colorTheme, monaco, refs]);

  return (
    <>
      <Toolbar />
      <Workspace />
      <CompilerLogPanel />
    </>
  );
}
