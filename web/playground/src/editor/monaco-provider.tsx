/**
 * monaco-provider.tsx
 *
 * Handles the critical Monaco initialization sequencing:
 *   1. useWorkerFactory({}) before initServices()
 *   2. initServices({}) before any monaco import
 *   3. registerKokaLanguage() before editor creation
 *
 * Children only render once Monaco is ready.
 */

import { createContext, useContext, useEffect, useState, type ReactNode } from 'react';
import type * as MonacoTypes from '@codingame/monaco-vscode-editor-api';

type MonacoInstance = typeof MonacoTypes;

const MonacoContext = createContext<MonacoInstance | null>(null);

export function useMonaco(): MonacoInstance {
  const monaco = useContext(MonacoContext);
  if (!monaco) throw new Error('useMonaco() called before MonacoProvider is ready');
  return monaco;
}

export function MonacoProvider({ children }: { children: ReactNode }) {
  const [monaco, setMonaco] = useState<MonacoInstance | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    (async () => {
      try {
        // These must be imported and called in this exact order
        const { useWorkerFactory } = await import('monaco-languageclient/workerFactory');
        const { initServices } = await import('monaco-languageclient/vscode/services');
        useWorkerFactory({});
        await initServices({});

        // Now safe to import monaco
        const m = await import('@codingame/monaco-vscode-editor-api');

        // Register Koka language before any editor creation
        const { registerKokaLanguage } = await import('./koka-lang');
        await registerKokaLanguage(m);

        m.editor.setTheme('vs-dark');

        if (!cancelled) setMonaco(m);
      } catch (e) {
        if (!cancelled) setError(String(e));
      }
    })();
    return () => { cancelled = true; };
  }, []);

  if (error) return <div style={{ color: '#f48771', padding: 20 }}>Failed to load editor: {error}</div>;
  if (!monaco) return <div style={{ color: '#858585', padding: 20 }}>Loading editor...</div>;

  return <MonacoContext.Provider value={monaco}>{children}</MonacoContext.Provider>;
}
