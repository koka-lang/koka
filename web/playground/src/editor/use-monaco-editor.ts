/**
 * use-monaco-editor.ts
 *
 * Hook that creates a Monaco editor instance attached to a container ref.
 * Disposes the editor on unmount.
 */

import { useEffect, useRef } from 'react';
import type * as MonacoTypes from '@codingame/monaco-vscode-editor-api';
import { useMonaco } from './monaco-provider';

export interface UseMonacoEditorOptions {
  options: MonacoTypes.editor.IStandaloneEditorConstructionOptions;
  onCreated?: (editor: MonacoTypes.editor.IStandaloneCodeEditor) => void;
}

export function useMonacoEditor({ options, onCreated }: UseMonacoEditorOptions) {
  const monaco = useMonaco();
  const containerRef = useRef<HTMLDivElement>(null);
  const editorRef = useRef<MonacoTypes.editor.IStandaloneCodeEditor | null>(null);

  useEffect(() => {
    if (!containerRef.current) return;

    const editor = monaco.editor.create(containerRef.current, {
      automaticLayout: true,
      ...options,
    });
    editorRef.current = editor;
    onCreated?.(editor);

    return () => {
      editor.dispose();
      editorRef.current = null;
    };
    // Only create once on mount — options changes are handled imperatively
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [monaco]);

  return { containerRef, editorRef };
}
