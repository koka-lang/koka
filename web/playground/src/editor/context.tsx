/**
 * context.tsx
 *
 * Central state management for the Koka playground.
 * Uses useReducer for state that drives re-renders, and refs for
 * imperative handles (VFS, LSP, editors) that don't.
 */

import {
  createContext, useContext, useReducer, useRef, useCallback,
  type ReactNode, type Dispatch, type MutableRefObject,
} from 'react';
import type * as MonacoTypes from '@codingame/monaco-vscode-editor-api';
import type { KokaVFS } from './vfs';
import type { LspClientHandle } from './lsp-client';
import type { WasmCompileResult } from './wasm-runner';
import type { FileEntry } from './samples';

// ── State ──────────────────────────────────────────────────────────────────

export type StatusKind = 'loading' | 'ready' | 'error' | 'running';
export type BackendType = 'lsp' | 'wasm';
export type ConsoleCls = 'stdout' | 'stderr' | 'info' | 'separator';

export interface ConsoleLine {
  text: string;
  cls: ConsoleCls;
}

export interface TabEntry {
  id: string;
  name: string;
  path: string;
  modelUri: string; // stored as string, looked up via monaco.editor.getModel(Uri.parse(...))
}

export interface FileBrowserSection {
  title: string;
  entries: FileEntry[];
  collapsed: boolean;
}

export interface ProgressInfo {
  title: string;
  message?: string;
  percentage?: number;
}

export interface InlayHintSettings {
  showImplicitArguments: boolean;
  showInferredTypes: boolean;
  showFullQualifiers: boolean;
}

export interface PlaygroundState {
  status: { kind: StatusKind; text: string };
  progress: ProgressInfo | null;
  backend: BackendType;
  verbosity: number;
  inlayHints: InlayHintSettings;
  tabs: TabEntry[];
  activeTabId: string | null;
  fileBrowserVisible: boolean;
  compilerLogCollapsed: boolean;
  jsOutputVisible: boolean;
  consoleLines: ConsoleLine[];
  compilerLogHtml: string[];
  fileBrowserSections: FileBrowserSection[];
}

const initialState: PlaygroundState = {
  status: { kind: 'loading', text: 'Loading editor...' },
  progress: null,
  backend: 'lsp',
  verbosity: 1,
  inlayHints: { showImplicitArguments: true, showInferredTypes: true, showFullQualifiers: false },
  tabs: [],
  activeTabId: null,
  fileBrowserVisible: true,
  compilerLogCollapsed: false,
  jsOutputVisible: false,
  consoleLines: [],
  compilerLogHtml: [],
  fileBrowserSections: [
    { title: 'Samples', entries: [], collapsed: false },
    { title: 'Output', entries: [], collapsed: false },
  ],
};

// ── Actions ────────────────────────────────────────────────────────────────

export type PlaygroundAction =
  | { type: 'SET_STATUS'; kind: StatusKind; text: string }
  | { type: 'SET_PROGRESS'; progress: ProgressInfo | null }
  | { type: 'SET_BACKEND'; backend: BackendType }
  | { type: 'SET_VERBOSITY'; verbosity: number }
  | { type: 'SET_INLAY_HINTS'; inlayHints: Partial<InlayHintSettings> }
  | { type: 'OPEN_TAB'; tab: TabEntry }
  | { type: 'CLOSE_TAB'; id: string }
  | { type: 'SWITCH_TAB'; id: string }
  | { type: 'TOGGLE_FILE_BROWSER' }
  | { type: 'SET_FILE_BROWSER_VISIBLE'; visible: boolean }
  | { type: 'TOGGLE_COMPILER_LOG' }
  | { type: 'SET_JS_OUTPUT_VISIBLE'; visible: boolean }
  | { type: 'APPEND_CONSOLE'; text: string; cls: ConsoleCls }
  | { type: 'CLEAR_CONSOLE' }
  | { type: 'APPEND_COMPILER_LOG'; html: string }
  | { type: 'CLEAR_COMPILER_LOG' }
  | { type: 'UPDATE_SECTION'; title: string; entries: FileEntry[] }
  | { type: 'TOGGLE_SECTION'; title: string };

function reducer(state: PlaygroundState, action: PlaygroundAction): PlaygroundState {
  switch (action.type) {
    case 'SET_STATUS':
      return { ...state, status: { kind: action.kind, text: action.text } };
    case 'SET_PROGRESS':
      return { ...state, progress: action.progress };
    case 'SET_BACKEND':
      return { ...state, backend: action.backend };
    case 'SET_VERBOSITY':
      return { ...state, verbosity: action.verbosity };
    case 'SET_INLAY_HINTS':
      return { ...state, inlayHints: { ...state.inlayHints, ...action.inlayHints } };
    case 'OPEN_TAB': {
      // If already open by path, just switch to it
      const existing = state.tabs.find(t => t.path === action.tab.path);
      if (existing) return { ...state, activeTabId: existing.id };
      return { ...state, tabs: [...state.tabs, action.tab], activeTabId: action.tab.id };
    }
    case 'CLOSE_TAB': {
      const tabs = state.tabs.filter(t => t.id !== action.id);
      let activeTabId = state.activeTabId;
      if (activeTabId === action.id) {
        activeTabId = tabs.length > 0 ? tabs[tabs.length - 1].id : null;
      }
      return { ...state, tabs, activeTabId };
    }
    case 'SWITCH_TAB':
      return { ...state, activeTabId: action.id };
    case 'TOGGLE_FILE_BROWSER':
      return { ...state, fileBrowserVisible: !state.fileBrowserVisible };
    case 'SET_FILE_BROWSER_VISIBLE':
      return { ...state, fileBrowserVisible: action.visible };
    case 'TOGGLE_COMPILER_LOG':
      return { ...state, compilerLogCollapsed: !state.compilerLogCollapsed };
    case 'SET_JS_OUTPUT_VISIBLE':
      return { ...state, jsOutputVisible: action.visible };
    case 'APPEND_CONSOLE':
      return { ...state, consoleLines: [...state.consoleLines, { text: action.text, cls: action.cls }] };
    case 'CLEAR_CONSOLE':
      return { ...state, consoleLines: [] };
    case 'APPEND_COMPILER_LOG':
      return {
        ...state,
        compilerLogHtml: [...state.compilerLogHtml, action.html],
        compilerLogCollapsed: false, // auto-expand on output
      };
    case 'CLEAR_COMPILER_LOG':
      return { ...state, compilerLogHtml: [] };
    case 'UPDATE_SECTION':
      return {
        ...state,
        fileBrowserSections: state.fileBrowserSections.map(s =>
          s.title === action.title ? { ...s, entries: action.entries } : s
        ),
      };
    case 'TOGGLE_SECTION':
      return {
        ...state,
        fileBrowserSections: state.fileBrowserSections.map(s =>
          s.title === action.title ? { ...s, collapsed: !s.collapsed } : s
        ),
      };
    default:
      return state;
  }
}

// ── Imperative refs (not in state) ────────────────────────────────────────

export type WasmCompileFn = (moduleName: string, sourceText: string, extraArgs?: string[]) => Promise<WasmCompileResult>;

export interface PlaygroundRefs {
  vfs: KokaVFS;
  lspHandle: LspClientHandle | null;
  wasmCompile: WasmCompileFn | null;
  wasmLoading: Promise<WasmCompileFn | null> | null;

  sourceEditor: MonacoTypes.editor.IStandaloneCodeEditor | null;
  jsEditor: MonacoTypes.editor.IStandaloneCodeEditor | null;
  samplesPreloaded: boolean;
}

// ── Context ───────────────────────────────────────────────────────────────

interface PlaygroundContextValue {
  state: PlaygroundState;
  dispatch: Dispatch<PlaygroundAction>;
  refs: MutableRefObject<PlaygroundRefs>;
}

const PlaygroundContext = createContext<PlaygroundContextValue | null>(null);

export function usePlayground(): PlaygroundContextValue {
  const ctx = useContext(PlaygroundContext);
  if (!ctx) throw new Error('usePlayground() called outside PlaygroundProvider');
  return ctx;
}

export function PlaygroundProvider({
  vfs,
  children,
}: {
  vfs: KokaVFS;
  children: ReactNode;
}) {
  const [state, dispatch] = useReducer(reducer, initialState);
  const refs = useRef<PlaygroundRefs>({
    vfs,
    lspHandle: null,
    wasmCompile: null,
    wasmLoading: null,

    sourceEditor: null,
    jsEditor: null,
    samplesPreloaded: false,
  });

  // Keep vfs ref current
  refs.current.vfs = vfs;

  const value = { state, dispatch, refs };

  return (
    <PlaygroundContext.Provider value={value}>
      {children}
    </PlaygroundContext.Provider>
  );
}
