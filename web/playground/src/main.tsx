/**
 * main.tsx
 *
 * React entry point for the Koka Playground.
 */

import { createRoot } from 'react-dom/client';
import { MonacoProvider } from './editor/monaco-provider';
import { PlaygroundProvider } from './editor/context';
import { KokaVFS } from './editor/vfs';
import { App } from './components/App';

// Load coi-serviceworker (for cross-origin isolation + SharedArrayBuffer)
// This must run before app initialization and needs to be a classic script.
const coiScript = document.createElement('script');
coiScript.src = './coi-serviceworker.js';
document.head.appendChild(coiScript);

const vfs = new KokaVFS();

createRoot(document.getElementById('app')!).render(
  <MonacoProvider>
    <PlaygroundProvider vfs={vfs}>
      <App />
    </PlaygroundProvider>
  </MonacoProvider>
);
