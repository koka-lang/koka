import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

export default defineConfig({
  plugins: [react()],
  base: './',
  build: {
    outDir: 'dist',
    sourcemap: true,
    rollupOptions: {
      input: {
        main: 'index.html',
        embed: 'embed.html',
      },
      output: {
        // Pages that embed <koka-editor> load it by a fixed name:
        // <script type="module" src=".../playground/koka-editor.js">.
        entryFileNames: (chunk) => chunk.name === 'embed' ? 'koka-editor.js' : 'assets/[name]-[hash].js',
      },
    },
  },
  worker: {
    format: 'es',
  },
  optimizeDeps: {
    include: [
      'vscode/localExtensionHost',
      '@codingame/monaco-vscode-editor-api',
    ],
  },
  // In dev, use `web/public/` for static assets (all.js, precompiled/, lib/, manifests)
  // In production, these are assembled by the CI pipeline into the same dist/ folder
  publicDir: 'public',
});
