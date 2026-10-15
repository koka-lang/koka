/**
 * color-theme.ts
 *
 * The playground's light or dark color theme. It is set on <html> as
 * `data-theme`, which selects the CSS palette in index.html; the editor theme
 * and the LSP's hover colors follow it.
 */

import type { ThemeKind } from './koka-theme';

const STORAGE_KEY = 'koka-playground-theme';

/** The viewer's saved choice, else the operating system's color scheme. */
export function initialColorTheme(): ThemeKind {
  try {
    const saved = localStorage.getItem(STORAGE_KEY);
    if (saved === 'dark' || saved === 'light') return saved;
  } catch {
    // storage can be unavailable (private windows, blocked site data)
  }
  return window.matchMedia('(prefers-color-scheme: light)').matches ? 'light' : 'dark';
}

export function currentColorTheme(): ThemeKind {
  return document.documentElement.dataset.theme === 'light' ? 'light' : 'dark';
}

export function applyColorTheme(kind: ThemeKind, save: boolean): void {
  document.documentElement.dataset.theme = kind;
  if (!save) return;
  try {
    localStorage.setItem(STORAGE_KEY, kind);
  } catch {
    // the choice then lasts for this page only
  }
}
