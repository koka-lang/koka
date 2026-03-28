import { useState } from 'react';
import { useMonaco } from '../editor/monaco-provider';
import { usePlayground } from '../editor/context';
import { KOKA_LANGUAGE_ID } from '../editor/koka-lang';
import { ALL_WEB_SAMPLE, type FileEntry } from '../editor/samples';
import { preloadSamplesDirectory } from '../editor/actions';

export function FileBrowserPanel() {
  const { state, dispatch } = usePlayground();

  return (
    <div id="file-browser-tree" className="file-browser">
      {/* Open files — derived from tabs */}
      <div className="fb-section">
        <div className="fb-section-header" role="button">
          <span className="fb-arrow">{'\u25BE'}</span>
          <span className="fb-section-title">Open Files</span>
        </div>
        <div className="fb-section-body">
          {state.tabs.map(tab => (
            <div
              key={tab.id}
              className={'fb-item fb-item-clickable' + (tab.id === state.activeTabId ? ' fb-item-active' : '')}
              style={{ paddingLeft: '8px' }}
              title={tab.path}
              onClick={() => dispatch({ type: 'SWITCH_TAB', id: tab.id })}
            >
              <span className="fb-item-icon">
                <img src="koka-icon-dark.svg" alt="kk" className="fb-file-icon" />
              </span>
              <span className="fb-item-name">{tab.name}</span>
            </div>
          ))}
        </div>
      </div>

      {/* Dynamic sections (Samples, Output) */}
      {state.fileBrowserSections.map(section => (
        <div key={section.title} className="fb-section">
          <div
            className="fb-section-header"
            role="button"
            onClick={() => dispatch({ type: 'TOGGLE_SECTION', title: section.title })}
          >
            <span className="fb-arrow">{section.collapsed ? '\u25B8' : '\u25BE'}</span>
            <span className="fb-section-title">{section.title}</span>
          </div>
          <div className="fb-section-body" style={{ display: section.collapsed ? 'none' : '' }}>
            {section.entries.length === 0 ? (
              <div className="fb-empty">(empty)</div>
            ) : (
              section.entries.map(entry => (
                <FileTreeNode key={entry.path} entry={entry} depth={0} />
              ))
            )}
          </div>
        </div>
      ))}
    </div>
  );
}

function FileTreeNode({ entry, depth }: { entry: FileEntry; depth: number }) {
  const monaco = useMonaco();
  const { dispatch, refs } = usePlayground();
  const [expanded, setExpanded] = useState(false);

  const isJs = entry.name.endsWith('.mjs') || entry.name.endsWith('.js');
  const isKk = entry.name.endsWith('.kk') || entry.name.endsWith('.kki');

  if (entry.type === 'directory') {
    const icon = expanded ? '▾' : '▸';

    return (
      <div>
        <div
          className="fb-item fb-item-dir"
          style={{ paddingLeft: `${8 + depth * 14}px` }}
          title={entry.path}
          onClick={() => setExpanded(!expanded)}
        >
          <span className="fb-item-icon">{icon}</span>
          <span className="fb-item-name">{entry.name}</span>
        </div>
        {expanded && entry.children && (
          <div>
            {entry.children.map(child => (
              <FileTreeNode key={child.path} entry={child} depth={depth + 1} />
            ))}
          </div>
        )}
      </div>
    );
  }

  // File
  const icon = isKk
    ? <img src="koka-icon-dark.svg" alt="kk" className="fb-file-icon" />
    : isJs
    ? <span className="fb-file-icon fb-js-icon">JS</span>
    : <span className="fb-file-icon">•</span>;

  const handleFileClick = async () => {
    // Special case: web-compatible all.kk with inline content
    if (entry.path === 'all-web') {
      await preloadSamplesDirectory(dispatch, refs);
      const uriStr = 'inmemory://playground/all.kk';
      const uri = monaco.Uri.parse(uriStr);
      let model = monaco.editor.getModel(uri);
      if (!model) {
        model = monaco.editor.createModel(ALL_WEB_SAMPLE, KOKA_LANGUAGE_ID, uri);
      } else {
        model.setValue(ALL_WEB_SAMPLE);
      }
      const id = 'tab-' + Math.random().toString(36).slice(2, 9);
      dispatch({ type: 'OPEN_TAB', tab: { id, name: 'all.kk', path: 'all.kk', modelUri: uriStr } });
      return;
    }

    // Preload sibling samples into VFS for module resolution
    if (entry.path.startsWith('samples/')) {
      await preloadSamplesDirectory(dispatch, refs);
    }

    let content = entry.content ?? '';
    if (entry.download_url && !content) {
      try {
        const r = await fetch(entry.download_url);
        content = await r.text();
      } catch {
        content = `// Could not load ${entry.name}`;
      }
    }

    if (isJs) {
      refs.current.jsEditor?.setValue(content);
      dispatch({ type: 'SET_JS_OUTPUT_VISIBLE', visible: true });
    } else {
      // Create or update the Monaco model with the fetched content
      const uriStr = `inmemory://playground/${entry.path}`;
      const uri = monaco.Uri.parse(uriStr);
      let model = monaco.editor.getModel(uri);
      const language = isKk ? KOKA_LANGUAGE_ID : 'plaintext';
      if (!model) {
        model = monaco.editor.createModel(content, language, uri);
      } else {
        model.setValue(content);
      }

      const id = 'tab-' + Math.random().toString(36).slice(2, 9);
      dispatch({
        type: 'OPEN_TAB',
        tab: { id, name: entry.name, path: entry.path, modelUri: uriStr },
      });
    }
  };

  return (
    <div
      className="fb-item fb-item-clickable"
      style={{ paddingLeft: `${8 + depth * 14}px` }}
      title={entry.path}
      onClick={handleFileClick}
    >
      <span className="fb-item-icon">{icon}</span>
      <span className="fb-item-name">{entry.name}</span>
    </div>
  );
}
