import { useRef } from 'react';
import { usePlayground } from '../editor/context';
import { FileBrowserPanel } from './FileBrowserPanel';
import { ResizeHandle } from './ResizeHandle';
import { SourcePane } from './SourcePane';
import { OutputPane } from './OutputPane';

function TabBar() {
  const { state, dispatch } = usePlayground();

  return (
    <div id="tab-bar">
      {state.tabs.map(tab => (
        <div
          key={tab.id}
          className={'tab' + (tab.id === state.activeTabId ? ' active' : '')}
          title={tab.path}
          onClick={() => dispatch({ type: 'SWITCH_TAB', id: tab.id })}
        >
          <span className="tab-name">{tab.name}</span>
          {tab.path !== 'main.kk' && (
            <span
              className="tab-close"
              title="Close tab"
              onClick={e => {
                e.stopPropagation();
                dispatch({ type: 'CLOSE_TAB', id: tab.id });
              }}
            >
              &times;
            </span>
          )}
        </div>
      ))}
    </div>
  );
}

export function Workspace() {
  const { state } = usePlayground();
  const fbPanelRef = useRef<HTMLDivElement>(null);
  const sourcePaneRef = useRef<HTMLDivElement>(null);
  const editorPanelsRef = useRef<HTMLDivElement>(null);

  return (
    <div id="workspace">
      <div
        id="file-browser-panel"
        ref={fbPanelRef}
        className={state.fileBrowserVisible ? '' : 'collapsed'}
      >
        <div className="panel-title">Explorer</div>
        <div id="file-browser-scroll">
          <FileBrowserPanel />
        </div>
      </div>

      <ResizeHandle
        id="resize-handle-fb"
        axis="x"
        targetRef={fbPanelRef}
        min={140}
        max={500}
        guard={() => state.fileBrowserVisible}
      />

      <div id="editor-area">
        <TabBar />
        <div id="editor-panels" ref={editorPanelsRef}>
          <div id="pane-source" ref={sourcePaneRef}>
            <SourcePane />
          </div>

          <ResizeHandle
            id="resize-handle-h"
            axis="x"
            targetRef={sourcePaneRef}
            min={200}
            max={() => (editorPanelsRef.current?.getBoundingClientRect().width ?? 800) - 200}
          />

          <OutputPane />
        </div>
      </div>
    </div>
  );
}
