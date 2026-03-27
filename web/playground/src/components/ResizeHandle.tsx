import { useEffect, useRef, type RefObject } from 'react';

interface ResizeHandleProps {
  id?: string;
  axis: 'x' | 'y';
  targetRef: RefObject<HTMLElement | null>;
  min: number;
  max: number | (() => number);
  invertDelta?: boolean;
  onResize?: () => void;
  guard?: () => boolean;
}

export function ResizeHandle({
  id, axis, targetRef, min, max, invertDelta, onResize, guard,
}: ResizeHandleProps) {
  const handleRef = useRef<HTMLDivElement>(null);

  const maxRef = useRef(max);
  const onResizeRef = useRef(onResize);
  const guardRef = useRef(guard);
  maxRef.current = max;
  onResizeRef.current = onResize;
  guardRef.current = guard;

  useEffect(() => {
    const handle = handleRef.current;
    const target = targetRef.current;
    if (!handle || !target) return;

    let dragging = false;
    let startPos = 0;
    let startSize = 0;
    const isX = axis === 'x';
    const cursor = isX ? 'col-resize' : 'row-resize';

    const getPos = (e: MouseEvent | Touch) => isX ? e.clientX : e.clientY;

    const beginDrag = (pos: number) => {
      if (guardRef.current && !guardRef.current()) return;
      dragging = true;
      startPos = pos;
      startSize = isX ? target.getBoundingClientRect().width : target.getBoundingClientRect().height;
      handle.classList.add('dragging');
      document.body.style.cursor = cursor;
      document.body.style.userSelect = 'none';
    };

    const updateDrag = (pos: number) => {
      if (!dragging) return;
      const rawDelta = pos - startPos;
      const delta = invertDelta ? -rawDelta : rawDelta;
      const m = maxRef.current;
      const maxVal = typeof m === 'function' ? m() : m;
      const newSize = Math.min(Math.max(startSize + delta, min), maxVal);
      if (isX) target.style.width = `${newSize}px`;
      else target.style.height = `${newSize}px`;
      onResizeRef.current?.();
    };

    const endDrag = () => {
      if (!dragging) return;
      dragging = false;
      handle.classList.remove('dragging');
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    };

    // Mouse events
    const onMouseDown = (e: MouseEvent) => { beginDrag(getPos(e)); e.preventDefault(); };
    const onMouseMove = (e: MouseEvent) => updateDrag(getPos(e));
    const onMouseUp = () => endDrag();

    // Touch events
    const onTouchStart = (e: TouchEvent) => {
      if (e.touches.length === 1) {
        beginDrag(getPos(e.touches[0]));
        e.preventDefault();
      }
    };
    const onTouchMove = (e: TouchEvent) => {
      if (e.touches.length === 1) {
        updateDrag(getPos(e.touches[0]));
        e.preventDefault();
      }
    };
    const onTouchEnd = () => endDrag();

    handle.addEventListener('mousedown', onMouseDown);
    document.addEventListener('mousemove', onMouseMove);
    document.addEventListener('mouseup', onMouseUp);
    handle.addEventListener('touchstart', onTouchStart, { passive: false });
    document.addEventListener('touchmove', onTouchMove, { passive: false });
    document.addEventListener('touchend', onTouchEnd);

    return () => {
      handle.removeEventListener('mousedown', onMouseDown);
      document.removeEventListener('mousemove', onMouseMove);
      document.removeEventListener('mouseup', onMouseUp);
      handle.removeEventListener('touchstart', onTouchStart);
      document.removeEventListener('touchmove', onTouchMove);
      document.removeEventListener('touchend', onTouchEnd);
    };
  }, [axis, targetRef, min, invertDelta]);

  return <div ref={handleRef} id={id} style={{ touchAction: 'none' }} />;
}
