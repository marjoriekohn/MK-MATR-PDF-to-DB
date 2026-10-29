// ------ The divider between an outline and its details --------------------------------------------
// Edit Extraction and Make an edit show a list on the left and the picked row's details on the right
// (.outline-split). Dragging the divider between them (.outline-divider) makes the list wider or
// narrower; so do the arrow keys while it has focus, and a double click puts it back. Each page keeps
// its own width in this browser (localStorage), so it stays when the page is opened again. On a phone
// the two take turns instead, and there is no divider.
(function () {
  'use strict';

  const smallest = 256; // px: the list is never narrower than this ...
  const roomForDetails = 320; // ... nor so wide that the details have less than this
  const step = 32; // px moved by an arrow key

  function keyOf(split) {
    const module = split.closest('[data-ns]');
    return 'matr.outline-width.' + (module ? module.dataset.ns : 'page');
  }

  function remembered(split) {
    try { return parseFloat(localStorage.getItem(keyOf(split))); } catch (e) { return NaN; }
  }

  function remember(split, width) {
    try {
      if (width === null) localStorage.removeItem(keyOf(split));
      else localStorage.setItem(keyOf(split), String(Math.round(width)));
    } catch (e) {}
  }

  // Sets the list's width, kept between the smallest and what leaves the details their room. null goes
  // back to the width the stylesheet gives it.
  function setWidth(split, width) {
    const list = split.querySelector(':scope > .outline-body');
    const divider = split.querySelector(':scope > .outline-divider');
    if (!list) return;
    if (width === null || isNaN(width)) {
      list.style.flexBasis = '';
      if (divider) divider.setAttribute('aria-valuenow', Math.round(list.getBoundingClientRect().width));
      return;
    }
    // a page not shown yet has no width to measure: the width is kept as it is until the page shows
    const room = split.getBoundingClientRect().width;
    const most = room > 0 ? Math.max(smallest, room - roomForDetails) : Infinity;
    const kept = Math.min(Math.max(width, smallest), most);
    list.style.flexBasis = kept + 'px';
    if (divider) divider.setAttribute('aria-valuenow', Math.round(kept));
    return kept;
  }

  function listWidth(split) {
    const list = split.querySelector(':scope > .outline-body');
    return list ? list.getBoundingClientRect().width : 0;
  }

  // A page's split gets the width kept for it once it is on the page (Shiny draws the pages after loading).
  function restore(root) {
    (root || document).querySelectorAll('.outline-split').forEach(function (split) {
      if (split.dataset.widthRestored) return;
      split.dataset.widthRestored = 'yes';
      const width = remembered(split);
      if (!isNaN(width)) setWidth(split, width);
    });
  }

  document.addEventListener('DOMContentLoaded', function () { restore(); });
  document.addEventListener('shiny:value', function () { restore(); });

  // ------ Dragging ----------------------------------------------------------------------------
  let dragging = null; // { split, divider, startX, startWidth }

  document.addEventListener('pointerdown', function (event) {
    const divider = event.target.closest && event.target.closest('.outline-divider');
    if (!divider || event.button !== 0) return;
    const split = divider.closest('.outline-split');
    dragging = { split: split, divider: divider, startX: event.clientX, startWidth: listWidth(split) };
    divider.setPointerCapture(event.pointerId);
    divider.classList.add('is-dragging');
    document.body.classList.add('is-resizing');
    event.preventDefault();
  });

  document.addEventListener('pointermove', function (event) {
    if (!dragging) return;
    setWidth(dragging.split, dragging.startWidth + event.clientX - dragging.startX);
  });

  function stop() {
    if (!dragging) return;
    remember(dragging.split, listWidth(dragging.split));
    dragging.divider.classList.remove('is-dragging');
    document.body.classList.remove('is-resizing');
    dragging = null;
  }
  document.addEventListener('pointerup', stop);
  document.addEventListener('pointercancel', stop);

  document.addEventListener('dblclick', function (event) {
    const divider = event.target.closest && event.target.closest('.outline-divider');
    if (!divider) return;
    const split = divider.closest('.outline-split');
    setWidth(split, null);
    remember(split, null);
  });

  document.addEventListener('keydown', function (event) {
    const divider = event.target.closest && event.target.closest('.outline-divider');
    if (!divider || (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight')) return;
    const split = divider.closest('.outline-split');
    const kept = setWidth(split, listWidth(split) + (event.key === 'ArrowRight' ? step : -step));
    remember(split, kept);
    event.preventDefault();
  });

  // A window made narrower keeps the details their room.
  window.addEventListener('resize', function () {
    document.querySelectorAll('.outline-split').forEach(function (split) {
      const list = split.querySelector(':scope > .outline-body');
      if (list && list.style.flexBasis) setWidth(split, listWidth(split));
    });
  });
})();
