// ------ Showing that the site is working ----------------------------------------------------------
// Shiny's own busy indicator (useBusyIndicators() in ui.R) puts a gold bar across the top of the page
// while the server is working; it is the only sign of it. The bar is for work staff asked for: a button
// pressed, or anything that takes a while. The quick round trips that every keystroke, pick and tick
// sends (and the checks on a reading every few seconds) don't show it: the page is marked
// `is-quiet-busy` (components/_busy.scss hides the bar), unless the server is still busy after
// `slowMs`, when the bar shows after all.
// The button pressed last can't be pressed again until the server is done, so a slow save or submit
// isn't sent twice. A press that sets off no work (opening a menu) changes nothing: the button is only
// held if the server gets busy soon after it was pressed.
(function () {
  'use strict';

  const pressWindowMs = 1000; // how soon after a press the server's work counts as the press's
  const slowMs = 1500; // work that wasn't a press shows the bar once it has taken this long
  const quietClass = 'is-quiet-busy';
  let pressed = null; // { button, at }
  let working = null; // the button held while the server works on its press
  let slowTimer = null;

  function isButton(el) {
    return el && !el.disabled && !el.closest('.dropdown-toggle, .btn-toggle, [data-bs-toggle]');
  }

  document.addEventListener('click', function (event) {
    const button = event.target.closest && event.target.closest('button, .btn, .action-button');
    if (!isButton(button)) return;
    pressed = { button: button, at: Date.now() };
  }, true);

  // Whether the work the server has started is a button's: then the button is held, and the bar shows.
  function holdPressed() {
    if (!pressed || Date.now() - pressed.at > pressWindowMs) return false;
    const button = pressed.button;
    pressed = null;
    if (!document.body.contains(button) || button.classList.contains('is-working')) return true;
    working = button;
    button.classList.add('is-working');
    button.setAttribute('aria-busy', 'true');
    return true;
  }

  function quiet() {
    document.documentElement.classList.add(quietClass);
    clearTimeout(slowTimer);
    slowTimer = setTimeout(function () {
      document.documentElement.classList.remove(quietClass);
    }, slowMs);
  }

  function start() {
    if (holdPressed()) {
      clearTimeout(slowTimer);
      document.documentElement.classList.remove(quietClass);
    } else if (!working) {
      quiet();
    }
  }

  function stop() {
    clearTimeout(slowTimer);
    document.documentElement.classList.remove(quietClass);
    if (!working) return;
    working.classList.remove('is-working');
    working.removeAttribute('aria-busy');
    working = null;
  }

  jQuery(document).on('shiny:busy', start);
  jQuery(document).on('shiny:idle', stop);
})();
