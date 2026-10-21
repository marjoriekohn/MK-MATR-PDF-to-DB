// ------ Showing that the site is working ----------------------------------------------------------
// Shiny's own busy indicator (useBusyIndicators() in ui.R) puts a gold bar across the top of the page
// while the server is working; it is the only sign of it. The button pressed last can't be pressed again
// until the server is done, so a slow save or submit isn't sent twice. A press that sets off no work
// (opening a menu) changes nothing: the button is only held if the server gets busy soon after it was
// pressed.
(function () {
  'use strict';

  const pressWindowMs = 1000; // how soon after a press the server's work counts as the press's
  let pressed = null; // { button, at }
  let working = null; // the button showing the spinner

  function isButton(el) {
    return el && !el.disabled && !el.closest('.dropdown-toggle, .btn-toggle, [data-bs-toggle]');
  }

  document.addEventListener('click', function (event) {
    const button = event.target.closest && event.target.closest('button, .btn, .action-button');
    if (!isButton(button)) return;
    pressed = { button: button, at: Date.now() };
  }, true);

  function start() {
    if (!pressed || Date.now() - pressed.at > pressWindowMs) return;
    const button = pressed.button;
    pressed = null;
    if (!document.body.contains(button) || button.classList.contains('is-working')) return;
    working = button;
    button.classList.add('is-working');
    button.setAttribute('aria-busy', 'true');
  }

  function stop() {
    if (!working) return;
    working.classList.remove('is-working');
    working.removeAttribute('aria-busy');
    working = null;
  }

  jQuery(document).on('shiny:busy', start);
  jQuery(document).on('shiny:idle', stop);
})();
