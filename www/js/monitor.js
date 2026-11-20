// ------ AI Monitor: Expand All and Collapse All ------------------------------------------------------
// Each kind of correction is a card (a <details>, R: monitor_kind_ui()) that opens to its corrections. One button
// above them (.monitor-expand-all) opens every card, or, when they are all open, shuts them. It says which it will
// do by its aria-expanded: "true" while every card is open (the stylesheet shows Collapse All then). Opening or
// shutting a card by hand changes what it says, and cards drawn again (another choice of blocks, variables or
// choices) open if every card was open before.
(function () {
  'use strict';

  function pageOf(element) { return element.closest('.monitor-page'); }

  function cards(page) { return Array.from(page.querySelectorAll('details.monitor-kind')); }

  // Says on the button whether every card is open (there is nothing to open when there are none).
  function showState(page) {
    const button = page.querySelector('.monitor-expand-all');
    if (!button) return;
    const all = cards(page);
    button.setAttribute('aria-expanded', all.length > 0 && all.every(function (card) { return card.open; }) ? 'true' : 'false');
  }

  document.addEventListener('click', function (event) {
    const button = event.target.closest && event.target.closest('.monitor-expand-all');
    if (!button) return;
    const page = pageOf(button);
    if (!page) return;
    const open = button.getAttribute('aria-expanded') !== 'true';
    cards(page).forEach(function (card) { card.open = open; });
    button.setAttribute('aria-expanded', open ? 'true' : 'false');
  });

  // A card's toggle event doesn't bubble, so it is heard on its way down.
  document.addEventListener('toggle', function (event) {
    if (!event.target.matches || !event.target.matches('details.monitor-kind')) return;
    const page = pageOf(event.target);
    if (page) showState(page);
  }, true);

  // Shiny draws an output's new content some time after this event, so the cards drawn again are opened once
  // they are in the output. (Shiny sends the event with jQuery, which a plain listener doesn't hear.)
  $(document).on('shiny:value', '.monitor-common', function (event) {
    const output = event.currentTarget;
    const page = pageOf(output);
    if (!page) return;
    const button = page.querySelector('.monitor-expand-all');
    const wasOpen = button && button.getAttribute('aria-expanded') === 'true';
    const drawn = new MutationObserver(function () {
      if (!output.querySelector('details.monitor-kind') && !output.querySelector('.list-card')) return;
      drawn.disconnect();
      if (wasOpen) cards(page).forEach(function (card) { card.open = true; });
      showState(page);
    });
    drawn.observe(output, { childList: true, subtree: true });
  });
})();
