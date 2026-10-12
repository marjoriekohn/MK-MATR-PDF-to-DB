// ------ Extraction pages: dropping files ---------------------------------------------
// A .drop-zone holds a Shiny file input. A file dropped anywhere on the zone is
// handed to that input, as if it had been chosen with the input's button.
(function () {
  'use strict';

  function zoneOf(event) {
    return event.target.closest ? event.target.closest('.drop-zone') : null;
  }

  ['dragenter', 'dragover'].forEach(function (type) {
    document.addEventListener(type, function (event) {
      const zone = zoneOf(event);
      if (!zone) return;
      event.preventDefault();
      zone.classList.add('is-dragged-over');
    });
  });

  document.addEventListener('dragleave', function (event) {
    const zone = zoneOf(event);
    if (zone && !zone.contains(event.relatedTarget)) zone.classList.remove('is-dragged-over');
  });

  document.addEventListener('drop', function (event) {
    const zone = zoneOf(event);
    if (!zone) return;
    zone.classList.remove('is-dragged-over');
    // Shiny takes files dropped on the input's own button
    if (event.target.closest('.input-group')) return;
    event.preventDefault();
    const input = zone.querySelector('input[type="file"]');
    if (!input || event.dataTransfer.files.length === 0) return;
    input.files = event.dataTransfer.files;
    input.dispatchEvent(new Event('change', { bubbles: true }));
  });
})();
