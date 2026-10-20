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

// ------ Extraction page: how long reading has taken ----------------------------------
// A .reading-elapsed holds the seconds reading had taken when the server drew it (R:
// reading_elapsed()). The browser counts on from there, so the server isn't asked every second.
(function () {
  'use strict';

  function words(seconds) {
    return seconds < 60 ? seconds + ' s' : Math.floor(seconds / 60) + ' min ' + (seconds % 60) + ' s';
  }

  setInterval(function () {
    document.querySelectorAll('.reading-elapsed').forEach(function (el) {
      if (!el.dataset.drawnAt) el.dataset.drawnAt = String(Date.now());
      const since = Math.floor((Date.now() - Number(el.dataset.drawnAt)) / 1000);
      el.textContent = words(Number(el.dataset.elapsed || 0) + since);
    });
  }, 1000);
})();
