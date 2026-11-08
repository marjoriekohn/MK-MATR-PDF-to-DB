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
