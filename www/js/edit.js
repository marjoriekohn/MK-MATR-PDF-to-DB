// ------ Make an edit page: scrolling --------------------------------------------------
// The server asks for the page to scroll to an element (by id) once a row is
// picked, so the form that follows isn't left below the list of cards.
(function () {
  'use strict';

  function register() {
    Shiny.addCustomMessageHandler('edit-scroll-to', function (id) {
      // after Shiny has drawn what's new
      setTimeout(function () {
        const output = document.getElementById(id);
        // an output's own element can have no box of its own; what it holds does
        const target = output && (output.getBoundingClientRect().height > 0 ? output : output.firstElementChild);
        if (target) target.scrollIntoView({ behavior: 'smooth', block: 'start' });
      }, 150);
    });
  }

  // Shiny announces itself through jQuery, not as a DOM event.
  if (window.Shiny && Shiny.addCustomMessageHandler) register();
  else if (window.jQuery) jQuery(document).one('shiny:connected', register);
})();
