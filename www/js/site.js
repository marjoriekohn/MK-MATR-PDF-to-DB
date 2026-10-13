// ------ The site: signing in --------------------------------------------------------------------
// What the login screen needs from the browser:
//   * a sign-in link brings the person back with their tokens after # in the address; they are
//     handed to the server (input `auth_tokens`) and taken out of the address, so they are never
//     bookmarked or sent on
//   * while a link is being sent, or the one that brought the person back is signing them in, the
//     button says so and can't be pressed again
//   * Enter in the email box presses the button
//   * the sign-in’s refresh token is kept in this tab (sessionStorage), so a reload signs the person back in; logging out forgets it.

(function () {
  'use strict';

  // The tokens a sign-in link brought back: #access_token=...&refresh_token=...&type=magiclink
  function tokensInAddress() {
    const hash = window.location.hash;
    if (!hash || hash.indexOf('access_token=') === -1) return null;
    const params = new URLSearchParams(hash.replace(/^#/, ''));
    const tokens = {
      access_token: params.get('access_token'),
      refresh_token: params.get('refresh_token'),
      type: params.get('type')
    };
    history.replaceState(null, '', window.location.pathname + window.location.search);
    return tokens;
  }

  // While the server sends a link, or signs someone in, the button says so and can't be pressed
  // again. The server's `login-ready` message ends it when it has something to say (a problem, or that
  // the link was sent); a sign-in that works takes the whole login screen away instead.
  function showBusy(label) {
    const button = document.getElementById('login_submit');
    const card = document.querySelector('.login-card');
    if (!button || !card) return;
    card.classList.add('is-busy');
    card.setAttribute('aria-busy', 'true');
    const message = document.getElementById('login_message');
    if (message) message.innerHTML = '';
    if (!button.dataset.label) button.dataset.label = button.innerHTML;
    button.innerHTML = '<span class="spinner-border spinner-border-sm" aria-hidden="true"></span> ' + label;
    button.disabled = true;
  }

  function showReady() {
    const button = document.getElementById('login_submit');
    const card = document.querySelector('.login-card');
    if (!button || !card) return;
    card.classList.remove('is-busy');
    card.removeAttribute('aria-busy');
    if (button.dataset.label) button.innerHTML = button.dataset.label;
    button.disabled = false;
  }

  // Bubbling to the document, so Shiny's own handler on the button has already counted the press.
  document.addEventListener('click', function (event) {
    const button = event.target.closest && event.target.closest('#login_submit');
    if (!button) return;
    const card = document.querySelector('.login-card');
    if (card && card.classList.contains('is-busy')) return;
    showBusy('Sending a sign-in link…');
  });

   // A link that didn't work comes back with #error=...&error_description=... instead of tokens.
  function problemInAddress() {
    const hash = window.location.hash;
    if (!hash || hash.indexOf('error') === -1) return null;
    const params = new URLSearchParams(hash.replace(/^#/, ''));
    history.replaceState(null, '', window.location.pathname + window.location.search);
    return params.get('error_description') || params.get('error') || 'The sign-in link did not work.';
  }

  // The sign-in's refresh token, kept so a reload doesn't sign the person out. sessionStorage lasts
  // while this tab is open; localStorage would also keep it across tabs and after the browser closes.
  const keptKey = 'matr-refresh-token';
  function kept() { try { return sessionStorage.getItem(keptKey); } catch (e) { return null; } }
  function keep(token) { try { if (token) sessionStorage.setItem(keptKey, token); } catch (e) {} }
  function forget() { try { sessionStorage.removeItem(keptKey); } catch (e) {} }

  // Inputs can only be set once Shiny has connected, so the address is read then, not before.
  function register() {
    const tokens = tokensInAddress();
    if (tokens) {
      showBusy('Signing in…');
      Shiny.setInputValue('auth_tokens', tokens, { priority: 'event' });
      return;
    }
    const problem = problemInAddress();
    if (problem) {
      Shiny.setInputValue('auth_problem', problem, { priority: 'event' });
      return;
    }
    const refresh = kept();
    if (refresh) {
      showBusy('Signing in…');
      Shiny.setInputValue('auth_resume', refresh, { priority: 'event' });
    }
  }

  // Shiny requires each handler to take exactly one argument, even when it isn't used.
  if (window.Shiny) {
    Shiny.addCustomMessageHandler('login-ready', function (_ready) { showReady(); });
    Shiny.addCustomMessageHandler('auth-keep', function (token) { keep(token); });
    Shiny.addCustomMessageHandler('auth-forget', function (_forget) { forget(); });
  }
  jQuery(document).one('shiny:connected', register);

  // Logging out forgets the kept sign-in, so the reload that follows lands on the login screen.
  document.addEventListener('click', function (event) {
    if (event.target.closest && event.target.closest('#log_out')) forget();
  });

    // Enter in the email box presses the button.
  document.addEventListener('keydown', function (event) {
    if (event.key !== 'Enter' || event.target.id !== 'login_email') return;
    event.preventDefault();
    // Shiny waits a moment after typing before sending a box's value; a change sends it now, so the
    // link doesn't go to a half-typed address.
    event.target.dispatchEvent(new Event('change', { bubbles: true }));
    const button = document.getElementById('login_submit');
    if (button) button.click();
  });
})();
