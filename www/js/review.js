// ------ Page events ------------------------------------------------------------
// The Edit Extraction page's outline, and the data-action buttons every page uses: loaded on every page
// (the Extraction, Change Requests, Make an edit and Home pages use data-action buttons too).
// The HTML that R renders says what to send back, so no R code builds JavaScript:
//   data-ns       a module's outer element: its Shiny namespace, e.g. "edit_extraction-outline-"
//   data-action   a click sends the element's data-* values to that module's `action`
//   data-field    a change sends {table, id, field, value} to that module's `edit`
//   data-suggest  a text field that offers the words in the <datalist> this names, as they are typed
// Drag and drop is SortableJS (from the sortable package). The callbacks named in
// sortable_options() in R are at the bottom.
(function () {
  'use strict';

  // Send a value to an input of the module that `el` sits in.
  function send(el, name, value) {
    const module = el.closest('[data-ns]');
    if (module && window.Shiny) {
      Shiny.setInputValue(module.dataset.ns + name, value, { priority: 'event' });
    }
  }

  // ------ Buttons and outline rows -----------------------------------------------
  document.addEventListener('click', function (event) {
    const target = event.target.closest('[data-action]');
    if (target && !target.disabled) send(target, 'action', Object.assign({}, target.dataset));
  });

  // Outline rows can be reached with Tab; Enter or Space selects one (not while typing in its words).
  document.addEventListener('keydown', function (event) {
    const row = event.target;
    if (row.matches('.outline-row') && (event.key === 'Enter' || event.key === ' ')) {
      event.preventDefault();
      row.click();
    }
  });

  // ------ Fields -------------------------------------------------------------------
  // Words are sent once typing pauses, and again when the field loses focus.
  // Drop-downs, toggles and checkboxes are sent straight away.
  function isTyped(field) {
    return field.tagName === 'TEXTAREA' || field.type === 'text' || field.type === 'number';
  }

  function sendField(field) {
    clearTimeout(field.reviewTimer);
    send(field, 'edit', {
      table: field.dataset.table,
      id: field.dataset.id,
      field: field.dataset.field,
      value: field.type === 'checkbox' ? field.checked : field.value,
      typed: isTyped(field)
    });
  }

  document.addEventListener('input', function (event) {
    const field = event.target.closest('[data-field]');
    if (field && isTyped(field)) {
      clearTimeout(field.reviewTimer);
      field.reviewTimer = setTimeout(function () { sendField(field); }, 400);
    }
    if (field && field.matches('textarea.outline-text')) autogrow(field);
  });

  document.addEventListener('change', function (event) {
    const field = event.target.closest('[data-field]');
    if (field) sendField(field);
  });

  // ------ A block's words, typed in its row ----------------------------------------------
  // The box grows with its words. Enter commits (Shift+Enter starts a new line).
  function autogrow(field) {
    field.style.height = 'auto';
    field.style.height = field.scrollHeight + 'px';
  }

  document.addEventListener('keydown', function (event) {
    const field = event.target;
    if (!field.matches || !field.matches('textarea.outline-text')) return;
    if (event.key === 'Enter' && !event.shiftKey) {
      // a highlighted suggestion is taken; otherwise the words are done
      if (suggest.pick()) { event.preventDefault(); return; }
      event.preventDefault();
      field.blur();
    } else if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
      if (suggest.move(event.key === 'ArrowDown' ? 1 : -1)) event.preventDefault();
    } else if (event.key === 'Escape') {
      suggest.hide();
    }
  });

  // ------ Suggestions ------------------------------------------------------------------
  // A text field with data-suggest offers the words of the <datalist> it names (a textarea can't use a
  // datalist itself). The list drops down under the field, narrowed to what is typed; a click or Enter
  // puts the words in the field and sends them.
  const suggest = {
    box: null,
    field: null,
    active: -1,

    show: function (field) {
      const list = document.getElementById(field.dataset.suggest);
      if (!list) return this.hide();
      const typed = field.value.trim().toLowerCase();
      const options = Array.from(list.options)
        .map(function (option) { return option.value; })
        .filter(function (value) { return value.toLowerCase() !== typed && (typed === '' || value.toLowerCase().indexOf(typed) !== -1); })
        .slice(0, 8);
      if (options.length === 0) return this.hide();
      if (!this.box) {
        this.box = document.createElement('div');
        this.box.className = 'suggest-box';
        this.box.setAttribute('role', 'listbox');
        document.body.appendChild(this.box);
        this.box.addEventListener('mousedown', function (event) {
          const item = event.target.closest('.suggest-item');
          if (!item) return;
          event.preventDefault(); // keep the field focused
          suggest.choose(item.textContent);
        });
      }
      this.field = field;
      this.active = -1;
      this.box.innerHTML = '';
      options.forEach(function (value) {
        const item = document.createElement('div');
        item.className = 'suggest-item';
        item.setAttribute('role', 'option');
        item.textContent = value;
        suggest.box.appendChild(item);
      });
      const rect = field.getBoundingClientRect();
      this.box.style.left = (rect.left + window.scrollX) + 'px';
      this.box.style.top = (rect.bottom + window.scrollY) + 'px';
      this.box.style.width = rect.width + 'px';
      this.box.style.display = 'block';
    },

    hide: function () {
      if (this.box) this.box.style.display = 'none';
      this.field = null;
      this.active = -1;
    },

    move: function (step) {
      if (!this.box || this.box.style.display === 'none') return false;
      const items = this.box.querySelectorAll('.suggest-item');
      this.active = (this.active + step + items.length) % items.length;
      items.forEach(function (item, i) { item.classList.toggle('is-active', i === suggest.active); });
      return true;
    },

    pick: function () {
      if (!this.box || this.box.style.display === 'none' || this.active < 0) return false;
      this.choose(this.box.querySelectorAll('.suggest-item')[this.active].textContent);
      return true;
    },

    choose: function (value) {
      const field = this.field;
      if (!field) return;
      field.value = value;
      autogrow(field);
      this.hide();
      sendField(field);
    }
  };

  document.addEventListener('focusin', function (event) {
    const field = event.target;
    if (field.matches && field.matches('[data-suggest]')) {
      autogrow(field);
      suggest.show(field);
    }
  });
  document.addEventListener('input', function (event) {
    if (event.target.matches && event.target.matches('[data-suggest]')) suggest.show(event.target);
  });
  document.addEventListener('focusout', function (event) {
    if (event.target.matches && event.target.matches('[data-suggest]')) setTimeout(function () { suggest.hide(); }, 150);
  });

  // ------ Marks the server sends as words are typed ---------------------------------------
  // The outline is only redrawn when its layout changes, so the selected row and each block's problems
  // come separately, and are put on again whenever Shiny redraws something.
  let marks = { selected: null, problems: [] };

  function applyMarks() {
    const tree = document.querySelector('.outline-tree');
    if (!tree) return;
    tree.querySelectorAll('.outline-row.is-selected').forEach(function (row) {
      if (row.dataset.id !== marks.selected) row.classList.remove('is-selected');
    });
    const byId = {};
    marks.problems.forEach(function (problem) { byId[problem.id] = problem; });
    tree.querySelectorAll('.outline-node').forEach(function (node) {
      const row = node.querySelector(':scope > .outline-row');
      if (!row) return;
      if (row.dataset.id === marks.selected && !row.classList.contains('is-selected')) {
        row.classList.add('is-selected');
        row.scrollIntoView({ block: 'nearest' });
      }
      const badge = row.querySelector('.outline-problems');
      if (!badge) return;
      const problem = byId[row.dataset.id];
      badge.classList.toggle('is-hidden', !problem);
      badge.title = problem ? problem.title : '';
      const count = badge.querySelector('.problem-count');
      if (count) count.textContent = problem ? problem.n + ' to fix' : '0 to fix';
    });
  }

  function register() {
    Shiny.addCustomMessageHandler('review-marks', function (sent) {
      marks = { selected: sent.selected || null, problems: sent.problems || [] };
      applyMarks();
    });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) register();
  else if (window.jQuery) jQuery(document).one('shiny:connected', register);

  // ------ After Shiny redraws ------------------------------------------------------
  // Put the marks back, size the text boxes, and give a block that has no words yet the cursor.
  $(document).on('shiny:value', function () {
    setTimeout(function () {
      applyMarks();
      document.querySelectorAll('textarea.outline-text').forEach(autogrow);
      const emptyWords = document.querySelector('.outline-row.is-selected [data-autofocus]');
      if (emptyWords && document.activeElement !== emptyWords) emptyWords.focus();
    });
  });

  // ------ Leaving the page -----------------------------------------------------------
  window.addEventListener('beforeunload', function (event) {
    if (document.querySelector('.save-status.is-dirty')) {
      event.preventDefault();
      event.returnValue = '';
    }
  });

  // ------ Drag and drop ---------------------------------------------------------------
  window.reviewPage = {
    // While a block is dragged, say in words where it will land.
    showDropHint: function (evt) {
      const where = evt.to.dataset.label ? 'inside ' + evt.to.dataset.label : 'at the top level';
      const near = evt.related && evt.related !== evt.to ? evt.related.dataset.label : null;
      const next = near ? (evt.willInsertAfter ? ', after ' : ', before ') + near : '';
      evt.dragged.dataset.dropHint = 'Drop here: ' + where + next;
    },

    // After a block is dropped, send every block id in printed order, each with
    // the section it's in.
    sendOutline: function (evt) {
      const tree = evt.item.closest('.outline-tree');
      const ids = [];
      const parents = [];
      tree.querySelectorAll('.outline-node').forEach(function (node) {
        const section = node.parentElement.closest('.outline-node');
        ids.push(node.dataset.id);
        parents.push(section ? section.dataset.id : '');
      });
      send(tree, 'order', { ids: ids, parents: parents });
    },

    // After variables or choices are dragged, send their new order.
    sendList: function (evt) {
      const ids = Array.from(evt.to.children).map(function (el) { return el.dataset.id; });
      send(evt.to, 'reorder', { table: evt.to.dataset.table, ids: ids });
    }
  };
})();
