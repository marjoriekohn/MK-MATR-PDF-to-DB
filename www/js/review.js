// ------ Page events ------------------------------------------------------------
// The Edit Extraction page's outline, and the data-action buttons every page uses: loaded on every page
// (the Extraction, Change Requests, Make an edit and Home pages use data-action buttons too).
// The HTML that R renders says what to send back, so no R code builds JavaScript:
//   data-ns       a module's outer element: its Shiny namespace, e.g. "edit_extraction-outline-"
//   data-action   a click sends the element's data-* values to that module's `action`
//   data-field    a change sends {table, id, field, value} to that module's `edit`
//   data-suggest  a text field that offers the words in the <datalist> this names, as they are typed
//   data-pick     a text field that is sent only once it is done (Enter, leaving it, or a suggestion
//                 picked), not as it is typed: what it holds is looked up, like a drop-down's pick
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

  // A change inside the choices a question's variables share (data-shared) says which variables it goes to.
  function withShared(el, data) {
    const box = el.closest('[data-shared]');
    if (box && box.dataset.shared) data.shared = box.dataset.shared;
    return data;
  }

  // ------ Buttons and outline rows -----------------------------------------------
  document.addEventListener('click', function (event) {
    const target = event.target.closest('[data-action]');
    if (!target || target.disabled) return;
    // typing a section's or text block's words in its row, opening a menu in it, or pressing its edit button
    // doesn't pick it (or open or close it)
    if (target.matches('.outline-row') &&
      event.target.closest('textarea:not([readonly]), input, .dropdown, [data-edit-words], .drag-handle')) return;
    // a section or question group opens or closes straight away; the server's marks then say the same
    if (target.matches('.outline-row.is-container')) toggleOpen(target);
    send(target, 'action', withShared(target, Object.assign({}, target.dataset)));
    // a pick in a dialog of boxes (+ Block, + Variable) closes it
    if (target.matches('.pick-box')) $(target.closest('.modal')).modal('hide');
  });

  function toggleOpen(row) {
    const node = row.closest('.outline-node');
    if (!node) return;
    const closed = node.classList.toggle('is-collapsed');
    row.setAttribute('aria-expanded', closed ? 'false' : 'true');
    if (marks.collapsed) {
      if (closed) marks.collapsed.add(row.dataset.id);
      else marks.collapsed.delete(row.dataset.id);
    }
    sizeFreshBoxes();
  }

  // ------ Words typed in the list, once its edit button is pressed ----------------------------------
  // A section's or text block's words in the list can't be typed in until its edit button is pressed (a click on
  // them opens or picks the row); they can't again once the box is left.
  document.addEventListener('click', function (event) {
    const button = event.target.closest && event.target.closest('[data-edit-words]');
    const row = button && button.closest('.outline-row');
    const field = row && row.querySelector(':scope > textarea.outline-text');
    if (!field) return;
    field.readOnly = false;
    field.focus();
    field.setSelectionRange(field.value.length, field.value.length);
  });
  document.addEventListener('focusout', function (event) {
    const field = event.target;
    if (field.matches && field.matches('.outline-tree textarea.outline-text')) field.readOnly = true;
  });

  // Outline rows can be reached with Tab; Enter or Space selects one (not while typing in its words).
  document.addEventListener('keydown', function (event) {
    const row = event.target;
    if (row.matches('.outline-row') && (event.key === 'Enter' || event.key === ' ')) {
      event.preventDefault();
      row.click();
    }
  });

  // ------ Being active -------------------------------------------------------------
  // The draft open on the Edit Extraction page is kept for this person only while they are doing
  // something on the site (R/31_edit_extraction_server.R). Any key, click, scroll or move of the
  // pointer, on any page, counts; it is sent to that page's `active` at most every 20 seconds.
  let lastActive = 0;
  function sendActive() {
    const now = Date.now();
    const page = document.querySelector('.edit-extraction-page[data-ns]');
    if (now - lastActive < 20000 || !page || !window.Shiny || !Shiny.setInputValue) return;
    lastActive = now;
    Shiny.setInputValue(page.dataset.ns + 'active', now);
  }
  ['keydown', 'pointerdown', 'pointermove', 'wheel', 'scroll'].forEach(function (name) {
    document.addEventListener(name, sendActive, { capture: true, passive: true });
  });

  // ------ Fields -------------------------------------------------------------------
  // Words are sent once typing pauses, and again when the field loses focus.
  // Drop-downs, toggles, checkboxes and data-pick fields are sent once they are picked.
  function isTyped(field) {
    if (field.hasAttribute('data-pick')) return false;
    return field.tagName === 'TEXTAREA' || field.type === 'text' || field.type === 'number';
  }

  function sendField(field) {
    clearTimeout(field.sendTimer);
    send(field, 'edit', withShared(field, {
      table: field.dataset.table,
      id: field.dataset.id,
      field: field.dataset.field,
      value: field.type === 'checkbox' ? field.checked : field.value,
      typed: isTyped(field)
    }));
  }

  // A field can be on the page twice: in its row and in the picked block's details. What is typed or picked
  // in one is put in the other, so the row's words follow the details' as they are typed.
  function fieldSelectorOf(field) {
    return '[data-table="' + CSS.escape(field.dataset.table || '') + '"][data-id="' + CSS.escape(field.dataset.id || '') +
      '"][data-field="' + CSS.escape(field.dataset.field) + '"]';
  }
  function copyToTwins(field) {
    if (!field.dataset.table || field.type === 'radio') return;
    document.querySelectorAll(fieldSelectorOf(field)).forEach(function (twin) {
      if (twin === field || twin.tagName !== field.tagName) return;
      if (field.type === 'checkbox') twin.checked = field.checked;
      else if (twin.value !== field.value) {
        twin.value = field.value;
        autogrow(twin);
      }
    });
  }

  document.addEventListener('input', function (event) {
    const field = event.target.closest('[data-field]');
    if (field && isTyped(field)) {
      clearTimeout(field.sendTimer);
      field.sendTimer = setTimeout(function () { sendField(field); }, 400);
    }
    if (field && field.matches('textarea.outline-text')) autogrow(field);
    if (field) copyToTwins(field);
  });

  document.addEventListener('change', function (event) {
    const field = event.target.closest('[data-field]');
    if (field) {
      copyToTwins(field);
      sendField(field);
    }
  });

  // Once a choice's code is changed, its choices offer Apply to All? (to give the same codes to the other
  // questions with these choices).
  document.addEventListener('input', function (event) {
    const field = event.target.closest && event.target.closest('[data-field="code_value"]');
    const table = field && field.closest('.choice-table');
    const apply = table && table.querySelector('.choice-apply');
    if (apply) apply.hidden = false;
  });

  // ------ A block's words, typed in its row ----------------------------------------------
  // The box grows with its words. Enter commits (Shift+Enter starts a new line in a block's words).
  // The same keys work in every field that offers suggestions.
  function autogrow(field) {
    if (field.tagName !== 'TEXTAREA' || !field.offsetParent) return;
    field.style.height = 'auto';
    field.style.height = field.scrollHeight + 'px';
  }

  document.addEventListener('keydown', function (event) {
    const field = event.target;
    if (!field.matches || !field.matches('textarea.outline-text, input[data-suggest]')) return;
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
      // what starts with the words typed comes first ("3" offers question 3 before question 13)
      const options = Array.from(list.options)
        .map(function (option) { return option.value; })
        .filter(function (value) { return value.toLowerCase() !== typed && (typed === '' || value.toLowerCase().indexOf(typed) !== -1); })
        .sort(function (a, b) {
          return (b.toLowerCase().indexOf(typed) === 0) - (a.toLowerCase().indexOf(typed) === 0);
        })
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
    if (field.matches && field.matches('[data-suggest]') && !field.readOnly) {
      autogrow(field);
      suggest.show(field);
    }
  });
  document.addEventListener('input', function (event) {
    if (event.target.matches && event.target.matches('[data-suggest]')) suggest.show(event.target);
  });
  document.addEventListener('focusout', function (event) {
    if (event.target.matches && event.target.matches('[data-suggest]')) setTimeout(function () { suggest.hide(); }, 150);
    // a box sized while focused is sized again once it isn't, so its words sit in the middle of its row
    if (event.target.matches && event.target.matches('textarea.outline-text')) {
      const field = event.target;
      setTimeout(function () { autogrow(field); });
    }
  });

  // ------ Marks the server sends as words are typed ---------------------------------------
  // The outline is only redrawn when its layout changes (drawing it takes seconds), so the selected row,
  // the closed sections and open question panels, the checked blocks, each block's problems, the fields
  // at fault (outlined in red) and the uploaded MATR IDs still free come separately (outline_marks() in
  // R), and are put on again whenever Shiny redraws something. Until the first marks come, the classes
  // the outline was drawn with stay as they are.
  let marks = {
    selected: null, problems: [], fields: [], matr_ids: null, matr_list: null, labels: null, label_list: null,
    collapsed: null, checked: null, all_checked: null, check_titles: null, changed: null, matr: {}
  };

  // Closed sections, open question panels, checked blocks, and the MATR IDs a question's count lists.
  function applyStateMarks(node, row) {
    const id = row.dataset.id;
    if (marks.collapsed) {
      const closed = marks.collapsed.has(id);
      node.classList.toggle('is-collapsed', closed);
      if (row.hasAttribute('aria-expanded')) row.setAttribute('aria-expanded', closed ? 'false' : 'true');
    }
    if (marks.checked) row.classList.toggle('is-verified', marks.checked.has(id)); // green once verified
    const count = row.querySelector(':scope > .outline-badges > .outline-count');
    if (count) count.dataset.matr = marks.matr[id] || '';
    const check = row.querySelector(':scope > .outline-actions > .outline-check');
    if (!check || !marks.checked) return;
    const checked = marks.checked.has(id);
    check.classList.toggle('is-checked', checked);
    check.classList.toggle('is-all-checked', marks.all_checked.has(id));
    if (marks.check_titles) check.title = checked ? marks.check_titles.checked : marks.check_titles.unchecked;
  }

  // A field at fault: {table, id, field, title}; an empty field means its whole row (a variable card).
  function fieldSelector(mark) {
    if (!mark.field) return '.variable-card[data-id="' + CSS.escape(mark.id) + '"]';
    return '[data-table="' + CSS.escape(mark.table) + '"][data-id="' + CSS.escape(mark.id) + '"][data-field="' + CSS.escape(mark.field) + '"]';
  }

  function applyFieldMarks(tree) {
    const titles = new Map();
    marks.fields.forEach(function (mark) {
      tree.querySelectorAll(fieldSelector(mark)).forEach(function (el) {
        titles.set(el, (titles.has(el) ? titles.get(el) + '\n' : '') + mark.title);
      });
    });
    tree.querySelectorAll('.has-problem').forEach(function (el) {
      if (!titles.has(el)) {
        el.classList.remove('has-problem');
        el.removeAttribute('aria-invalid');
        el.removeAttribute('title');
      }
    });
    titles.forEach(function (title, el) {
      el.classList.add('has-problem');
      el.title = title;
      if (el.matches('[data-field]')) el.setAttribute('aria-invalid', 'true');
    });
  }

  // Every MATR ID field offers the uploaded IDs no variable has yet, and every parent-question field the
  // questions. A list is only replaced when what it offers has changed, and all at once: a browser works
  // out the suggestions of the field being typed in again for every change to its list, which took
  // seconds an option at a time.
  const shownLists = new WeakMap();
  function fillList(id, values) {
    const list = id && values ? document.getElementById(id) : null;
    if (!list) return;
    const offered = values.join('\n');
    if (shownLists.get(list) === offered) return;
    const options = document.createDocumentFragment();
    values.forEach(function (value) {
      const option = document.createElement('option');
      option.value = value;
      options.appendChild(option);
    });
    list.replaceChildren(options);
    shownLists.set(list, offered);
  }

  function applyMatrIds() {
    fillList(marks.matr_list, marks.matr_ids);
    fillList(marks.label_list, marks.labels);
  }

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
      applyStateMarks(node, row);
      if (row.dataset.id === marks.selected && !row.classList.contains('is-selected')) {
        row.classList.add('is-selected');
        row.scrollIntoView({ block: 'nearest' });
      }
      const badge = row.querySelector('.outline-problems');
      if (!badge) return;
      const problem = byId[row.dataset.id];
      showProblems(badge, problem);
    });
    applyFieldMarks(tree.closest('.outline-panel') || tree);
    applyDetailMarks();
    applyMatrIds();
    applyFilter(tree);
    sizeFreshBoxes(); // the boxes of sections just opened
  }

  // A badge of what there is to fix: a warning sign and the number, which lists the problems when pointed at.
  function showProblems(badge, problem) {
    badge.classList.toggle('is-hidden', !problem);
    badge.dataset.problems = problem ? problem.title : '';
    badge.setAttribute('aria-label', (problem ? problem.n : 0) + ' to fix');
    const count = badge.querySelector('.problem-count');
    if (count) count.textContent = problem ? problem.n : 0;
  }

  // The picked block's details: its Verify button (which waits until nothing is left to fix) and its badge of
  // what there is to fix, kept up to date as the row's are.
  function applyDetailMarks() {
    const detail = document.querySelector('.block-detail');
    if (!detail) return;
    const id = detail.dataset.id;
    const problem = marks.problems.find(function (each) { return each.id === id; });
    const verify = detail.querySelector('.detail-verify');
    if (verify && marks.checked) {
      const checked = marks.checked.has(id);
      verify.classList.toggle('is-checked', checked);
      verify.disabled = !checked && !!problem;
      const label = verify.querySelector('.verify-label');
      if (label) label.textContent = checked ? 'Verified' : 'Verify';
      if (marks.check_titles) verify.title = checked ? marks.check_titles.checked : marks.check_titles.unchecked;
    }
    const badge = detail.querySelector('.detail-head .outline-problems');
    if (badge) showProblems(badge, problem);
  }

  // Add Parent and Add Child show their field, with the cursor in it.
  document.addEventListener('click', function (event) {
    const item = event.target.closest && event.target.closest('[data-reveal]');
    const detail = item && item.closest('.block-detail');
    const field = detail && detail.querySelector(item.dataset.reveal);
    if (!field) return;
    field.hidden = false;
    const input = field.querySelector('input');
    if (input) input.focus();
  });

  // ------ Which blocks are shown ------------------------------------------------------
  // The toolbar's pick (outline_filter() in R): all blocks, those not checked, those with something to fix,
  // or those changed from what the AI read. A section or question group stays while anything in it is shown,
  // and is open while the filter is on. Blocks are hidden, not taken out, so nothing is drawn again.
  // The toolbar's search narrows it further: blocks whose words, numbers or variables in short have what is typed.
  let filter = 'all';
  let search = '';
  function shownByFilter(id, toFix) {
    if (filter === 'unchecked') return !(marks.checked && marks.checked.has(id));
    if (filter === 'problems') return toFix.has(id);
    if (filter === 'changed') return !!(marks.changed && marks.changed.has(id));
    return true;
  }
  function shownBySearch(node) {
    if (!search) return true;
    const row = node.querySelector(':scope > .outline-row');
    if (!row) return false;
    const words = [];
    row.querySelectorAll(':scope > .outline-text, :scope > .outline-numbers > *, :scope > .outline-row-summary > *').forEach(function (el) {
      words.push(el.matches('input, textarea') ? el.value : el.textContent);
    });
    return words.join(' ').toLowerCase().indexOf(search) !== -1;
  }
  function applyFilter(tree) {
    const picker = document.querySelector('[data-outline-filter]');
    if (picker) {
      const changed = picker.querySelector('option[value="changed"]');
      // a draft typed by hand has no AI reading to compare with
      if (changed) changed.disabled = !marks.changed;
      if (filter === 'changed' && !marks.changed) filter = picker.value = 'all';
    }
    const on = filter !== 'all' || search !== '';
    const toFix = new Set(marks.problems.map(function (problem) { return problem.id; }));
    tree.classList.toggle('is-filtered', on);
    let any = false;
    // the deepest blocks first, so a section knows whether anything in it is shown
    Array.from(tree.querySelectorAll('.outline-node')).reverse().forEach(function (node) {
      const shown = !on || (shownByFilter(node.dataset.id, toFix) && shownBySearch(node)) ||
        !!node.querySelector(':scope > .outline-list > .outline-node:not(.is-filtered-out)');
      node.classList.toggle('is-filtered-out', !shown);
      any = any || shown;
    });
    const body = tree.closest('.outline-body');
    if (body) body.classList.toggle('is-filter-empty', on && !any);
  }
  document.addEventListener('change', function (event) {
    if (!event.target.matches || !event.target.matches('[data-outline-filter]')) return;
    filter = event.target.value;
    const tree = document.querySelector('.outline-tree');
    if (tree) applyFilter(tree);
  });
  document.addEventListener('input', function (event) {
    if (!event.target.matches || !event.target.matches('[data-outline-search]')) return;
    search = event.target.value.trim().toLowerCase();
    const tree = document.querySelector('.outline-tree');
    if (tree) applyFilter(tree);
  });

  // ------ On a phone the list and the details take turns -----------------------------------
  // Picking a block shows its details, and Back the list.
  document.addEventListener('click', function (event) {
    const target = event.target.closest && event.target.closest('[data-detail-back], .outline-panel [data-action="select"], .outline-panel [data-action="details"]');
    if (!target) return;
    const panel = target.closest('.outline-panel');
    if (!panel) return;
    if (target.matches('[data-detail-back]')) panel.classList.remove('is-detail-open');
    else panel.classList.add('is-detail-open');
  });

  // ------ The PDF, in its own tab ----------------------------------------------------
  // The header's Open PDF opens it at the page the picked block was read from (each outline node's data-page),
  // which the browser's PDF viewer reads from the address. Drafts read before pages were noted open at the start.
  document.addEventListener('click', function (event) {
    const link = event.target.closest && event.target.closest('[data-pdf-at-picked]');
    if (!link) return;
    const node = marks.selected && document.querySelector('.outline-node[data-id="' + CSS.escape(marks.selected) + '"]');
    const page = node ? node.dataset.page : null;
    link.href = link.href.split('#')[0] + (page ? '#page=' + page : '');
  });

  // ------ What there is to fix, listed as it is pointed at ---------------------------------
  // A row's "N to fix" badge lists that block's problems, and the toolbar's "N blocks to fix" every block's,
  // each a link to its block. A question's count of variables lists their MATR IDs the same way (not in red).
  // The list is made only when it is shown, from what the marks already hold.
  const problemPop = {
    box: null,
    owner: null,
    timer: null,

    items: function (owner) {
      if (owner.matches('.outline-count')) {
        return (owner.dataset.matr || '').split('\n').filter(Boolean).map(function (text) { return { text: text }; });
      }
      if (owner.matches('.outline-problems')) {
        return (owner.dataset.problems || '').split('\n').filter(Boolean).map(function (text) { return { text: text }; });
      }
      return marks.problems.map(function (problem) {
        const node = nodeOf(problem.id);
        const label = node && node.dataset.label ? node.dataset.label.charAt(0).toUpperCase() + node.dataset.label.slice(1) : 'A block';
        return { id: problem.id, text: label + ': ' + problem.title.split('\n').join(' ') };
      });
    },

    show: function (owner) {
      const items = this.items(owner);
      const panel = owner.closest('.outline-panel');
      if (items.length === 0 || !panel) return;
      clearTimeout(this.timer);
      if (!this.box) {
        this.box = document.createElement('div');
        this.box.className = 'problem-pop';
        this.box.setAttribute('role', 'tooltip');
        this.box.addEventListener('mouseenter', function () { clearTimeout(problemPop.timer); });
        this.box.addEventListener('mouseleave', function () { problemPop.hideSoon(); });
        this.box.addEventListener('click', function (event) { if (event.target.closest('[data-action]')) problemPop.hide(); });
      }
      if (this.box.parentElement !== panel) panel.appendChild(this.box); // inside the module, so a link's click reaches it
      this.box.classList.toggle('is-info', owner.matches('.outline-count'));
      const list = document.createElement('ul');
      items.slice(0, 40).forEach(function (item) {
        const li = document.createElement('li');
        if (item.id) {
          const link = document.createElement('button');
          link.type = 'button';
          link.className = 'problem-pop-link';
          link.dataset.action = 'select';
          link.dataset.id = item.id;
          link.textContent = item.text;
          li.appendChild(link);
        } else {
          li.textContent = item.text;
        }
        list.appendChild(li);
      });
      if (items.length > 40) {
        const more = document.createElement('li');
        more.className = 'problem-pop-more';
        more.textContent = 'and ' + (items.length - 40) + ' more';
        list.appendChild(more);
      }
      this.box.replaceChildren(list);
      const rect = owner.getBoundingClientRect();
      this.box.style.left = Math.max(8, Math.min(rect.left, window.innerWidth - 400)) + 'px';
      this.box.style.top = (rect.bottom + 4) + 'px';
      this.box.style.display = 'block';
      this.owner = owner;
      owner.setAttribute('aria-describedby', 'problem-pop');
      this.box.id = 'problem-pop';
    },

    hideSoon: function () {
      clearTimeout(this.timer);
      this.timer = setTimeout(function () { problemPop.hide(); }, 200);
    },

    hide: function () {
      clearTimeout(this.timer);
      if (this.box) this.box.style.display = 'none';
      if (this.owner) this.owner.removeAttribute('aria-describedby');
      this.owner = null;
    }
  };

  const problemOwners = '.outline-problems:not(.is-hidden), .to-fix-note:not(.is-clear), .outline-count[data-matr]:not([data-matr=""])';
  document.addEventListener('mouseover', function (event) {
    const owner = event.target.closest && event.target.closest(problemOwners);
    if (owner && owner !== problemPop.owner) problemPop.show(owner);
  });
  document.addEventListener('mouseout', function (event) {
    const owner = event.target.closest && event.target.closest(problemOwners);
    if (owner && !(event.relatedTarget && owner.contains(event.relatedTarget))) problemPop.hideSoon();
  });
  document.addEventListener('focusin', function (event) {
    if (event.target.matches && event.target.matches(problemOwners)) problemPop.show(event.target);
  });
  document.addEventListener('focusout', function (event) {
    if (event.target.matches && event.target.matches(problemOwners)) problemPop.hideSoon();
  });
  document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape') problemPop.hide();
  });
  window.addEventListener('scroll', function (event) {
    if (!(problemPop.box && problemPop.box.contains(event.target))) problemPop.hide(); // the page moved, not the list
  }, true);

  // ------ Changes to the layout, put in place -------------------------------------------
  // When blocks are added, deleted, moved or picked from a list, only what is different is drawn
  // (outline_patch() in R): blocks to take out and to add, rows and open question panels drawn again,
  // every list's blocks in order, and the numbers, display orders and labels that follow from the order.
  function nodeOf(id) {
    return document.querySelector('.outline-tree .outline-node[data-id="' + CSS.escape(id) + '"]');
  }

  function listOf(parent) {
    if (!parent) return document.querySelector('.outline-tree > .outline-list');
    const node = nodeOf(parent);
    return node ? node.querySelector(':scope > .outline-list') : null;
  }

  // Draws `html` in place of `old` (or at the end of `parent`, if there is no `old`).
  function drawInPlace(old, parent, html) {
    if (old) {
      Shiny.renderContent(old, { html: html, deps: [] }, 'beforeBegin');
      old.remove();
    } else if (parent) {
      Shiny.renderContent(parent, { html: html, deps: [] }, 'beforeEnd');
    }
  }

  // Each list holds its blocks in the draft's order; a block that is elsewhere is moved, not drawn again.
  function putInOrder(lists) {
    lists.forEach(function (list) {
      const el = listOf(list.parent);
      if (!el) return;
      const now = Array.from(el.children).filter(function (child) { return child.matches('.outline-node'); })
        .map(function (child) { return child.dataset.id; });
      if (now.join('\n') === list.ids.join('\n')) return;
      list.ids.forEach(function (id) {
        const node = nodeOf(id);
        if (node) el.appendChild(node);
      });
    });
  }


  function applyPatch(patch) {
    patch.insert.forEach(function (block) { drawInPlace(null, listOf(block.parent), block.html); });
    putInOrder(patch.lists);
    patch.remove.forEach(function (id) {
      const node = nodeOf(id);
      if (node) node.remove();
    });
    patch.rows.forEach(function (row) {
      const node = nodeOf(row.id);
      if (node) drawInPlace(node.querySelector(':scope > .outline-row'), null, row.html);
    });
    document.querySelectorAll('.outline-tree .outline-node').forEach(function (node) {
      const id = node.dataset.id;
      const row = node.querySelector(':scope > .outline-row');
      if (patch.labels[id] !== undefined) {
        node.dataset.label = patch.labels[id];
        const list = node.querySelector(':scope > .outline-list');
        if (list) list.dataset.label = patch.labels[id];
      }
      const order = row && row.querySelector(':scope > .outline-order');
      if (order && patch.orders[id] !== undefined) order.textContent = patch.orders[id];
      const number = row && row.querySelector(':scope > .outline-numbers > .outline-number');
      if (number && patch.numbers[id] !== undefined) number.textContent = patch.numbers[id];
    });
    fillList(patch.parent_list, patch.questions);
    fillList(patch.skip_list, patch.skip_targets);
    marks.selected = patch.selected || null;
    afterDrawing();
  }

  // Put the marks back, size the text boxes just drawn, and give a block that has no words yet the cursor.
  // The boxes are sized all at once (every height reset, then every height read, then set): one box at a
  // time made the browser lay the page out again for each.
  // Text boxes not sized yet are sized all at once. One in a closed section has no height to read, so it waits
  // until its section opens (applyMarks() comes back here).
  function sizeFreshBoxes() {
    sizeBoxes(document.querySelectorAll('textarea.outline-text:not([style*="height"])'));
  }
  function sizeBoxes(fields) {
    const shown = Array.from(fields).filter(function (field) { return field.offsetParent; });
    shown.forEach(function (field) { field.style.height = 'auto'; });
    const heights = shown.map(function (field) { return field.scrollHeight; });
    shown.forEach(function (field, i) { field.style.height = heights[i] + 'px'; });
  }

  // A window made wider or narrower wraps the words differently, so the boxes shown are sized again once it stops.
  let resizeTimer = null;
  let lastWidth = window.innerWidth;
  window.addEventListener('resize', function () {
    if (window.innerWidth === lastWidth) return;
    lastWidth = window.innerWidth;
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function () { sizeBoxes(document.querySelectorAll('textarea.outline-text')); }, 200);
  });

  function afterDrawing() {
    applyMarks();
    const emptyWords = document.querySelector('.block-detail [data-autofocus]');
    if (emptyWords && !emptyWords.value && document.activeElement !== emptyWords) emptyWords.focus();
    // an optional field just added to a variable gets the cursor
    const added = document.querySelector('.outline-panel [data-focus-now]');
    if (added) {
      added.removeAttribute('data-focus-now');
      added.focus();
    }
  }

  function register() {
    Shiny.addCustomMessageHandler('edit-extraction-patch', applyPatch);
    // giving out MATR IDs by hand opens every question's variables, which is the outline's
    // giving out MATR IDs by hand shows each question's variables in short under its row
    Shiny.addCustomMessageHandler('edit-extraction-assign', function (on) {
      const panel = document.querySelector('.outline-panel[data-ns]');
      if (panel) panel.classList.toggle('is-assigning', !!on);
    });
    Shiny.addCustomMessageHandler('edit-extraction-marks', function (sent) {
      const set = function (ids) { return ids ? new Set(ids) : null; };
      marks = {
        selected: sent.selected || null, problems: sent.problems || [], fields: sent.fields || [],
        matr_ids: sent.matr_ids || [], matr_list: sent.matr_list || null,
        labels: sent.labels || [], label_list: sent.label_list || null,
        collapsed: set(sent.collapsed), checked: set(sent.checked),
        all_checked: set(sent.all_checked) || new Set(), check_titles: sent.check_titles || null,
        changed: set(sent.changed), matr: sent.matr && !Array.isArray(sent.matr) ? sent.matr : {}
      };
      applyMarks();
    });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) register();
  else if (window.jQuery) jQuery(document).one('shiny:connected', register);

  // ------ After Shiny redraws the outline -----------------------------------------------
  // Only the outline's own redraw: the page's other outputs (the progress bar, the count to fix) change as
  // words are typed, and sizing every text box again each time made typing stutter.
  $(document).on('shiny:value', function (event) {
    if (event.name && !/outline-(tree|detail)$/.test(event.name)) return;
    setTimeout(afterDrawing);
  });

  // ------ Leaving the page -----------------------------------------------------------
  window.addEventListener('beforeunload', function (event) {
    if (document.querySelector('.save-status.is-dirty')) {
      event.preventDefault();
      event.returnValue = '';
    }
  });

  // ------ Drag and drop ---------------------------------------------------------------
  window.editExtractionPage = {
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
      send(evt.to, 'reorder', withShared(evt.to, { table: evt.to.dataset.table, ids: ids }));
    }
  };
})();
