// ------ A question's variables, edited on Make an edit ---------------------------------------------
// Adding a question to a questionnaire asks for its variables and their choices, which a form of single
// fields can't hold. The form has one field for them (R: edit_variables_ui()), a hidden text box whose
// value is the variables as JSON:
//   [{variable_type, matr_id, subject, label, answer_data_type,
//     response_options: [{code_value, answer_option, printed_label, skip_instruction}]}]
// This draws an editor from it, like the Edit Extraction page's variable cards, and writes the JSON back
// into the text box as it is changed, so Shiny reads it like any other field. Who a variable can be about
// is typed (blank is the respondent), with the subjects in the editor's data-subjects offered; the kinds of
// answer come from data-answer-types; the answer choices it offers as they are typed are in the <datalist>
// data-answers names.
(function () {
  'use strict';

  function parse(text) {
    try {
      const value = JSON.parse(text || '[]');
      return Array.isArray(value) ? value : [];
    } catch (e) {
      return [];
    }
  }

  // An element, with attributes (a function for on* is an event handler) and children.
  function make(tag, attributes, children) {
    const el = document.createElement(tag);
    Object.keys(attributes || {}).forEach(function (name) {
      const value = attributes[name];
      if (value === null || value === undefined || value === false) return;
      if (name.indexOf('on') === 0 && typeof value === 'function') el.addEventListener(name.slice(2), value);
      else if (name === 'value') el.value = value;
      else el.setAttribute(name, value === true ? '' : value);
    });
    (children || []).forEach(function (child) {
      if (child === null || child === undefined) return;
      el.appendChild(typeof child === 'string' ? document.createTextNode(child) : child);
    });
    return el;
  }

  function icon(name) {
    return make('i', { class: 'fas fa-' + name, role: 'presentation', 'aria-hidden': 'true' });
  }

  function text(value) {
    return value === null || value === undefined ? '' : String(value);
  }

  // The next code in a variable's choices: one more than the highest number there.
  function nextCode(choices) {
    const numbers = choices.map(function (choice) { return parseInt(choice.code_value, 10); })
      .filter(function (n) { return !isNaN(n); });
    return String(numbers.length ? Math.max.apply(null, numbers) + 1 : 1);
  }

  function blankChoice(choices) {
    return { code_value: nextCode(choices || []), answer_option: '', printed_label: '', skip_instruction: '' };
  }

  function start(editor) {
    if (editor.dataset.ready) return;
    const input = document.getElementById(editor.dataset.input);
    const list = editor.querySelector('.edit-variables-list');
    const addButton = editor.querySelector('.edit-variables-add');
    if (!input || !list || !addButton) return;
    editor.dataset.ready = 'true';

    const subjects = parse(editor.dataset.subjects);
    const answerTypes = parse(editor.dataset.answerTypes);
    const subjectList = editor.dataset.input + '-subjects';
    editor.appendChild(make('datalist', { id: subjectList }, subjects.map(function (subject) {
      return make('option', { value: subject });
    })));
    let variables = parse(input.value);
    let timer = null;

    // Words are sent once typing pauses; anything that changes what is drawn is sent, and drawn, at once.
    function save(redraw) {
      clearTimeout(timer);
      const send = function () {
        input.value = JSON.stringify(variables);
        jQuery(input).trigger('change');
      };
      if (redraw) {
        send();
        draw();
      } else {
        timer = setTimeout(send, 400);
      }
    }

    function textField(owner, field, placeholder, extra) {
      return make('input', Object.assign({
        type: 'text', class: 'form-control form-control-sm', value: text(owner[field]),
        placeholder: placeholder, 'aria-label': placeholder, title: placeholder,
        oninput: function (event) { owner[field] = event.target.value; save(false); },
        onchange: function (event) { owner[field] = event.target.value; save(false); }
      }, extra || {}));
    }

    function selectField(owner, field, choices, blank, label, redraw) {
      const current = text(owner[field]);
      const options = [make('option', { value: '' }, [blank])].concat(choices.map(function (choice) {
        const value = Array.isArray(choice) ? choice[1] : choice;
        const shown = Array.isArray(choice) ? choice[0] : choice;
        return make('option', { value: value, selected: value === current }, [shown]);
      }));
      return make('select', {
        class: 'form-select form-select-sm', 'aria-label': label, title: label,
        onchange: function (event) {
          owner[field] = event.target.value;
          // a variable that becomes one with choices starts with a blank one, if it has none
          if (field === 'variable_type' && owner[field] !== 'open_ended' && !(owner.response_options || []).length) {
            owner.response_options = [blankChoice([])];
          }
          save(redraw);
        }
      }, options);
    }

    function removeButton(label, onclick) {
      return make('button', { type: 'button', class: 'btn btn-sm btn-icon', title: label, 'aria-label': label, onclick: onclick }, [icon('trash-can')]);
    }

    function choiceRow(variable, choice, j) {
      return make('div', { class: 'edit-choice-row' }, [
        textField(choice, 'code_value', 'Code', { type: 'number', class: 'form-control form-control-sm edit-choice-code' }),
        textField(choice, 'answer_option', 'Response option', { list: editor.dataset.answers }),
        textField(choice, 'printed_label', 'Printed as (optional)'),
        textField(choice, 'skip_instruction', 'Skip to (optional)'),
        removeButton('Delete this choice', function () {
          variable.response_options.splice(j, 1);
          save(true);
        })
      ]);
    }

    function card(variable, i) {
      const closed = variable.variable_type !== 'open_ended';
      if (closed && !Array.isArray(variable.response_options)) variable.response_options = [];
      const row = make('div', { class: 'edit-variable-row' }, [
        make('span', { class: 'edit-variable-number' }, [String(i + 1)]),
        selectField(variable, 'variable_type', [['Choices', 'closed_ended'], ['Write-in', 'open_ended']], 'Kind', 'Kind', true),
        textField(variable, 'matr_id', 'MATR ID'),
        textField(variable, 'subject', 'Respondent', { list: subjectList, 'aria-label': 'About (blank for the respondent)', title: 'About (blank for the respondent)' }),
        textField(variable, 'label', 'Label (optional)'),
        closed ? null : selectField(variable, 'answer_data_type', answerTypes, 'Select an answer type', 'Answer type', false),
        removeButton('Delete this variable', function () {
          variables.splice(i, 1);
          save(true);
        })
      ]);
      const choices = closed ? make('div', { class: 'edit-choices' }, variable.response_options.map(function (choice, j) {
        return choiceRow(variable, choice, j);
      }).concat([
        make('button', {
          type: 'button', class: 'btn btn-sm btn-outline-dark edit-choice-add',
          onclick: function () {
            variable.response_options.push(blankChoice(variable.response_options));
            save(true);
          }
        }, [icon('plus'), ' Choice'])
      ])) : null;
      return make('div', { class: 'edit-variable' }, [row, choices]);
    }

    function draw() {
      list.innerHTML = '';
      if (variables.length === 0) list.appendChild(make('p', { class: 'form-text' }, ['No variables yet.']));
      variables.forEach(function (variable, i) {
        if (variable.variable_type === 'closed_ended' || !variable.variable_type) variable.variable_type = 'closed_ended';
        list.appendChild(card(variable, i));
      });
    }

    // A new variable is like the last one (its kind, who it's about and its choices), without a MATR ID.
    addButton.addEventListener('click', function () {
      const last = variables[variables.length - 1];
      const copy = last ? JSON.parse(JSON.stringify(last)) : { variable_type: 'closed_ended', subject: '' };
      copy.matr_id = '';
      copy.label = '';
      if (copy.variable_type !== 'open_ended' && (!copy.response_options || copy.response_options.length === 0)) {
        copy.response_options = [blankChoice([])];
      }
      variables.push(copy);
      save(true);
    });

    draw();
  }

  function startAll() {
    document.querySelectorAll('.edit-variables').forEach(start);
  }

  // The form is drawn by Shiny, so editors are looked for whenever it draws something.
  jQuery(document).on('shiny:value', function () { setTimeout(startAll); });
  jQuery(startAll);
})();
