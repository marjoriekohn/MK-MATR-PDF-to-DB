// ------ The questionnaire table in the Push dialog: its toolbar -------------------------------------------
// The table (R: questionnaire_preview_ui()) is a DataTables table with a toolbar of the site's own above it:
//   * All / New / Already in the database shows every row, the rows with something the push adds, or those with
//     nothing new (each row's status is in a hidden column: "new", "found", or "" for a block that isn't a
//     question when the catalog's couldn't be read, which only All shows)
//   * Search narrows the rows to those with the words typed
//   * Download CSV presses DataTables' own CSV button, which is hidden
// The table calls caraPreviewTable() once it is drawn (its `callback`); it is drawn again whenever the push is
// tried again, and the toolbar with it, so each drawing is wired anew and starts with every row.
(function () {
  'use strict';

  // DataTables asks every search function about every table's rows: this one only narrows a table whose toolbar
  // picked New or Already in the database (data-qt-filter on its <table>).
  let filterAdded = false;
  function addFilter() {
    if (filterAdded || !window.jQuery || !jQuery.fn.dataTable) return;
    filterAdded = true;
    jQuery.fn.dataTable.ext.search.push(function (settings, data, index) {
      const node = settings.nTable;
      const want = node && node.dataset.qtFilter;
      if (!want || want === 'all') return true;
      const row = settings.aoData[index] && settings.aoData[index]._aData;
      return !!row && row[Number(node.dataset.qtStatusColumn)] === want;
    });
  }

  window.caraPreviewTable = function (table) {
    addFilter();
    const node = table.table().node();
    const box = node.closest('.questionnaire-preview');
    if (!box) return;
    node.dataset.qtStatusColumn = box.dataset.statusColumn;
    node.dataset.qtFilter = 'all';

    box.querySelectorAll('input[data-qt-filter]').forEach(function (choice) {
      choice.addEventListener('change', function () {
        if (!choice.checked) return;
        node.dataset.qtFilter = choice.value;
        table.draw();
      });
    });

    const search = box.querySelector('.qt-search');
    if (search) {
      search.addEventListener('input', function () { table.search(search.value).draw(); });
    }

    const download = box.querySelector('.qt-download');
    if (download) {
      download.addEventListener('click', function () { table.button(0).trigger(); });
    }
  };
})();
