# ------ Icons ---------------------------------------------------------------------------------
# Every icon the site shows, named for what it means, so one change here changes it everywhere (the page
# scripts that draw icons are given theirs from here too). They are Font Awesome's solid icons
# (https://fontawesome.com/icons), named as Font Awesome names them, without "fa-".

app_icons <- c(
  # doing things
  add = "circle-plus",
  delete = "trash",
  remove = "xmark", # a tag pill's ×
  edit = "pen-to-square",
  undo = "rotate-left",
  save = "floppy-disk",
  preview = "eye",
  help = "circle-info", # beside a field's name: pointing at it says what the field is for
  verify = "circle-check",
  ungroup = "object-ungroup",
  change_kind = "right-left",
  drag = "grip-vertical",
  open = "circle-arrow-right",
  back = "arrow-left",
  expand = "expand",
  collapse = "compress",
  filter = "filter",
  search = "magnifying-glass",
  reextract = "arrows-rotate",
  link = "link",
  unlink = "link-slash",
  merge = "code-merge", # two of the same thing made one
  visible = "fas fa-eye", # shown on the dashboard
  hidden = "fas fa-eye-slash", # kept off the dashboard
  duplicate = "fas fa-clone", # a copy of a block, right after it (written whole, like `view`: the solid one)
  revert = "fas fa-backward", # a block put back as the AI read it
  view = "fas fa-folder-open", # on a tag and after View: opens the thing named (written whole: the short name gives the outlined folder)
  # what something is
  question_number = "hashtag",
  matr_id = "fingerprint",
  pdf = "file-pdf",
  csv = "file-lines",
  choices = "list-ul",
  write_in = "keyboard",
  choices_open = "caret-down",
  choices_closed = "caret-right",
  new = "certificate",
  locked = "lock",
  # how things stand
  done = "check",
  problem = "triangle-exclamation",
  warning = "circle-exclamation",
  attention = "exclamation",
  info = "circle-info",
  # the menu, sign-in and the home page
  account = "circle-user",
  password = "key",
  email = "envelope",
  report = "bug",
  log_out = "right-from-bracket",
  upload = "file-arrow-up",
  drafts = "file-pen",
  metadata = "database",
  make_edit = "pencil",
  next_step = "arrow-right",
  history = "clock-rotate-left",
  ai_monitor = "chart-line",
  # the kinds of block, in Add Block
  section = "folder-open",
  question_group = "layer-group",
  question = "circle-question",
  introduction = "door-open",
  instruction = "list-ol",
  definition = "book",
  interviewer_script = "comment",
  note = "note-sticky",
  block = "square"
)

# An icon, by what it means.
# name: A name in `app_icons`, e.g. `"delete"`.
# ...: Passed to `shiny::icon()`, e.g. `class`.
app_icon <- function(name, ...) {
  if (!name %in% names(app_icons)) stop("No icon is named ", name, call. = FALSE)
  shiny::icon(app_icons[[name]], ...)
}

# A button's icon and words. On a phone only the icon shows (`.btn-text`, _page.scss); the words stay for
# screen readers.
# icon_name: A name in `app_icons`.
# text: The words.
icon_label <- function(icon_name, text) {
  tagList(app_icon(icon_name), span(class = "btn-text", text))
}
