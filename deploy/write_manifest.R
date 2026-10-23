# Writes manifest.json, which Posit Connect reads to deploy the site: the files the site runs from, and the
# packages it needs at the versions in renv.lock. Run it from the project folder whenever a package or a file
# is added, removed or renamed, and commit manifest.json with that change:
#
#   Rscript deploy/write_manifest.R
#
# The tests, docs and local files (.env, uploads/, logs/) aren't part of the site, so they aren't listed. On
# Connect the keys come from the content's environment variables, not from .env.
app_files <- c(
  "global.R", "ui.R", "server.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE),
  list.files("prompts", full.names = TRUE)
)
rsconnect::writeManifest(appFiles = app_files)
