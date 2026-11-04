# Make an edit's kinds in the group Registries and studies: registries, their investigators and studies.
# The kinds are listed together in edit_entities (service_edit_kinds.R); the pieces their forms are built from
# are in service_edit_entities.R.

# ------ Registries and studies ---------------------------------------------------------------
registry_entity <- list(
  label = "Registry", noun = "registry", plural = "registries", article = "a",
  group = "Registries and studies", table = "registry",
  about = "The outermost container: every study, questionnaire and variable belongs to one registry.",
  delete_about = paste0(
    "A registry holds studies, and they hold everything else. Deleting it deletes every study in ",
    "it, with all their questionnaires, waves and variables."
  ),
  cards = function(snap, scope) {
    rows <- snap$registry
    n_studies <- count_in(snap$study$registry_id, rows$registry_id)
    edit_cards(
      key_text(rows$registry_id), rows$name,
      subtitle = paste0(rows$abbreviation, " · ", format(rows$participant_count, big.mark = ",", trim = TRUE), " participants"),
      facts = lapply(n_studies, function(n) edit_count(n, "study", "studies"))
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("name", "Name",
        required = TRUE, placeholder = "e.g. Mid-Atlantic Twin Registry",
        problem = function(value, values) {
          same <- edit_taken(snap, "registry", "name", value, row)
          if (has_text(value) && nrow(same) > 0) sprintf("There's already a registry called “%s”.", same$name[1])
        }
      ),
      edit_field("abbreviation", "Abbreviation", required = TRUE, placeholder = "e.g. MATR"),
      edit_field("participant_count", "Participants", "number",
        required = TRUE, min = 0, placeholder = "e.g. 50,000",
        hint = "How many people are registered."
      ),
      edit_field("description", "Description", "textarea")
    )
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "registry", "name", values$name, "A registry with this name")
    ))
  }
)

investigator_entity <- list(
  label = "Investigator", noun = "investigator", plural = "investigators", article = "an",
  group = "Registries and studies", table = "principal_investigator",
  about = "A researcher who leads a study or is its contact.",
  delete_about = paste0(
    "An investigator can only be deleted once they no longer lead a study. Where they're a study's ",
    "contact, the contact is simply left empty."
  ),
  cards = function(snap, scope) {
    rows <- snap$principal_investigator
    leads <- count_in(snap$study$principal_investigator_id, rows$principal_investigator_id)
    contact <- count_in(snap$study$contact_principal_investigator_id, rows$principal_investigator_id)
    edit_cards(
      key_text(rows$principal_investigator_id), rows$name,
      subtitle = rows$email,
      facts = Map(function(l, c) {
        c(
          if (l > 0) paste("leads", edit_count(l, "study", "studies")),
          if (c > 0) paste("contact for", edit_count(c, "study", "studies")),
          if (l + c == 0) "not on any study"
        )
      }, leads, contact)
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("name", "Name", required = TRUE),
      edit_field("email", "Email",
        required = TRUE, placeholder = "name@vcu.edu",
        problem = function(value, values) {
          if (has_text(value) && !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", trimws(value))) {
            "Type a full email address, like name@vcu.edu."
          }
        }
      )
    )
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "principal_investigator", "name", values$name, "An investigator with this name"),
      edit_duplicate_rows(snap, "principal_investigator", "email", values$email, "An investigator with this email")
    ))
  }
)

# Study status is Active or Ended (study_status_check in the database).
study_statuses <- c("Active" = "Active", "Ended" = "Ended")
study_status_tooltip <- "Active: the study is ongoing. Ended: all data collections are finished."

study_entity <- list(
  label = "Study", noun = "study", plural = "studies", article = "a",
  group = "Registries and studies", table = "study",
  about = "One research project run under a registry.",
  delete_about = paste0(
    "Deleting a study deletes all its questionnaires, with their waves and variables. Registries ",
    "and investigators are not affected."
  ),
  cards = function(snap, scope) {
    rows <- snap$study
    n_q <- count_in(snap$questionnaire$study_id, rows$study_id)
    registry <- edit_lookup(snap, "registry", "registry_id", rows$registry_id, "abbreviation")
    lead <- edit_lookup(snap, "principal_investigator", "principal_investigator_id", rows$principal_investigator_id, "name")
    edit_cards(
      key_text(rows$study_id), rows$name,
      subtitle = paste0(
        registry, ifelse(is.na(lead), "", paste0(" · led by ", lead)), ifelse(is.na(rows$status), "", paste0(" · ", rows$status))
      ),
      facts = lapply(n_q, function(n) edit_count(n, "questionnaire"))
    )
  },
  fields = function(snap, mode, row, scope) {
    investigators <- edit_choices(snap, "principal_investigator", blank = "Choose an investigator")
    list(
      edit_field("registry_id", "Registry", "select",
        required = TRUE,
        choices = edit_choices(snap, "registry", blank = "Choose a registry")
      ),
      edit_field("principal_investigator_id", "Lead investigator", "select",
        choices = investigators,
        # one can be picked later, if none was when the study was added
        locked = if (!is.null(row) && !is.na(row$principal_investigator_id)) "A study's lead investigator never changes once it's recorded."
      ),
      edit_field("contact_principal_investigator_id", "Contact investigator", "select",
        choices = investigators,
        hint = "Who to contact about this study."
      ),
      edit_field("name", "Name",
        required = TRUE, placeholder = "e.g. Adult Health Study",
        problem = function(value, values) {
          if (!has_text(value) || !has_text(values$registry_id)) {
            return(NULL)
          }
          same <- edit_taken(snap, "study", "name", value, row, within = list(registry_id = as.numeric(values$registry_id)))
          if (nrow(same) > 0) sprintf("This registry already has a study called “%s”.", same$name[1])
        }
      ),
      edit_field("status", "Status", "select",
        choices = c("Choose a status" = "", study_statuses), tooltip = study_status_tooltip
      ),
      edit_field("description", "Description", "textarea"),
      edit_field("methods", "Methods", "textarea"),
      edit_field("acknowledgements", "Acknowledgements", "textarea")
    )
  },
  duplicates = function(snap, values) {
    registry <- if (has_text(values$registry_id)) list(registry_id = as.numeric(values$registry_id))
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "study", "name", values$name, "A study with this name", within = registry)
    ))
  }
)
