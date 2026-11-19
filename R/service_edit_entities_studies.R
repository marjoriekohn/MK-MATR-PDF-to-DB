# Make an edit's kinds in the group Registries and studies: registries, their investigators and studies, and the
# studies' publications.
# The kinds are listed together in edit_entities (service_edit_kinds.R); the pieces their forms are built from
# are in service_edit_entities.R.

# ------ Registries and studies ---------------------------------------------------------------
registry_entity <- list(
  label = "Registry", noun = "registry", plural = "registries", article = "a",
  group = "Registries and studies", table = "registry",
  about = "The outermost container: every study, questionnaire and variable belongs to one registry.",
  delete_about = paste0(
    "A registry can only be deleted once it holds no studies. Empty it first (its contents go, the registry and ",
    "its studies stay), or delete its studies."
  ),
  empty_about = "Its studies, questionnaires and waves stay, with nothing printed or asked in them.",
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
    "An investigator can only be deleted once they lead no study. Where they're a study's ",
    "contact, the contact is simply left empty."
  ),
  alike = function(snap, row) snap$principal_investigator,
  cards = function(snap, scope) {
    rows <- snap$principal_investigator
    leads <- count_in(snap$study$principal_investigator_id, rows$principal_investigator_id)
    contact <- count_in(snap$study$contact_principal_investigator_id, rows$principal_investigator_id)
    edit_cards(
      key_text(rows$principal_investigator_id), rows$name,
      subtitle = ifelse(has_text(rows$email), rows$email, "no email"),
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
        placeholder = "name@vcu.edu",
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
    "A study can only be deleted once it holds no questionnaires. Empty it first (its contents go, the study and ",
    "its questionnaires stay), or delete its questionnaires."
  ),
  empty_about = "Its questionnaires and waves stay, with nothing printed or asked in them.",
  cards = function(snap, scope) {
    rows <- snap$study
    n_q <- count_in(snap$questionnaire$study_id, rows$study_id)
    registry <- edit_lookup(snap, "registry", "registry_id", rows$registry_id, "abbreviation")
    lead <- edit_lookup(snap, "principal_investigator", "principal_investigator_id", rows$principal_investigator_id, "name")
    edit_cards(
      key_text(rows$study_id), rows$name,
      subtitle = paste0(registry, ifelse(is.na(lead), "", paste0(" · led by ", lead))),
      facts = Map(
        function(questionnaires, publications) {
          c(edit_count(questionnaires, "questionnaire"), if (publications > 0) edit_count(publications, "publication"))
        },
        n_q, count_in(snap$study_publication$study_id, rows$study_id)
      )
    )
  },
  fields = function(snap, mode, row, scope) {
    investigators <- edit_choices(snap, "principal_investigator", blank = "Choose an investigator")
    list(
      edit_field("registry_id", "Registry", "select",
        required = TRUE,
        choices = edit_choices(snap, "registry", blank = "Choose a registry")
      ),
      edit_field("principal_investigator_id", "Investigator", "select", choices = investigators, hint = "Who leads this study."),
      edit_field("contact_principal_investigator_id", "Contact", "select",
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
      edit_field("acknowledgements", "Acknowledgements", "textarea"),
      edit_hidden_field("study")
    )
  },
  # its publications by title, each opening the publication
  usage_rows = function(snap, row, table, rows) {
    if (table != "study_publication") {
      return(NULL)
    }
    titles <- edit_lookup(snap, "publication", "publication_id", rows$publication_id, "title")
    by_title <- order(tolower(titles), method = "radix")
    rows <- rows[by_title, , drop = FALSE]
    list(rows = rows, labels = titles[by_title], text = edit_count(nrow(rows), "publication"), open = edit_opens("publication", rows$publication_id))
  },
  duplicates = function(snap, values) {
    registry <- if (has_text(values$registry_id)) list(registry_id = as.numeric(values$registry_id))
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "study", "name", values$name, "A study with this name", within = registry)
    ))
  }
)

# ------ Publications ------------------------------------------------------------------------------------
# A paper, chapter or report that used a study's data (PUBLICATION). One can be about several studies, and a
# study has many, so which studies it is about is a table of its own (STUDY_PUBLICATION): the form picks them
# with the publication, and saving adds and removes those rows with it.

# What a DOI looks like (publication_doi_format in the database).
doi_pattern <- "^10\\.[0-9]{4,9}/[^[:space:]]+$"

# What a plain link looks like (publication_url_format in the database).
link_pattern <- "^https?://[^[:space:]]+$"

# A DOI as it is kept: the DOI alone, in lower case, whatever was pasted ("https://doi.org/10.1007/BF01065905",
# "doi:10.1007/BF01065905"). The link to the publication is https://doi.org/ followed by it.
# Returns: The DOIs; what isn't one comes back trimmed, for the form to point out.
normal_doi <- function(doi) {
  doi <- trimws(as.character(doi))
  doi <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  tolower(sub("^doi:[[:space:]]*", "", doi, ignore.case = TRUE))
}

# The studies a publication is about.
# Returns: Their IDs.
publication_studies <- function(snap, publication_id) {
  links <- snap$study_publication
  links$study_id[key_text(links$publication_id) %in% key_text(publication_id)]
}

publication_entity <- list(
  label = "Publication", noun = "publication", plural = "publications", article = "a",
  group = "Registries and studies", table = "publication",
  about = paste0(
    "A paper, chapter or report that used a study's data. It can be about more than one study: each study it is ",
    "about lists it."
  ),
  update_about = "A publication is shared by the studies it is about: a change shows in each of their lists.",
  delete_about = "Deleting a publication takes it off every study's list. The studies stay.",
  reads = c("study", "registry", "study_publication"),
  cards = function(snap, scope) {
    rows <- snap$publication
    said <- function(...) {
      parts <- c(...)
      paste(parts[!is.na(parts) & nzchar(parts)], collapse = " · ")
    }
    about <- lapply(rows$publication_id, function(id) {
      sort(edit_lookup(snap, "study", "study_id", publication_studies(snap, id), "name"))
    })
    edit_cards(
      key_text(rows$publication_id), rows$title,
      subtitle = vapply(seq_len(nrow(rows)), function(i) {
        said(rows$authors[i], rows$journal[i], substr(rows$published_date[i], 1, 4))
      }, character(1)),
      facts = lapply(about, function(studies) if (length(studies) > 0) studies else "in no study's list yet")
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("title", "Title", "textarea", required = TRUE, rows = 2, placeholder = "The publication's title"),
      edit_field("authors", "Authors",
        placeholder = "e.g. Eaves LJ, Silberg JL, Meyer JM", hint = "As the publication lists them, in one line."
      ),
      edit_field("journal", "Journal", placeholder = "e.g. Behavior Genetics", hint = "Or the book or series it is in."),
      edit_field("published_date", "Date published", "date",
        placeholder = "MM/DD/YYYY", hint = "The first of the month, or of the year, when only that is known."
      ),
      edit_field("doi", "DOI",
        placeholder = "e.g. 10.1007/BF01065905", normalize = normal_doi,
        hint = "Paste the DOI or its link. It is kept as the DOI alone; the link is https://doi.org/ followed by it.",
        problem = function(value, values) {
          if (!has_text(value)) {
            return(NULL) # a publication needn't have one
          }
          doi <- normal_doi(value)
          if (!grepl(doi_pattern, doi)) {
            return("A DOI starts with 10. and a slash follows its first number, like 10.1007/BF01065905.")
          }
          same <- edit_taken(snap, "publication", "doi", doi, row)
          if (nrow(same) > 0) sprintf("“%s” already has this DOI.", shorten(same$title[1], 60))
        }
      ),
      edit_field("url", "Link",
        placeholder = "e.g. https://scholarscompass.vcu.edu/...",
        hint = "For a publication with no DOI: where it can be read. With a DOI, the DOI's link is the one shown.",
        problem = function(value, values) {
          if (has_text(value) && !grepl(link_pattern, trimws(value))) {
            "A link is a whole address, starting https:// or http://, with no spaces."
          }
        }
      ),
      edit_field("studies", "Studies it is about", "several",
        required = TRUE, short = "studies it is about", choices = edit_choices(snap, "study"),
        hint = "Every study in the database whose data it used."
      )
    )
  },
  initial = function(snap, row, scope) list(studies = edit_several_text(publication_studies(snap, row$publication_id))),
  # the studies it is about by name, each opening the study
  usage_rows = function(snap, row, table, rows) {
    if (table != "study_publication") {
      return(NULL)
    }
    names <- edit_lookup(snap, "study", "study_id", rows$study_id, "name")
    by_name <- order(tolower(names), method = "radix")
    rows <- rows[by_name, , drop = FALSE]
    list(rows = rows, labels = names[by_name], text = paste("In the list of", edit_count(nrow(rows), "study", "studies")), open = edit_opens("study", rows$study_id))
  },
  # the publication's own fields, then a row of STUDY_PUBLICATION for each study picked, and none for one unpicked
  build = function(mode, snap, row, scope, fields, values) {
    entity <- edit_entities$publication
    own <- Filter(function(field) field$name != "studies", fields)
    picked <- edit_several_ids(values[["publication__studies"]])
    listed <- function(publication, studies, action) {
      lapply(studies, function(study) {
        key <- edit_identifier(list(publication_id = publication, study_id = as.numeric(study)))
        edit_item("study_publication", key, action, NULL, NA, NA)
      })
    }
    items <- if (mode == "add") {
      c(list(edit_add_items(entity, snap, own, values)), listed("new:publication:1", picked, "Insert"))
    } else {
      had <- key_text(publication_studies(snap, row$publication_id))
      c(
        list(edit_update_items(entity, snap, row, own, values)),
        listed(row$publication_id, setdiff(picked, had), "Insert"), listed(row$publication_id, setdiff(had, picked), "Delete")
      )
    }
    items <- Filter(Negate(is.null), items)
    if (length(items) == 0) NULL else do.call(rbind, items)
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "publication", "title", values$title, "A publication with this title")
    ))
  }
)
