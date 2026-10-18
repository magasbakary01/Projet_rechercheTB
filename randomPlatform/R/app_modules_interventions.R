rp_interventions_panel <- function() {
  shiny::tagList(
    rp_card(
      "Bras de traitement",
      subtitle = "De 2 \u00E0 5 groupes ; saisir soit l'effectif total et le ratio d'allocation, soit directement l'effectif de chaque groupe.",
      rp_grid(
        shiny::numericInput("group_count", "Nombre de groupes", value = 2, min = 2, max = 5, step = 1),
        shiny::numericInput("n_total", "Effectif total", value = 20, min = 1, step = 1)
      ),
      shiny::radioButtons(
        "sample_input_mode",
        "Mode de saisie des effectifs",
        choices = c("Effectif total + ratio d'allocation" = "ratio", "Effectif par groupe" = "n_per_group"),
        selected = "ratio",
        inline = TRUE
      ),
      shiny::textInput("blind_label", "Libell\u00E9 en aveugle (identique pour tous les groupes)", value = "Traitement de l'\u00E9tude"),
      shiny::uiOutput("intervention_rows"),
      shiny::uiOutput("sample_size_hint")
    ),
    shiny::tags$div(
      class = "rp-warning",
      shiny::strong("Maintien de l'aveugle :"),
      "si le pr\u00E9fixe du num\u00E9ro de traitement d\u00E9pend du groupe, les sites peuvent deviner l'allocation. En double aveugle, utilisez un pr\u00E9fixe commun \u00E0 tous les groupes."
    )
  )
}

rp_intervention_rows_ui <- function(group_count, sample_input_mode = "ratio") {
  defaults <- LETTERS[seq_len(group_count)]
  derived_n <- identical(sample_input_mode, "ratio")
  rows <- lapply(seq_len(group_count), function(i) {
    shiny::tags$div(
      class = paste("rp-repeat-row", if (derived_n) "rp-ratio-mode" else "rp-direct-n-mode"),
      shiny::tags$div(class = "rp-repeat-index", paste0("#", i)),
      shiny::textInput(paste0("group_id_", i), "Code du groupe", value = defaults[i]),
      shiny::textInput(paste0("intervention_name_", i), "Nom de l'intervention", value = if (i == 1) "Groupe exp\u00E9rimental" else if (i == 2) "Groupe t\u00E9moin" else paste0("Groupe ", i)),
      shiny::tagAppendAttributes(
        shiny::numericInput(paste0("ratio_", i), "Ratio d'allocation", value = 1, min = 0.01, step = 0.01),
        class = "rp-ratio-field"
      ),
      shiny::tagAppendAttributes(
        shiny::numericInput(paste0("n_group_", i), "Effectif du groupe", value = 10, min = 0, step = 1),
        class = "rp-n-group-field"
      )
    )
  })
  readonly_script <- sprintf(
    "setTimeout(function() {
      var ratioMode = %s;
      document.querySelectorAll('#intervention_rows input[id^=\"ratio_\"]').forEach(function(el) {
        el.readOnly = !ratioMode;
        el.classList.toggle('rp-readonly-input', !ratioMode);
      });
      document.querySelectorAll('#intervention_rows input[id^=\"n_group_\"]').forEach(function(el) {
        el.readOnly = ratioMode;
        el.classList.toggle('rp-readonly-input', ratioMode);
      });
    }, 0);",
    if (derived_n) "true" else "false"
  )
  shiny::tagList(rows, shiny::tags$script(shiny::HTML(readonly_script)))
}

rp_collect_interventions <- function(input) {
  n <- as.integer(input$group_count)
  group_id <- vapply(seq_len(n), function(i) input[[paste0("group_id_", i)]] %||% LETTERS[i], character(1))
  intervention_name <- vapply(seq_len(n), function(i) input[[paste0("intervention_name_", i)]] %||% paste0("Group ", i), character(1))
  blind_label_value <- input$blind_label %||% "Treatment"
  blind_label <- rep(as.character(blind_label_value), n)
  data.frame(group_id = group_id, intervention_name = intervention_name, blind_label = blind_label, stringsAsFactors = FALSE)
}

rp_collect_allocation <- function(input, interventions) {
  n <- nrow(interventions)
  ratio <- vapply(seq_len(n), function(i) input[[paste0("ratio_", i)]] %||% 1, numeric(1))
  n_group <- vapply(seq_len(n), function(i) input[[paste0("n_group_", i)]] %||% 0, numeric(1))
  names(ratio) <- interventions$group_id
  names(n_group) <- interventions$group_id
  if (identical(input$sample_input_mode, "n_per_group")) {
    n_group <- stats::setNames(as.integer(n_group), names(n_group))
    list(n_total = NULL, allocation_ratio = NULL, n_per_group = n_group)
  } else {
    list(n_total = as.integer(input$n_total), allocation_ratio = ratio, n_per_group = NULL)
  }
}
