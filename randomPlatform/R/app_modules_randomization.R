rp_randomization_panel <- function() {
  shiny::tagList(
    rp_card(
      "Plan de randomisation",
      subtitle = "Choisissez la m\u00E9thode de randomisation et fixez la graine al\u00E9atoire pour garantir la reproductibilit\u00E9.",
      rp_grid(
        shiny::selectInput(
          "method",
          "M\u00E9thode de randomisation",
          choices = c("Randomisation simple" = "simple", "Randomisation par blocs" = "block", "Randomisation stratifi\u00E9e" = "stratified", "Randomisation stratifi\u00E9e par blocs" = "stratified_block")
        ),
        shiny::numericInput("seed", "Graine al\u00E9atoire", value = 20260528, min = 1, step = 1)
      )
    ),
    rp_card(
      "R\u00E8gles de num\u00E9rotation",
      subtitle = "Format des num\u00E9ros de randomisation, des num\u00E9ros d'ordre des enveloppes et des num\u00E9ros de traitement.",
      rp_grid(
        shiny::textInput("random_no_prefix", "Pr\u00E9fixe du num\u00E9ro de randomisation", value = "R"),
        shiny::numericInput("random_no_width", "Chiffres du num\u00E9ro de randomisation", value = 3, min = 1, max = 8),
        shiny::textInput("envelope_no_prefix", "Pr\u00E9fixe du num\u00E9ro d'ordre des enveloppes", value = "No. "),
        shiny::numericInput("envelope_no_width", "Chiffres du num\u00E9ro d'ordre des enveloppes", value = 3, min = 1, max = 8),
        shiny::textInput("code_prefix", "Pr\u00E9fixe du num\u00E9ro de traitement", value = "MED"),
        shiny::numericInput("code_width", "Chiffres du num\u00E9ro de traitement", value = 4, min = 1, max = 10)
      ),
      rp_grid(
        shiny::checkboxInput("random_no_by_center", "Num\u00E9ros de randomisation par centre", value = FALSE),
        shiny::checkboxInput("code_random_digits", "Num\u00E9ros de traitement tir\u00E9s au hasard", value = TRUE),
        shiny::checkboxInput("code_by_group", "Pr\u00E9fixe du num\u00E9ro li\u00E9 au groupe (ne pas cocher en aveugle !)", value = FALSE)
      )
    ),
    rp_card(
      "Blocs et plage des num\u00E9ros de traitement",
      subtitle = "S\u00E9parez les valeurs multiples par une virgule (,).",
      rp_grid(
        shiny::numericInput("code_range_min", "Valeur minimale des num\u00E9ros al\u00E9atoires", value = 1000, min = 0, step = 1),
        shiny::numericInput("code_range_max", "Valeur maximale des num\u00E9ros al\u00E9atoires", value = 9999, min = 1, step = 1),
        shiny::textInput("block_sizes", "Taille des blocs", value = "4,6,8"),
        shiny::textInput("block_size_probs", "Probabilit\u00E9s des tailles de blocs (facultatif)", value = "")
      )
    ),
    rp_card(
      "Facteurs de stratification",
      subtitle = "Jusqu'\u00E0 6 facteurs de stratification ; une alerte s'affiche si le nombre de strates est trop \u00E9lev\u00E9.",
      rp_grid(
        shiny::numericInput("strata_count", "Nombre de facteurs de stratification", value = 0, min = 0, max = 6, step = 1),
        shiny::uiOutput("center_var_ui")
      ),
      shiny::uiOutput("strata_rows"),
      shiny::tags$div(class = "rp-info-box", shiny::uiOutput("strata_preview"))
    )
  )
}

rp_strata_rows_ui <- function(strata_count) {
  if (strata_count < 1) return(shiny::tags$em("Aucun facteur de stratification d\u00E9fini."))
  shiny::tagList(lapply(seq_len(strata_count), function(i) {
    shiny::tags$div(
      class = "rp-repeat-row rp-repeat-row-compact",
      shiny::tags$div(class = "rp-repeat-index", paste0("#", i)),
      shiny::textInput(paste0("strata_name_", i), paste0("Facteur de stratification ", i, " : nom"), value = if (i == 1) "center" else paste0("factor", i)),
      shiny::textInput(paste0("strata_levels_", i), "Modalit\u00E9s (s\u00E9par\u00E9es par des virgules)", value = if (i == 1) "C01,C02" else "bas,haut")
    )
  }))
}

rp_collect_strata <- function(input) {
  n <- as.integer(input$strata_count %||% 0)
  if (n < 1) return(NULL)
  out <- list()
  for (i in seq_len(n)) {
    nm <- trimws(input[[paste0("strata_name_", i)]])
    lv <- trimws(strsplit(input[[paste0("strata_levels_", i)]], ",", fixed = TRUE)[[1]])
    lv <- lv[nzchar(lv)]
    if (nzchar(nm) && length(lv) > 0) out[[nm]] <- lv
  }
  if (length(out) == 0) NULL else out
}

rp_parse_numeric_csv <- function(value, integer = FALSE) {
  value <- trimws(value %||% "")
  if (!nzchar(value)) return(NULL)
  out <- as.numeric(trimws(strsplit(value, ",", fixed = TRUE)[[1]]))
  if (any(is.na(out))) stop("Comma-separated numeric fields must contain only numbers.", call. = FALSE)
  if (integer) out <- as.integer(out)
  out
}
