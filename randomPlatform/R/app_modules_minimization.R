rp_minimization_panel <- function() {
  shiny::tagList(
    rp_card(
      "Minimisation (randomisation dynamique)",
      subtitle = "Attribution dynamique au fil des inclusions ; l'\u00E9tat de la session est enregistr\u00E9 dans un fichier local.",
      rp_grid(
        shiny::textInput("min_factors", "Facteurs d'\u00E9quilibre (s\u00E9par\u00E9s par des virgules, 6 au maximum)", value = "center,sex"),
        shiny::textInput("min_weights", "Poids des facteurs (s\u00E9par\u00E9s par des virgules)", value = "1,1"),
        shiny::numericInput("min_prob_best", "prob_best", value = 0.8, min = 0.01, max = 1, step = 0.01),
        shiny::textInput("min_state_file", "Nom du fichier d'\u00E9tat", value = "minimization_session.rds")
      ),
      shiny::tags$div(class = "rp-action-row", shiny::actionButton("min_start", "D\u00E9marrer / r\u00E9initialiser la session", class = "btn-primary"))
    ),
    rp_card(
      "Attribution au fil des inclusions",
      rp_grid(
        shiny::textInput("min_subject_id", "Num\u00E9ro de s\u00E9lection ou identifiant interne du participant", value = ""),
        shiny::textInput("min_operator", "Nom de l'op\u00E9rateur (facultatif)", value = "")
      ),
      shiny::uiOutput("min_covariate_inputs"),
      shiny::textAreaInput("min_note", "Commentaire (facultatif)", value = "", rows = 2),
      shiny::tags$div(class = "rp-action-row", shiny::actionButton("min_assign", "Attribuer au participant suivant", class = "btn-success"))
    ),
    rp_table_card("Derni\u00E8re attribution (en aveugle)", DT::DTOutput("min_blinded")),
    shiny::tags$details(
      shiny::tags$summary("Derni\u00E8re attribution non aveugle (personnes autoris\u00E9es uniquement)"),
      shiny::checkboxInput("min_show_unblinded", "Je confirme que l'utilisateur actuel est une personne autoris\u00E9e", value = FALSE),
      DT::DTOutput("min_unblinded")
    ),
    rp_table_card("Attributions enregistr\u00E9es", DT::DTOutput("min_assignments"))
  )
}
