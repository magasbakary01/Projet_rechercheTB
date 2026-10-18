rp_outputs_panel <- function() {
  shiny::tagList(
    rp_card(
      "Fichiers de sortie",
      subtitle = "Choisissez le rapport, les enveloppes PDF et les fichiers de reproductibilit\u00E9 \u00E0 g\u00E9n\u00E9rer.",
      rp_grid(
        shiny::checkboxInput("generate_report", "G\u00E9n\u00E9rer le rapport de randomisation", value = TRUE),
        shiny::textInput("report_file", "Nom du fichier du rapport", value = "randomization_report.xlsx")
      ),
      rp_grid(
        shiny::checkboxInput("generate_random_envelope", "G\u00E9n\u00E9rer les enveloppes de randomisation (PDF)", value = FALSE),
        shiny::textInput("random_envelope_file", "Nom du fichier des enveloppes de randomisation", value = "random_envelopes.pdf")
      ),
      rp_grid(
        shiny::checkboxInput("generate_emergency_envelope", "G\u00E9n\u00E9rer les enveloppes d'urgence (PDF)", value = FALSE),
        shiny::textInput("emergency_envelope_file", "Nom du fichier des enveloppes d'urgence", value = "emergency_unblinding_envelopes.pdf")
      ),
      rp_grid(
        shiny::checkboxInput("generate_reproducibility", "G\u00E9n\u00E9rer le dossier de reproductibilit\u00E9", value = TRUE),
        shiny::checkboxInput("standalone_reproducibility_code", "G\u00E9n\u00E9rer un code de reproduction autonome", value = TRUE),
        shiny::checkboxInput("generate_print_checklist", "G\u00E9n\u00E9rer la liste de contr\u00F4le d'impression et de mise sous enveloppe", value = TRUE),
        shiny::checkboxInput("split_files_by_center", "Un fichier d'enveloppes par centre (pr\u00E9vu, non actif)", value = FALSE)
      )
    ),
    shiny::tags$div(class = "rp-sensitive", "Les fichiers sensibles comprennent la liste principale non aveugle, les enveloppes d'urgence et la liste de correspondance (code de l'aveugle) du dossier de reproductibilit\u00E9. V\u00E9rifiez les droits de l'utilisateur avant tout t\u00E9l\u00E9chargement.")
  )
}
