rp_results_panel <- function() {
  shiny::tagList(
    rp_card(
      "G\u00E9n\u00E9ration et aper\u00E7u",
      subtitle = "V\u00E9rifiez le r\u00E9sum\u00E9 des param\u00E8tres avant de lancer la randomisation ; les tableaux et les empreintes des fichiers s'affichent ensuite ici.",
      shiny::tags$div(class = "rp-info-box", shiny::uiOutput("parameter_summary")),
      shiny::uiOutput("generation_status")
    ),
    rp_table_card("Aper\u00E7u de la liste en aveugle (sites)", DT::DTOutput("blinded_table")),
    shiny::tags$details(
      shiny::tags$summary("Liste principale non aveugle (personnes autoris\u00E9es uniquement)"),
      shiny::checkboxInput("show_unblinded", "Je confirme que l'utilisateur actuel est une personne autoris\u00E9e", value = FALSE),
      DT::DTOutput("unblinded_table")
    ),
    rp_table_card("Effectifs par groupe", DT::DTOutput("balance_table")),
    rp_table_card("Fichiers produits", DT::DTOutput("files_table")),
    rp_table_card("SHA-256", DT::DTOutput("hash_table"))
  )
}

rp_downloads_panel <- function() {
  shiny::tagList(
    rp_card(
      "T\u00E9l\u00E9chargements",
      subtitle = "Le t\u00E9l\u00E9chargement des fichiers sensibles n\u00E9cessite une confirmation d'autorisation.",
      shiny::checkboxInput("confirm_sensitive_download", "Je confirme que l'utilisateur actuel est autoris\u00E9 \u00E0 t\u00E9l\u00E9charger les fichiers sensibles contenant la liste de correspondance ou des informations permettant la lev\u00E9e de l'aveugle.", value = FALSE),
      shiny::tags$div(
        class = "rp-download-grid",
        shiny::downloadButton("download_zip", "Tous les fichiers (ZIP)"),
        shiny::downloadButton("download_report", "Rapport de randomisation"),
        shiny::downloadButton("download_blinded", "Liste en aveugle (sites)"),
        shiny::downloadButton("download_unblinded", "Liste principale non aveugle"),
        shiny::downloadButton("download_random_pdf", "Enveloppes de randomisation (PDF)"),
        shiny::downloadButton("download_emergency_pdf", "Enveloppes d'urgence (PDF)"),
        shiny::downloadButton("download_repro", "Dossier de reproductibilit\u00E9 (ZIP)"),
        shiny::downloadButton("download_audit", "Journal d'audit"),
        shiny::downloadButton("download_hashes", "Empreintes SHA-256")
      )
    )
  )
}

rp_audit_panel <- function() {
  shiny::tagList(
    rp_card(
      "Audit et reproductibilit\u00E9",
      subtitle = "Environnement R, param\u00E8tres du g\u00E9n\u00E9rateur al\u00E9atoire (RNG), empreintes et r\u00E9sultat de la v\u00E9rification de reproductibilit\u00E9.",
      shiny::tags$div(class = "rp-info-box", shiny::uiOutput("runtime_info")),
      shiny::tags$div(class = "rp-action-row", shiny::actionButton("verify_repro", "V\u00E9rifier la reproductibilit\u00E9", class = "btn-primary")),
      shiny::verbatimTextOutput("verify_result")
    ),
    rp_card(
      "Aper\u00E7u du journal d'audit",
      shiny::tags$div(class = "rp-preview-box", shiny::verbatimTextOutput("audit_preview"))
    ),
    rp_card(
      "Commande de reproduction",
      shiny::tags$code("source('reproducibility/reproduce_randomization.R')")
    )
  )
}
