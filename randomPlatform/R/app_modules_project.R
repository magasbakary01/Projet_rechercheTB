rp_project_panel <- function() {
  output_dir <- rp_default_output_dir()
  shiny::tagList(
    rp_card(
      "Informations sur l'\u00E9tude",
      subtitle = "Informations reprises dans le rapport, les enveloppes et les fichiers de reproductibilit\u00E9.",
      rp_grid(
        shiny::textInput("project_name", "Nom de l'\u00E9tude", value = "Exemple d'essai clinique"),
        shiny::textInput("protocol_no", "Num\u00E9ro de protocole", value = "RP-001"),
        shiny::textInput("sponsor_name", "Nom du promoteur", value = "Exemple de promoteur"),
        shiny::selectInput("language", "Langue", choices = c("Fran\u00E7ais" = "fr-FR", "Chinois" = "zh-CN", "Anglais" = "en-US"), selected = "fr-FR")
      )
    ),
    rp_card(
      "Sorties et s\u00E9curit\u00E9",
      subtitle = "Fonctionne en local par d\u00E9faut ; le t\u00E9l\u00E9chargement des fichiers sensibles demande une confirmation suppl\u00E9mentaire.",
      shiny::tags$div(
        class = "rp-output-dir-row",
        shiny::textInput("output_dir", "Dossier de sortie", value = output_dir),
        shinyFiles::shinyDirButton("choose_output_dir", "Choisir un dossier", "S\u00E9lectionnez le dossier de sortie", class = "btn-secondary rp-dir-button")
      ),
      rp_grid(
        shiny::checkboxInput("encrypt_sensitive_outputs", "Exiger un mot de passe pour les fichiers sensibles (aucun chiffrement des fichiers actuellement)", value = FALSE),
        shiny::passwordInput("password", "Mot de passe (seule sa saisie est v\u00E9rifi\u00E9e)", value = "")
      )
    )
  )
}

rp_default_output_dir <- function() {
  home <- Sys.getenv("USERPROFILE", unset = path.expand("~"))
  documents <- file.path(home, "Documents")
  root <- if (dir.exists(documents)) documents else home
  path <- file.path(root, "randomPlatform_outputs")
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(if (dir.exists(path)) path else root, winslash = "/", mustWork = TRUE)
}
