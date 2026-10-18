rp_app_ui <- function() {
  www <- system.file("app/www", package = "randomPlatform")
  if (!nzchar(www) && dir.exists("inst/app/www")) www <- normalizePath("inst/app/www", winslash = "/", mustWork = TRUE)
  if (nzchar(www)) shiny::addResourcePath("rp-www", www)
  shiny::fluidPage(
    theme = bslib::bs_theme(
      version = 3,
      bootswatch = "flatly",
      primary = "#176f6b",
      secondary = "#4b5563",
      success = "#2f7d4f",
      danger = "#b42318",
      warning = "#b7791f",
      base_font = bslib::font_collection("-apple-system", "BlinkMacSystemFont", "Segoe UI", "Microsoft YaHei", "Arial", "sans-serif")
    ),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "rp-www/app.css"),
      shiny::tags$link(rel = "icon", href = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'%3E%3Crect width='64' height='64' rx='10' fill='%23176f6b'/%3E%3Ctext x='32' y='39' text-anchor='middle' font-size='22' font-family='Arial' font-weight='700' fill='white'%3ERP%3C/text%3E%3C/svg%3E"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$script(shiny::HTML(
        "window.rpShowPage = function(id) {
          document.querySelectorAll('.rp-page').forEach(function(el) { el.style.display = 'none'; });
          var page = document.getElementById('rp-page-' + id);
          if (page) page.style.display = 'block';
          document.querySelectorAll('.rp-nav-list > li').forEach(function(el) { el.classList.remove('active'); });
          var link = document.querySelector('.rp-nav-list a[data-rp-page=\"' + id + '\"]');
          if (link && link.parentElement) link.parentElement.classList.add('active');
          var refresh = function() {
            window.dispatchEvent(new Event('resize'));
            if (window.HTMLWidgets && window.HTMLWidgets.staticRender) window.HTMLWidgets.staticRender();
          };
          setTimeout(refresh, 80);
          setTimeout(refresh, 350);
          setTimeout(refresh, 900);
        };
        document.addEventListener('DOMContentLoaded', function() {
          var registerHandler = function() {
            if (!window.Shiny) {
              setTimeout(registerHandler, 60);
              return;
            }
            if (!window.rpShowPageHandlerRegistered) {
              window.rpShowPageHandlerRegistered = true;
              window.Shiny.addCustomMessageHandler('rp-show-page', function(id) {
                window.rpShowPage(id);
              });
            }
          };
          registerHandler();
        });"
      ))
    ),
    shiny::tags$div(
      class = "rp-app",
      shiny::tags$header(
        class = "rp-topbar",
        shiny::tags$div(
          class = "rp-brand",
          shiny::tags$span(class = "rp-brand-mark", "RP"),
          shiny::tags$div(
            shiny::tags$h1("randomPlatform"),
            shiny::tags$p("Gestion de la randomisation en recherche clinique")
          ),
          shiny::tags$span(class = "rp-version-badge", "V1.0")
        ),
        shiny::tags$div(
          class = "rp-topbar-meta",
          shiny::tags$span("Local uniquement"),
          shiny::tags$strong("127.0.0.1")
        )
      ),
      shiny::tags$div(
        class = "rp-workspace",
        shiny::tags$div(
          class = "rp-shell",
          shiny::tags$aside(
            class = "rp-sidebar",
            shiny::tags$div(
              class = "rp-nav-host",
              shiny::tags$ul(
                class = "nav nav-pills nav-stacked rp-nav-list",
                rp_nav_link("project", "Informations sur l'\u00E9tude", active = TRUE),
                rp_nav_link("interventions", "Bras de traitement"),
                rp_nav_link("randomization", "Plan de randomisation"),
                rp_nav_link("outputs", "Fichiers de sortie"),
                rp_nav_link("results", "G\u00E9n\u00E9ration et aper\u00E7u"),
                rp_nav_link("downloads", "T\u00E9l\u00E9chargements"),
                rp_nav_link("audit", "Audit et reproductibilit\u00E9")
              )
            ),
            shiny::tags$div(
              class = "rp-execute-panel",
              shiny::actionButton("generate", "Lancer la randomisation", class = "btn-primary btn-lg rp-execute-button"),
              shiny::tags$p("G\u00E9n\u00E8re la randomisation \u00E0 partir des param\u00E8tres saisis."),
              shiny::tags$div(
                class = "rp-envelope-sequence-warning",
                "Attention : les enveloppes de randomisation produites par ce syst\u00E8me doivent \u00EAtre ouvertes strictement dans l'ordre croissant des num\u00E9ros, selon l'ordre d'inclusion des participants."
              )
            )
          ),
          shiny::tags$main(
            class = "rp-main",
            rp_page_panel("project", rp_project_panel(), active = TRUE),
            rp_page_panel("interventions", rp_interventions_panel()),
            rp_page_panel("randomization", rp_randomization_panel()),
            rp_page_panel("outputs", rp_outputs_panel()),
            rp_page_panel("results", rp_results_panel()),
            rp_page_panel("downloads", rp_downloads_panel()),
            rp_page_panel("audit", rp_audit_panel())
          )
        )
      ),
      shiny::tags$footer(
        class = "rp-footer",
        shiny::tags$div(
          class = "rp-footer-brand",
          shiny::tags$strong("Centre de recherche clinique, h\u00F4pital Longhua, Universit\u00E9 de m\u00E9decine traditionnelle chinoise de Shanghai")
        ),
        shiny::tags$div(
          class = "rp-footer-links",
          shiny::tags$a(href = "mailto:yangpluszhu@sina.com", "Contact : yangpluszhu@sina.com"),
          shiny::tags$a(href = "https://github.com/yangpluszhu/randomPlatform", target = "_blank", rel = "noopener noreferrer", "D\u00E9p\u00F4t du projet : https://github.com/yangpluszhu/randomPlatform")
        )
      )
    )
  )
}

rp_nav_link <- function(id, label, active = FALSE) {
  js <- sprintf(
    "window.rpShowPage('%s'); return false;",
    id
  )
  shiny::tags$li(
    class = if (isTRUE(active)) "active" else NULL,
    shiny::tags$a(href = "#", `data-rp-page` = id, onclick = js, label)
  )
}

rp_page_panel <- function(id, ..., active = FALSE) {
  shiny::tags$div(
    id = paste0("rp-page-", id),
    class = "rp-page",
    style = if (isTRUE(active)) NULL else "display: none;",
    ...
  )
}

rp_card <- function(title, ..., subtitle = NULL, class = "") {
  shiny::tags$section(
    class = paste("rp-card", class),
    shiny::tags$div(
      class = "rp-card-heading",
      shiny::tags$h2(title),
      if (!is.null(subtitle)) shiny::tags$p(subtitle)
    ),
    ...
  )
}

rp_grid <- function(..., class = "rp-form-grid") {
  shiny::tags$div(class = class, ...)
}

rp_table_card <- function(title, output, subtitle = NULL) {
  shiny::tags$section(
    class = "rp-card rp-table-card",
    shiny::tags$div(class = "rp-card-heading", shiny::tags$h2(title), if (!is.null(subtitle)) shiny::tags$p(subtitle)),
    output
  )
}

rp_stat_line <- function(label, value) {
  shiny::tags$div(
    class = "rp-stat-line",
    shiny::tags$dt(label),
    shiny::tags$dd(if (is.null(value) || length(value) == 0 || !nzchar(as.character(value)[1])) "Non renseign\u00E9" else as.character(value)[1])
  )
}

rp_info_grid <- function(...) {
  shiny::tags$dl(class = "rp-info-grid", ...)
}
