rp_build_call_args <- function(input) {
  interventions <- rp_collect_interventions(input)
  alloc <- rp_collect_allocation(input, interventions)
  strata <- rp_collect_strata(input)
  method <- input$method
  if (identical(method, "minimization")) {
    stop("Pour la minimisation, utilisez l'attribution au fil des inclusions de la page \u00AB Minimisation (randomisation dynamique) \u00BB.", call. = FALSE)
  }
  center_var <- input$center_var
  if (is.null(center_var) || !nzchar(center_var) || identical(center_var, "<none>")) center_var <- NULL
  list(
    project_name = input$project_name,
    protocol_no = input$protocol_no,
    sponsor_name = input$sponsor_name,
    interventions = interventions,
    method = method,
    seed = as.integer(input$seed),
    n_total = alloc$n_total,
    n_per_group = alloc$n_per_group,
    allocation_ratio = alloc$allocation_ratio,
    strata = strata,
    center_var = center_var,
    block_sizes = rp_parse_numeric_csv(input$block_sizes, integer = TRUE),
    block_size_probs = rp_parse_numeric_csv(input$block_size_probs),
    random_no_prefix = input$random_no_prefix,
    random_no_width = as.integer(input$random_no_width),
    random_no_by_center = isTRUE(input$random_no_by_center),
    code_prefix = input$code_prefix,
    code_width = as.integer(input$code_width),
    code_random_digits = isTRUE(input$code_random_digits),
    code_random_range = c(as.integer(input$code_range_min), as.integer(input$code_range_max)),
    code_by_group = isTRUE(input$code_by_group),
    envelope_no_prefix = input$envelope_no_prefix,
    envelope_no_width = as.integer(input$envelope_no_width),
    generate_random_envelope = isTRUE(input$generate_random_envelope),
    random_envelope_file = input$random_envelope_file,
    generate_emergency_envelope = isTRUE(input$generate_emergency_envelope),
    emergency_envelope_file = input$emergency_envelope_file,
    generate_report = isTRUE(input$generate_report),
    report_file = input$report_file,
    generate_reproducibility = isTRUE(input$generate_reproducibility),
    standalone_reproducibility_code = isTRUE(input$standalone_reproducibility_code),
    output_dir = input$output_dir,
    language = input$language,
    encrypt_sensitive_outputs = isTRUE(input$encrypt_sensitive_outputs),
    password = input$password
  )
}

rp_group_n_from_ratio <- function(n_total, ratio) {
  n_total <- as.integer(n_total)
  ratio_names <- names(ratio)
  ratio <- as.numeric(ratio)
  names(ratio) <- ratio_names
  if (is.na(n_total) || n_total < 0 || any(is.na(ratio)) || any(ratio < 0) || sum(ratio) <= 0) {
    return(stats::setNames(rep(0L, length(ratio)), names(ratio)))
  }
  raw_n <- n_total * ratio / sum(ratio)
  base_n <- floor(raw_n)
  remainder <- n_total - sum(base_n)
  if (remainder > 0) {
    order_add <- order(raw_n - base_n, decreasing = TRUE)
    base_n[order_add[seq_len(remainder)]] <- base_n[order_add[seq_len(remainder)]] + 1L
  }
  stats::setNames(as.integer(base_n), names(ratio))
}

rp_app_pages <- function() {
  c(
    project = "Informations sur l'\u00E9tude",
    interventions = "Bras de traitement",
    randomization = "Plan de randomisation",
    outputs = "Fichiers de sortie",
    results = "G\u00E9n\u00E9ration et aper\u00E7u",
    downloads = "T\u00E9l\u00E9chargements",
    audit = "Audit et reproductibilit\u00E9"
  )
}

rp_copy_download <- function(file_path, file) {
  if (is.null(file_path) || !file.exists(file_path)) stop("Requested file has not been generated.", call. = FALSE)
  file.copy(file_path, file, overwrite = TRUE)
}

rp_zip_paths <- function(paths, zip_file) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) stop("No generated files are available to download.", call. = FALSE)
  old <- setwd(dirname(paths[1]))
  on.exit(setwd(old), add = TRUE)
  zip::zipr(zipfile = zip_file, files = basename(paths))
}

rp_directory_roots <- function() {
  roots <- list()
  add_root <- function(label, path) {
    if (!is.null(path) && length(path) == 1 && nzchar(path) && dir.exists(path)) {
      roots[[label]] <<- normalizePath(path, winslash = "/", mustWork = TRUE)
    }
  }

  home <- Sys.getenv("USERPROFILE", unset = path.expand("~"))
  add_root("Home", home)
  add_root("Desktop", file.path(home, "Desktop"))
  add_root("Documents", file.path(home, "Documents"))
  add_root("Downloads", file.path(home, "Downloads"))
  add_root("Workspace", getwd())

  if (identical(.Platform$OS.type, "windows")) {
    drives <- paste0(LETTERS, ":/")
    for (drive in drives[dir.exists(drives)]) {
      add_root(paste0("Drive ", sub("/$", "", drive)), drive)
    }
  } else {
    add_root("Root /", "/")
  }

  unlist(roots, use.names = TRUE)
}

rp_app_server <- function(input, output, session) {
  result <- shiny::reactiveVal(NULL)
  last_error <- shiny::reactiveVal(NULL)
  min_session <- shiny::reactiveVal(NULL)
  min_last <- shiny::reactiveVal(NULL)
  syncing_sample_size <- FALSE

  roots <- rp_directory_roots()
  shinyFiles::shinyDirChoose(input, "choose_output_dir", roots = roots, session = session)

  shiny::observeEvent(input$choose_output_dir, {
    chosen <- shinyFiles::parseDirPath(roots, input$choose_output_dir)
    if (length(chosen) == 1 && nzchar(chosen)) {
      shiny::updateTextInput(session, "output_dir", value = normalizePath(chosen, winslash = "/", mustWork = TRUE))
    }
  })

  output$intervention_rows <- shiny::renderUI({
    rp_intervention_rows_ui(as.integer(input$group_count %||% 2), input$sample_input_mode %||% "ratio")
  })

  output$sample_size_hint <- shiny::renderUI({
    if (identical(input$sample_input_mode, "n_per_group")) {
      shiny::tags$div(class = "rp-muted rp-inline-note", "Dans ce mode, l'effectif total est mis \u00E0 jour automatiquement (somme des effectifs des groupes).")
    } else {
      shiny::tags$div(class = "rp-muted rp-inline-note", "Dans ce mode, l'effectif de chaque groupe est calcul\u00E9 automatiquement \u00E0 partir de l'effectif total et du ratio d'allocation (m\u00E9thode du plus fort reste).")
    }
  })

  shiny::observe({
    if (isTRUE(syncing_sample_size)) return()
    shiny::req(input$group_count)
    n <- as.integer(input$group_count)
    if (n < 1 || !identical(input$sample_input_mode, "ratio")) return()
    ratio <- vapply(seq_len(n), function(i) input[[paste0("ratio_", i)]] %||% 1, numeric(1))
    group_id <- vapply(seq_len(n), function(i) input[[paste0("group_id_", i)]] %||% LETTERS[i], character(1))
    names(ratio) <- group_id
    n_group <- rp_group_n_from_ratio(input$n_total %||% 0, ratio)
    syncing_sample_size <<- TRUE
    on.exit(syncing_sample_size <<- FALSE, add = TRUE)
    for (i in seq_len(n)) {
      shiny::updateNumericInput(session, paste0("n_group_", i), value = unname(n_group[i]))
    }
  })

  shiny::observe({
    if (isTRUE(syncing_sample_size)) return()
    shiny::req(input$group_count)
    n <- as.integer(input$group_count)
    if (n < 1 || !identical(input$sample_input_mode, "n_per_group")) return()
    n_group <- vapply(seq_len(n), function(i) input[[paste0("n_group_", i)]] %||% 0, numeric(1))
    total <- sum(as.integer(n_group), na.rm = TRUE)
    syncing_sample_size <<- TRUE
    on.exit(syncing_sample_size <<- FALSE, add = TRUE)
    shiny::updateNumericInput(session, "n_total", value = total)
  })

  output$strata_rows <- shiny::renderUI({
    rp_strata_rows_ui(as.integer(input$strata_count %||% 0))
  })

  output$center_var_ui <- shiny::renderUI({
    strata <- rp_collect_strata(input)
    choices <- c("<none>", names(strata))
    shiny::selectInput("center_var", "Variable d\u00E9signant le centre", choices = stats::setNames(choices, c("(aucune)", names(strata))), selected = if ("center" %in% choices) "center" else "<none>")
  })

  output$strata_preview <- shiny::renderUI({
    strata <- rp_collect_strata(input)
    if (is.null(strata)) {
      return(rp_info_grid(
        rp_stat_line("Nombre de strates", "0"),
        rp_stat_line("Statut", "Stratification non activ\u00E9e")
      ))
    }
    n_combo <- prod(lengths(strata))
    n_total <- if (identical(input$sample_input_mode, "ratio")) as.integer(input$n_total) else {
      sum(vapply(seq_len(input$group_count), function(i) input[[paste0("n_group_", i)]], numeric(1)))
    }
    per <- if (n_combo > 0) floor(n_total / n_combo) else NA_integer_
    risk <- if (n_combo > max(1, n_total / 2)) "Attention : le nombre de strates est trop \u00E9lev\u00E9 par rapport \u00E0 l'effectif. R\u00E9duisez le nombre de facteurs de stratification ou modifiez le plan de randomisation." else "Le nombre de strates est acceptable."
    rp_info_grid(
      rp_stat_line("Nombre de strates", n_combo),
      rp_stat_line("Effectif attendu par strate (environ)", per),
      rp_stat_line("\u00C9valuation", risk)
    )
  })

  output$parameter_summary <- shiny::renderUI({
    args <- tryCatch(rp_build_call_args(input), error = function(e) e)
    if (inherits(args, "error")) {
      return(shiny::tags$div(class = "rp-muted", paste("Param\u00E8tres incomplets :", args$message)))
    }
    rp_info_grid(
      rp_stat_line("Nom de l'\u00E9tude", args$project_name),
      rp_stat_line("Num\u00E9ro de protocole", args$protocol_no),
      rp_stat_line("Promoteur", args$sponsor_name),
      rp_stat_line("M\u00E9thode de randomisation", args$method),
      rp_stat_line("Graine al\u00E9atoire", args$seed),
      rp_stat_line("Nombre de groupes", nrow(args$interventions)),
      rp_stat_line("Effectif total", args$n_total %||% sum(args$n_per_group)),
      rp_stat_line("Facteurs de stratification", paste(names(args$strata %||% list()), collapse = ", ")),
      rp_stat_line("Enveloppes de randomisation", if (isTRUE(args$generate_random_envelope)) "Oui" else "Non"),
      rp_stat_line("Enveloppes d'urgence", if (isTRUE(args$generate_emergency_envelope)) "Oui" else "Non")
    )
  })

  shiny::observeEvent(input$generate, {
    last_error(NULL)
    shiny::withProgress(message = "Randomisation en cours", value = 0, {
      shiny::incProgress(0.2, detail = "V\u00E9rification des param\u00E8tres")
      args <- tryCatch(rp_build_call_args(input), error = function(e) e)
      if (inherits(args, "error")) {
        last_error(args$message)
        return(NULL)
      }
      shiny::incProgress(0.4, detail = "Tirage de la randomisation")
      res <- tryCatch(do.call(rp_randomize, args), error = function(e) e)
      if (inherits(res, "error")) {
        last_error(res$message)
        return(NULL)
      }
      shiny::incProgress(0.3, detail = "Pr\u00E9paration des fichiers de sortie")
      result(res)
      shiny::showModal(shiny::modalDialog(
        title = "Randomisation termin\u00E9e",
        "Les r\u00E9sultats de la randomisation et les fichiers de sortie ont \u00E9t\u00E9 g\u00E9n\u00E9r\u00E9s. V\u00E9rifiez les r\u00E9sultats dans la page \u00AB G\u00E9n\u00E9ration et aper\u00E7u \u00BB, puis t\u00E9l\u00E9chargez les fichiers \u00E0 archiver dans la page \u00AB T\u00E9l\u00E9chargements \u00BB.",
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      session$sendCustomMessage("rp-show-page", "results")
      shiny::incProgress(0.1)
    })
  })

  output$generation_status <- shiny::renderUI({
    if (!is.null(last_error())) {
      return(shiny::tags$div(class = "rp-error", paste("\u00C9chec de la g\u00E9n\u00E9ration :", last_error())))
    }
    if (is.null(result())) return(shiny::tags$div(class = "rp-muted", "Aucun r\u00E9sultat de randomisation pour le moment."))
    shiny::tags$div(class = "rp-ok", "Les r\u00E9sultats de la randomisation ont \u00E9t\u00E9 g\u00E9n\u00E9r\u00E9s.")
  })

  output$blinded_table <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$blinded_table, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$unblinded_table <- DT::renderDT({
    shiny::req(result(), input$show_unblinded)
    DT::datatable(result()$unblinded_table, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$balance_table <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$balance_summary$by_group, options = list(dom = "t"))
  })

  output$files_table <- DT::renderDT({
    shiny::req(result())
    files <- result()$files
    DT::datatable(data.frame(name = names(files), path = unlist(files), stringsAsFactors = FALSE), options = list(pageLength = 20, scrollX = TRUE))
  })

  output$hash_table <- DT::renderDT({
    shiny::req(result())
    hashes <- result()$audit$file_hashes
    DT::datatable(data.frame(file = names(hashes), sha256 = unname(hashes), stringsAsFactors = FALSE), options = list(pageLength = 20, scrollX = TRUE))
  })

  output$runtime_info <- shiny::renderUI({
    res <- result()
    rp_info_grid(
      rp_stat_line("Version de R", R.version.string),
      rp_stat_line("Version de randomPlatform", "V1.0"),
      rp_stat_line("G\u00E9n\u00E9rateur al\u00E9atoire (RNG)", paste(RNGkind(), collapse = " / ")),
      rp_stat_line("Graine al\u00E9atoire", input$seed %||% if (is.null(res)) "" else res$design$seed),
      rp_stat_line("Date et heure", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    )
  })

  output$audit_preview <- shiny::renderText({
    shiny::req(result())
    path <- file.path(result()$design$output_dir %||% dirname(result()$files$blinded_site_table), "audit_log.jsonl")
    if (!file.exists(path)) return("Aucun journal d'audit disponible.")
    paste(utils::tail(readLines(path, warn = FALSE), 20), collapse = "\n")
  })

  output$verify_result <- shiny::renderText("")

  shiny::observeEvent(input$verify_repro, {
    shiny::req(result())
    repro_dir <- result()$files$reproducibility_dir
    script_path <- file.path(repro_dir, "reproduce_randomization.R")
    if (is.null(repro_dir) || !dir.exists(repro_dir) || !file.exists(script_path)) {
      output$verify_result <- shiny::renderText("Aucun dossier de reproductibilit\u00E9 n'a \u00E9t\u00E9 g\u00E9n\u00E9r\u00E9 pour ce r\u00E9sultat : v\u00E9rification impossible.")
      return()
    }

    check <- tryCatch({
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(repro_dir)
      repro_env <- new.env(parent = baseenv())
      sys.source(script_path, envir = repro_env)
      if (!exists("res", envir = repro_env, inherits = FALSE)) {
        stop("Le script de reproduction n'a pas cr\u00E9\u00E9 l'objet res.", call. = FALSE)
      }
      repro_res <- get("res", envir = repro_env, inherits = FALSE)
      rp_verify_reproducibility(result()$allocation_table, repro_res$allocation_table)
    }, error = function(e) {
      list(message = paste0("\u00C9chec de la v\u00E9rification de reproductibilit\u00E9 : ", e$message))
    })

    output$verify_result <- shiny::renderText(check$message)
  })

  download_one <- function(name) {
    shiny::downloadHandler(
      filename = function() basename(result()$files[[name]] %||% paste0(name, ".dat")),
      content = function(file) {
        shiny::req(result())
        if (name %in% c("unblinded_master_table", "emergency_envelope", "reproducibility_dir") && !isTRUE(input$confirm_sensitive_download)) {
          stop("Confirmez d'abord que l'utilisateur actuel est autoris\u00E9 \u00E0 t\u00E9l\u00E9charger les fichiers sensibles.", call. = FALSE)
        }
        if (identical(name, "reproducibility_dir")) {
          paths <- list.files(result()$files$reproducibility_dir, full.names = TRUE, recursive = TRUE)
          zip::zipr(file, paths)
        } else {
          rp_copy_download(result()$files[[name]], file)
        }
      }
    )
  }
  output$download_report <- download_one("report")
  output$download_blinded <- download_one("blinded_site_table")
  output$download_unblinded <- download_one("unblinded_master_table")
  output$download_random_pdf <- download_one("random_envelope")
  output$download_emergency_pdf <- download_one("emergency_envelope")
  output$download_repro <- download_one("reproducibility_dir")
  output$download_audit <- shiny::downloadHandler(
    filename = "audit_log.jsonl",
    content = function(file) {
      shiny::req(result())
      rp_copy_download(file.path(dirname(result()$files$blinded_site_table), "audit_log.jsonl"), file)
    }
  )
  output$download_hashes <- download_one("hashes")
  output$download_zip <- shiny::downloadHandler(
    filename = function() paste0("randomPlatform_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      shiny::req(result())
      if (!isTRUE(input$confirm_sensitive_download)) stop("Confirmez d'abord que l'utilisateur actuel est autoris\u00E9 \u00E0 t\u00E9l\u00E9charger les fichiers sensibles.", call. = FALSE)
      root <- dirname(result()$files$blinded_site_table)
      paths <- list.files(root, full.names = TRUE, recursive = TRUE)
      zip::zipr(file, paths)
    }
  )

  min_factors <- shiny::reactive({
    x <- trimws(strsplit(input$min_factors %||% "", ",", fixed = TRUE)[[1]])
    x[nzchar(x)][seq_len(min(6, length(x[nzchar(x)])))]
  })

  output$min_covariate_inputs <- shiny::renderUI({
    f <- min_factors()
    shiny::tagList(lapply(f, function(nm) shiny::textInput(paste0("min_cov_", nm), paste0(nm, " : modalit\u00E9"), value = "")))
  })

  shiny::observeEvent(input$min_start, {
    interventions <- rp_collect_interventions(input)
    alloc <- rp_collect_allocation(input, interventions)
    weights <- rp_parse_numeric_csv(input$min_weights)
    names(weights) <- min_factors()
    sess <- tryCatch(rp_minimization_session(
      project_name = input$project_name,
      protocol_no = input$protocol_no,
      sponsor_name = input$sponsor_name,
      interventions = interventions,
      allocation_ratio = alloc$allocation_ratio %||% stats::setNames(rep(1, nrow(interventions)), interventions$group_id),
      factors = min_factors(),
      weights = weights,
      prob_best = input$min_prob_best,
      seed = input$seed,
      state_file = input$min_state_file,
      output_dir = input$output_dir,
      generate_random_envelope = input$generate_random_envelope,
      generate_emergency_envelope = input$generate_emergency_envelope
    ), error = function(e) e)
    if (inherits(sess, "error")) {
      shiny::showNotification(sess$message, type = "error")
    } else {
      min_session(sess)
      shiny::showNotification("Session de minimisation d\u00E9marr\u00E9e.", type = "message")
    }
  })

  shiny::observeEvent(input$min_assign, {
    shiny::req(min_session())
    cov <- lapply(min_factors(), function(nm) input[[paste0("min_cov_", nm)]])
    names(cov) <- min_factors()
    out <- tryCatch(rp_assign_next(min_session(), input$min_subject_id, cov, input$min_operator, input$min_note), error = function(e) e)
    if (inherits(out, "error")) {
      shiny::showNotification(out$message, type = "error")
    } else {
      min_session(out$session)
      min_last(out)
    }
  })

  output$min_blinded <- DT::renderDT({
    shiny::req(min_last())
    DT::datatable(min_last()$blinded, options = list(dom = "t", scrollX = TRUE))
  })
  output$min_unblinded <- DT::renderDT({
    shiny::req(min_last(), input$min_show_unblinded)
    DT::datatable(min_last()$unblinded, options = list(dom = "t", scrollX = TRUE))
  })
  output$min_assignments <- DT::renderDT({
    shiny::req(min_session())
    DT::datatable(min_session()$assignments, options = list(pageLength = 10, scrollX = TRUE))
  })

  always_live_outputs <- c(
    "intervention_rows",
    "sample_size_hint",
    "center_var_ui",
    "strata_rows",
    "strata_preview",
    "parameter_summary",
    "generation_status",
    "blinded_table",
    "unblinded_table",
    "balance_table",
    "files_table",
    "hash_table",
    "min_covariate_inputs",
    "min_blinded",
    "min_unblinded",
    "min_assignments",
    "runtime_info",
    "verify_result",
    "audit_preview"
  )
  for (id in always_live_outputs) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}
