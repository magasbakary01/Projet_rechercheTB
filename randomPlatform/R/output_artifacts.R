.rp_write_report <- function(file, project_name, protocol_no, sponsor_name, seed, allocation_table, blinded_table, method, rng_kind, normal_kind, sample_kind) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Project")
  project <- data.frame(
    item = c("Nom de l'\u00E9tude", "Num\u00E9ro de protocole", "Promoteur", "M\u00E9thode de randomisation", "Graine al\u00E9atoire", "Date de g\u00E9n\u00E9ration"),
    value = c(project_name, protocol_no, sponsor_name, method, seed, format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    stringsAsFactors = FALSE
  )
  names(project) <- c("\u00C9l\u00E9ment", "Valeur")
  openxlsx::writeData(wb, "Project", project)

  openxlsx::addWorksheet(wb, "Unblinded Master")
  openxlsx::writeData(wb, "Unblinded Master", allocation_table)

  openxlsx::addWorksheet(wb, "Blinded Site Table")
  openxlsx::writeData(wb, "Blinded Site Table", blinded_table)

  openxlsx::addWorksheet(wb, "Balance")
  balance <- as.data.frame(table(allocation_table$group_id), stringsAsFactors = FALSE)
  names(balance) <- c("group_id", "n")
  openxlsx::writeData(wb, "Balance", balance)

  openxlsx::addWorksheet(wb, "Reproducibility")
  repro <- data.frame(
    item = c("Version de R", "Plateforme", "Graine al\u00E9atoire", "G\u00E9n\u00E9rateur al\u00E9atoire (RNG kind)", "normal.kind", "sample.kind", "Empreinte SHA-256 de la liste"),
    value = c(R.version.string, R.version$platform, seed, rng_kind, normal_kind, sample_kind, .rp_table_hash(allocation_table)),
    stringsAsFactors = FALSE
  )
  names(repro) <- c("\u00C9l\u00E9ment", "Valeur")
  openxlsx::writeData(wb, "Reproducibility", repro)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

.rp_find_pdf_font <- function(cairo = isTRUE(capabilities("cairo"))) {
  # Latin font able to render French accents; no CJK font is required any more.
  if (!isTRUE(cairo)) return("Helvetica")
  if (identical(.Platform$OS.type, "windows")) {
    "Arial"
  } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
    "Helvetica"
  } else {
    "sans"
  }
}

.rp_write_csv_utf8 <- function(df, file) {
  con <- file(file, open = "wb")
  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  close(con)
  con <- file(file, open = "at", encoding = "UTF-8")
  utils::write.csv(df, con, row.names = FALSE, fileEncoding = "")
  close(con)
}

.rp_bilingual <- function(fr, en) {
  paste0(fr, " (", en, ")")
}

.rp_page_header <- function(title, title_en, envelope_no, accent = "#176f6b") {
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot.new()
  rect(0, 0, 1, 1, col = "#ffffff", border = NA)
  rect(0.035, 0.86, 0.965, 0.965, col = accent, border = NA)
  title_cex <- if (nchar(title) > 10) 0.92 else 1.20
  en_cex <- if (nchar(title_en) > 28) 0.56 else 0.72
  text(0.055, 0.928, title, cex = title_cex, font = 2, col = "white", adj = 0)
  text(0.055, 0.878, title_en, cex = en_cex, font = 2, col = "white", adj = 0)
  text(0.945, 0.922, envelope_no, cex = 1.03, font = 2, adj = 1, col = "white")
  rect(0.035, 0.84, 0.965, 0.852, col = accent, border = NA)
}

.rp_wrap <- function(x, width = 42) {
  x <- as.character(x)
  paste(strwrap(x, width = width), collapse = "\n")
}

.rp_draw_kv_table <- function(rows, y_top = 0.79, row_h = 0.061, label_w = 0.35, accent = "#176f6b", value_cex = 0.78, label_cex = 0.62) {
  x0 <- 0.07
  x1 <- 0.93
  label_x1 <- x0 + label_w
  for (i in seq_len(nrow(rows))) {
    y1 <- y_top - (i - 1) * row_h
    y0 <- y1 - row_h
    rect(x0, y0, label_x1, y1, col = "#eef5f5", border = "#c9d7da")
    rect(label_x1, y0, x1, y1, col = "#fbfdfd", border = "#c9d7da")
    text(x0 + 0.012, y0 + row_h / 2, .rp_wrap(rows$label[i], floor(25 / label_cex)), adj = c(0, 0.5), cex = label_cex, font = 2, col = accent)
    text(label_x1 + 0.014, y0 + row_h / 2, .rp_wrap(rows$value[i], 44), adj = c(0, 0.5), cex = value_cex, col = "#172326")
  }
  invisible(y_top - nrow(rows) * row_h)
}

.rp_draw_note_box <- function(
  title, lines, y_top, height, border = "#176f6b", fill = "#f8fbfb",
  col = "#172326", cex = 0.73, title_cex = 0.78, line_gap = 0.03,
  paragraph_gap = 0.007, wrap_width = 72
) {
  x0 <- 0.07
  x1 <- 0.93
  rect(x0, y_top - height, x1, y_top, col = fill, border = border, lwd = 1.6)
  text(x0 + 0.018, y_top - 0.035, title, adj = 0, cex = title_cex, font = 2, col = border)
  if (length(lines) > 0) {
    y <- y_top - 0.075
    for (line in lines) {
      wrapped <- strwrap(as.character(line), width = wrap_width)
      if (length(wrapped) == 0) wrapped <- ""
      for (part in wrapped) {
        text(x0 + 0.02, y, part, adj = 0, cex = cex, col = col)
        y <- y - line_gap
      }
      y <- y - paragraph_gap
    }
  }
}

.rp_note_box_required_height <- function(
  lines,
  line_gap = 0.03,
  paragraph_gap = 0.007,
  wrap_width = 72,
  top_padding = 0.075,
  bottom_padding = 0.02
) {
  if (length(lines) == 0) {
    return(top_padding + bottom_padding)
  }

  wrapped_counts <- vapply(lines, function(line) {
    wrapped <- strwrap(as.character(line), width = wrap_width)
    max(1L, length(wrapped))
  }, integer(1))

  top_padding + sum(wrapped_counts) * line_gap + length(lines) * paragraph_gap + bottom_padding
}

.rp_emergency_insert_sensitive_note_lines <- function() {
  c(.rp_bilingual("Cette page r\u00E9v\u00E8le le traitement attribu\u00E9 : consultation r\u00E9serv\u00E9e \u00E0 une personne autoris\u00E9e, en cas de lev\u00E9e d'aveugle d'urgence", "This page contains allocation information and is restricted to authorized emergency unblinding use"))
}

.rp_emergency_insert_sensitive_note_spec <- function(y_after, gap = 0.035) {
  lines <- .rp_emergency_insert_sensitive_note_lines()
  line_gap <- 0.022
  paragraph_gap <- 0.003
  wrap_width <- 115
  height <- max(
    0.11,
    .rp_note_box_required_height(
      lines,
      line_gap = line_gap,
      paragraph_gap = paragraph_gap,
      wrap_width = wrap_width
    )
  )

  list(
    title = .rp_bilingual("Information sensible", "Sensitive information"),
    lines = lines,
    y_top = min(0.24, y_after - gap),
    height = height,
    border = "#b42318",
    fill = "#fff0ef",
    col = "#7f2119",
    cex = 0.56,
    title_cex = 0.62,
    line_gap = line_gap,
    paragraph_gap = paragraph_gap,
    wrap_width = wrap_width
  )
}

.rp_envelope_strata_cols <- function(allocation) {
  known <- c(
    "group_id", "intervention_name", "blind_label", "allocation_label",
    "sequence_no", "method", "envelope_no", "stratum_id", "random_no",
    "block_id", "block_size", "global_block_id", "drug_code"
  )
  cols <- setdiff(names(allocation), known)
  cols[vapply(cols, function(nm) {
    values <- allocation[[nm]]
    is.atomic(values) && any(nzchar(trimws(as.character(values[!is.na(values)]))))
  }, logical(1))]
}

.rp_stratum_display <- function(row, strata_cols) {
  strata_cols <- strata_cols[strata_cols %in% names(row)]
  if (length(strata_cols) == 0) return(NULL)
  values <- vapply(strata_cols, function(nm) {
    value <- as.character(row[[nm]][1])
    if (is.na(value) || !nzchar(trimws(value))) return(NA_character_)
    paste0(nm, ":", value)
  }, character(1))
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NULL)
  paste(values, collapse = ";")
}

.rp_envelope_common_rows <- function(row, project_name, protocol_no, sponsor_name, center_col, stratum_display = NULL) {
  rows <- data.frame(
    label = c(
      .rp_bilingual("Nom de l'\u00E9tude", "Project name"),
      .rp_bilingual("Num\u00E9ro de protocole", "Protocol number"),
      .rp_bilingual("Nom du promoteur", "Sponsor"),
      .rp_bilingual("Num\u00E9ro d'ordre de l'enveloppe", "Envelope serial number")
    ),
    value = c(project_name, protocol_no, sponsor_name, as.character(row$envelope_no)),
    stringsAsFactors = FALSE
  )
  if (!is.null(center_col)) {
    rows <- rbind(
      rows[seq_len(3), , drop = FALSE],
      data.frame(label = .rp_bilingual("Code du centre / \u00E9tablissement", "Center/institution"), value = as.character(row[[center_col]]), stringsAsFactors = FALSE),
      rows[4, , drop = FALSE]
    )
  }
  if (!is.null(stratum_display) && nzchar(stratum_display)) {
    rows <- rbind(
      rows,
      data.frame(label = .rp_bilingual("Strate", "Stratification"), value = stratum_display, stringsAsFactors = FALSE)
    )
  }
  rows
}

.rp_draw_footer <- function(kind, envelope_no) {
  text(0.5, 0.025, paste0(kind, " | ", .rp_bilingual("Num\u00E9ro d'ordre", "Serial"), ": ", envelope_no), cex = 0.58, col = "#607179")
}

.rp_pdf <- function(file, font_family = NULL) {
  page_width <- 210 / 25.4
  page_height <- 148 / 25.4
  if (is.null(font_family) || !nzchar(font_family)) {
    font_family <- .rp_find_pdf_font()
  }
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(filename = file, width = page_width, height = page_height, family = font_family %||% "sans", onefile = TRUE)
  } else {
    grDevices::pdf(file, width = page_width, height = page_height, family = font_family %||% "sans", useDingbats = FALSE)
  }
}

.rp_write_random_envelopes <- function(file, project_name, protocol_no, sponsor_name, allocation, center_col, font_family = NULL) {
  .rp_pdf(file, font_family)
  on.exit(grDevices::dev.off(), add = TRUE)
  strata_cols <- .rp_envelope_strata_cols(allocation)
  for (i in seq_len(nrow(allocation))) {
    row <- allocation[i, , drop = FALSE]
    stratum_display <- .rp_stratum_display(row, strata_cols)

    .rp_page_header("Enveloppe de randomisation - couverture", "Random Envelope Cover", row$envelope_no)
    common_rows <- .rp_envelope_common_rows(row, project_name, protocol_no, sponsor_name, center_col, stratum_display)
    y_after <- .rp_draw_kv_table(common_rows, y_top = 0.78, row_h = 0.061)
    cover_lines <- c(
      .rp_bilingual("Ouvrir les enveloppes dans l'ordre des inclusions", "Open sequentially according to enrollment order"),
      .rp_bilingual("V\u00E9rifier l'\u00E9ligibilit\u00E9 du participant avant l'ouverture", "Confirm participant eligibility before opening"),
      .rp_bilingual("La couverture ne doit indiquer ni le groupe attribu\u00E9 ni la lev\u00E9e de l'aveugle", "This cover must not display allocation or unblinding information")
    )
    .rp_draw_note_box(
      .rp_bilingual("Consignes", "Instruction"),
      cover_lines,
      y_top = min(0.36, y_after - 0.05),
      height = max(0.17, .rp_note_box_required_height(cover_lines, line_gap = 0.024, paragraph_gap = 0.003, wrap_width = 118)),
      cex = 0.55,
      title_cex = 0.68,
      line_gap = 0.024,
      paragraph_gap = 0.003,
      wrap_width = 118
    )
    .rp_draw_footer("Enveloppe de randomisation - couverture", row$envelope_no)

    .rp_page_header("Enveloppe de randomisation - feuillet int\u00E9rieur", "Random Envelope Insert", row$envelope_no)
    insert_rows <- rbind(
      common_rows,
      data.frame(label = .rp_bilingual("Num\u00E9ro de traitement / d'intervention", "Drug/intervention code"), value = as.character(row$drug_code), stringsAsFactors = FALSE)
    )
    .rp_draw_kv_table(insert_rows, y_top = 0.78, row_h = 0.066, value_cex = 0.86)
    .rp_draw_footer("Enveloppe de randomisation - feuillet int\u00E9rieur", row$envelope_no)
  }
}

.rp_write_emergency_envelopes <- function(file, project_name, protocol_no, sponsor_name, allocation, center_col, font_family = NULL) {
  .rp_pdf(file, font_family)
  on.exit(grDevices::dev.off(), add = TRUE)
  strata_cols <- .rp_envelope_strata_cols(allocation)
  for (i in seq_len(nrow(allocation))) {
    row <- allocation[i, , drop = FALSE]
    stratum_display <- .rp_stratum_display(row, strata_cols)

    .rp_page_header("Enveloppe d'urgence - couverture", "Emergency Unblinding Envelope Cover", row$envelope_no, accent = "#b42318")
    .rp_draw_note_box(
      .rp_bilingual("Avertissement", "Warning"),
      c(
        .rp_bilingual("R\u00E9serv\u00E9e \u00E0 la lev\u00E9e d'aveugle d'urgence", "Emergency unblinding use only"),
        .rp_bilingual("Ne pas ouvrir en dehors d'une urgence m\u00E9dicale", "Do not open unless medically necessary"),
        .rp_bilingual("Toute ouverture vaut lev\u00E9e de l'aveugle pour ce participant", "Opening means the participant has been unblinded")
      ),
      y_top = 0.805,
      height = 0.18,
      border = "#b42318",
      fill = "#fff0ef",
      col = "#7f2119",
      cex = 0.54,
      title_cex = 0.66,
      line_gap = 0.024,
      paragraph_gap = 0.003,
      wrap_width = 120
    )
    common_rows <- .rp_envelope_common_rows(row, project_name, protocol_no, sponsor_name, center_col, stratum_display)
    .rp_draw_kv_table(common_rows, y_top = 0.595, row_h = 0.047, accent = "#b42318", value_cex = 0.66, label_cex = 0.47)
    .rp_draw_note_box(
      .rp_bilingual("Ouverture de l'enveloppe (\u00E0 remplir \u00E0 la main sur site)", "Opening record, handwritten on site"),
      c(
        paste0(.rp_bilingual("Motif de l'ouverture", "Reason for opening"), ": ________________________________"),
        paste0(.rp_bilingual("Signature de la personne qui ouvre, date et heure", "Opened by/date time"), ": __________________"),
        paste0(.rp_bilingual("Signature du t\u00E9moin, date et heure", "Witness/date time"), ": ________________")
      ),
      y_top = 0.285,
      height = 0.16,
      border = "#b42318",
      fill = "#ffffff",
      col = "#172326",
      cex = 0.38,
      title_cex = 0.62,
      line_gap = 0.022,
      paragraph_gap = 0.002,
      wrap_width = 94
    )
    .rp_draw_footer("Enveloppe d'urgence - couverture", row$envelope_no)

    .rp_page_header("Enveloppe d'urgence - feuillet int\u00E9rieur", "Emergency Unblinding Envelope Insert", row$envelope_no, accent = "#b42318")
    insert_rows <- rbind(
      common_rows,
      data.frame(label = .rp_bilingual("Num\u00E9ro de randomisation du participant", "Participant randomization number"), value = as.character(row$random_no), stringsAsFactors = FALSE),
      data.frame(label = .rp_bilingual("Num\u00E9ro de traitement / d'intervention", "Drug/intervention code"), value = as.character(row$drug_code), stringsAsFactors = FALSE),
      data.frame(label = .rp_bilingual("Traitement attribu\u00E9", "Treatment/intervention allocation"), value = paste0(row$group_id, " - ", row$intervention_name), stringsAsFactors = FALSE)
    )
    y_after <- .rp_draw_kv_table(insert_rows, y_top = 0.78, row_h = 0.055, accent = "#b42318", value_cex = 0.73)
    note_spec <- .rp_emergency_insert_sensitive_note_spec(y_after)
    .rp_draw_note_box(
      note_spec$title,
      note_spec$lines,
      y_top = note_spec$y_top,
      height = note_spec$height,
      border = note_spec$border,
      fill = note_spec$fill,
      col = note_spec$col,
      cex = note_spec$cex,
      title_cex = note_spec$title_cex,
      line_gap = note_spec$line_gap,
      paragraph_gap = note_spec$paragraph_gap,
      wrap_width = note_spec$wrap_width
    )
    .rp_draw_footer("Enveloppe d'urgence - feuillet int\u00E9rieur", row$envelope_no)
  }
}

.rp_package_function_names <- function() {
  ns <- environment(rp_randomize)
  object_names <- ls(ns, all.names = TRUE)
  function_names <- object_names[vapply(object_names, function(name) {
    exists(name, envir = ns, inherits = FALSE) && is.function(get(name, envir = ns, inherits = FALSE))
  }, logical(1))]
  exported <- intersect(getNamespaceExports("randomPlatform"), function_names)
  c(sort(exported), sort(setdiff(function_names, exported)))
}

.rp_repro_assignment_name <- function(name) {
  reserved <- c(
    "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
    "TRUE", "FALSE", "NULL", "Inf", "NaN",
    "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_"
  )
  if (make.names(name) == name && !name %in% reserved) {
    return(name)
  }
  paste0("`", gsub("`", "\\`", name, fixed = TRUE), "`")
}

.rp_repro_deparse_function <- function(name, fn) {
  lines <- deparse(fn, width.cutoff = 500L, control = "keepInteger")
  lines[1] <- paste0(.rp_repro_assignment_name(name), " <- ", lines[1])
  c(paste0("# ---- ", name, " ----"), lines, "")
}

.rp_repro_function_source_lines <- function(function_names = .rp_package_function_names()) {
  ns <- environment(rp_randomize)
  unlist(lapply(function_names, function(name) {
    .rp_repro_deparse_function(name, get(name, envir = ns, inherits = FALSE))
  }), use.names = FALSE)
}

.rp_write_reproducibility_bundle <- function(
  dir, project_name, protocol_no, sponsor_name, interventions, method, seed, n_total,
  n_per_group, allocation_ratio, strata, stratum_n, center_var, block_sizes, block_size_probs,
  random_no_prefix, random_no_width, random_no_by_center, code_prefix, code_width, code_random_digits,
  code_random_range, code_by_group, envelope_no_prefix, envelope_no_width, language,
  rng_kind, normal_kind, sample_kind, allocation_table, standalone_reproducibility_code, rng_start
) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  params <- list(
    project_name = project_name,
    protocol_no = protocol_no,
    sponsor_name = sponsor_name,
    interventions = interventions,
    method = method,
    seed = seed,
    n_total = n_total,
    n_per_group = as.list(n_per_group),
    allocation_ratio = as.list(allocation_ratio),
    strata = strata,
    stratum_n = if (is.null(stratum_n)) NULL else as.list(stratum_n),
    center_var = center_var,
    block_sizes = block_sizes,
    block_size_probs = block_size_probs,
    random_no_prefix = random_no_prefix,
    random_no_width = random_no_width,
    random_no_by_center = random_no_by_center,
    code_prefix = if (length(code_prefix) > 1L) as.list(code_prefix) else code_prefix,
    code_width = code_width,
    code_random_digits = code_random_digits,
    code_random_range = code_random_range,
    code_by_group = code_by_group,
    envelope_no_prefix = envelope_no_prefix,
    envelope_no_width = envelope_no_width,
    language = language,
    rng_kind = rng_kind,
    normal_kind = normal_kind,
    sample_kind = sample_kind,
    table_hash = .rp_table_hash(allocation_table)
  )
  jsonlite::write_json(params, file.path(dir, "parameters.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  utils::capture.output(utils::sessionInfo(), file = file.path(dir, "session_info.txt"))
  saveRDS(rng_start, file.path(dir, "rng_state.rds"))
  .rp_write_csv_utf8(allocation_table, file.path(dir, "original_randomization_table.csv"))

  params_path <- file.path(dir, "parameters.json")
  script_path <- file.path(dir, "reproduce_randomization.R")
  function_lines <- .rp_repro_function_source_lines()

  call_lines <- c(
    "# randomPlatform reproducibility script",
    "# This script embeds the package functions used to regenerate the randomization table.",
    "library(jsonlite)",
    "library(digest)",
    "resolve_bundle_dir <- function() {",
    "  for (cand in c('.', 'reproducibility')) {",
    "    path <- file.path(cand, 'parameters.json')",
    "    if (file.exists(path)) return(dirname(normalizePath(path, winslash = '/', mustWork = TRUE)))",
    "  }",
    "  calls <- sys.calls()",
    "  parents <- sys.parents()",
    "  for (idx in rev(seq_along(calls))) {",
    "    call <- calls[[idx]]",
    "    fun <- paste(deparse(call[[1L]]), collapse = '')",
    "    if (!grepl('(^|::)(source|sys.source)$', fun)) next",
    "    nms <- names(call)",
    "    arg <- if (!is.null(nms) && 'file' %in% nms) call[[which(nms == 'file')[1L]]] else if (length(call) >= 2L) call[[2L]] else NULL",
    "    if (is.null(arg)) next",
    "    for (frame_idx in unique(c(parents[idx], idx))) {",
    "      if (is.na(frame_idx) || frame_idx < 1L) next",
    "      path <- tryCatch(eval(arg, envir = sys.frame(frame_idx)), error = function(e) NULL)",
    "      if (is.character(path) && length(path) == 1L && nzchar(path)) return(dirname(normalizePath(path, winslash = '/', mustWork = TRUE)))",
    "    }",
    "  }",
    "  stop('Could not locate the reproducibility bundle. Run source() from the bundle root or reproducibility directory.', call. = FALSE)",
    "}",
    "bundle_dir <- resolve_bundle_dir()",
    "params_path <- file.path(bundle_dir, 'parameters.json')",
    "params <- jsonlite::read_json(params_path, simplifyVector = TRUE)",
    "expected_names <- c('project_name','protocol_no','sponsor_name','interventions','method','seed','n_total','n_per_group','allocation_ratio','strata','stratum_n','center_var','block_sizes','block_size_probs','random_no_prefix','random_no_width','random_no_by_center','code_prefix','code_width','code_random_digits','code_random_range','code_by_group','envelope_no_prefix','envelope_no_width','language','rng_kind','normal_kind','sample_kind')",
    "for (nm in expected_names[!expected_names %in% names(params)]) params[[nm]] <- NULL",
    "params$n_per_group <- if (is.null(params$n_per_group)) NULL else unlist(params$n_per_group, use.names = TRUE)",
    "params$allocation_ratio <- if (is.null(params$allocation_ratio)) NULL else unlist(params$allocation_ratio, use.names = TRUE)",
    "params$stratum_n <- if (is.null(params$stratum_n)) NULL else unlist(params$stratum_n, use.names = TRUE)",
    "if (!is.null(params$stratum_n)) params$stratum_n <- stats::setNames(as.integer(params$stratum_n), names(params$stratum_n))",
    "if (is.list(params$code_prefix)) params$code_prefix <- unlist(params$code_prefix, use.names = TRUE)",
    "call_params <- params[expected_names]",
    "names(call_params) <- expected_names",
    "res <- do.call(rp_randomize, c(call_params, list(generate_report = FALSE, generate_random_envelope = FALSE, generate_emergency_envelope = FALSE, generate_reproducibility = FALSE, output_dir = tempdir())))",
    "hash <- digest::digest(res$allocation_table, algo = 'sha256')",
    "stopifnot(identical(hash, params$table_hash))",
    "message('Reproducibility check passed: ', hash)"
  )

  code <- if (isTRUE(standalone_reproducibility_code)) {
    c(function_lines, call_lines)
  } else {
    c(
      "# randomPlatform reproducibility script",
      "library(jsonlite)",
      "library(digest)",
      paste0("RNGkind(kind = ", deparse(rng_kind), ", normal.kind = ", deparse(normal_kind), ", sample.kind = ", deparse(sample_kind), ")"),
      paste0("set.seed(", as.integer(seed), ")"),
      "if (!requireNamespace('randomPlatform', quietly = TRUE)) stop('Install randomPlatform before running this package-based reproducibility script.')",
      "resolve_bundle_dir <- function() {",
      "  for (cand in c('.', 'reproducibility')) {",
      "    path <- file.path(cand, 'parameters.json')",
      "    if (file.exists(path)) return(dirname(normalizePath(path, winslash = '/', mustWork = TRUE)))",
      "  }",
      "  calls <- sys.calls()",
      "  parents <- sys.parents()",
      "  for (idx in rev(seq_along(calls))) {",
      "    call <- calls[[idx]]",
      "    fun <- paste(deparse(call[[1L]]), collapse = '')",
      "    if (!grepl('(^|::)(source|sys.source)$', fun)) next",
      "    nms <- names(call)",
      "    arg <- if (!is.null(nms) && 'file' %in% nms) call[[which(nms == 'file')[1L]]] else if (length(call) >= 2L) call[[2L]] else NULL",
      "    if (is.null(arg)) next",
      "    for (frame_idx in unique(c(parents[idx], idx))) {",
      "      if (is.na(frame_idx) || frame_idx < 1L) next",
      "      path <- tryCatch(eval(arg, envir = sys.frame(frame_idx)), error = function(e) NULL)",
      "      if (is.character(path) && length(path) == 1L && nzchar(path)) return(dirname(normalizePath(path, winslash = '/', mustWork = TRUE)))",
      "    }",
      "  }",
      "  stop('Could not locate the reproducibility bundle. Run source() from the bundle root or reproducibility directory.', call. = FALSE)",
      "}",
      "bundle_dir <- resolve_bundle_dir()",
      "params_path <- file.path(bundle_dir, 'parameters.json')",
      "params <- jsonlite::read_json(params_path, simplifyVector = TRUE)",
      "expected_names <- c('project_name','protocol_no','sponsor_name','interventions','method','seed','n_total','n_per_group','allocation_ratio','strata','stratum_n','center_var','block_sizes','block_size_probs','random_no_prefix','random_no_width','random_no_by_center','code_prefix','code_width','code_random_digits','code_random_range','code_by_group','envelope_no_prefix','envelope_no_width','language','rng_kind','normal_kind','sample_kind')",
      "for (nm in expected_names[!expected_names %in% names(params)]) params[[nm]] <- NULL",
      "params$n_per_group <- if (is.null(params$n_per_group)) NULL else unlist(params$n_per_group, use.names = TRUE)",
      "params$allocation_ratio <- if (is.null(params$allocation_ratio)) NULL else unlist(params$allocation_ratio, use.names = TRUE)",
      "params$stratum_n <- if (is.null(params$stratum_n)) NULL else unlist(params$stratum_n, use.names = TRUE)",
      "if (!is.null(params$stratum_n)) params$stratum_n <- stats::setNames(as.integer(params$stratum_n), names(params$stratum_n))",
      "if (is.list(params$code_prefix)) params$code_prefix <- unlist(params$code_prefix, use.names = TRUE)",
      "call_params <- params[expected_names]",
      "names(call_params) <- expected_names",
      "res <- do.call(randomPlatform::rp_randomize, c(call_params, list(generate_report = FALSE, generate_random_envelope = FALSE, generate_emergency_envelope = FALSE, generate_reproducibility = FALSE, output_dir = tempdir())))",
      "hash <- digest::digest(res$allocation_table, algo = 'sha256')",
      "stopifnot(identical(hash, params$table_hash))",
      "message('Reproducibility check passed: ', hash)"
    )
  }
  writeLines(code, script_path, useBytes = TRUE)
  writeLines(c(
    "# Algorithm snapshot",
    "# The active implementation is stored in the package R sources.",
    "# Use the package version and SHA-256 hashes recorded with this output bundle."
  ), file.path(dir, "algorithm_snapshot.R"), useBytes = TRUE)
}

#' Verify a reproduced randomization table against an original table or hash.
#'
#' This helper compares SHA-256 hashes of two tables and reports whether they
#' are identical. Use it when you want a strict, hash-based reproducibility
#' check rather than a semantic or tolerance-based comparison.
#'
#' @param original_table Optional data frame or table-like object. Defaults to
#'   `NULL`. Represents the original randomization result and is required when
#'   `original_hash` is `NULL`. The object should be the same shape as the
#'   reproduced result so the hash is computed on the full table contents.
#' @param reproduced_table Data frame or table-like object to verify. Defaults
#'   to `NULL`, but a value is required at call time. It is always hashed and
#'   compared against the original reference.
#' @param original_hash Optional character scalar. Defaults to `NULL`.
#'   Represents the precomputed SHA-256 hash of the original table. If supplied,
#'   `original_table` is not needed. If `NULL`, `original_table` must be
#'   provided so the hash can be derived.
#'
#' @return A list with four elements:
#'   \describe{
#'     \item{identical}{Logical `TRUE` when the hashes match exactly; `FALSE`
#'       otherwise.}
#'     \item{original_hash}{Character scalar containing the original SHA-256
#'       hash, either supplied directly or computed from `original_table`.}
#'     \item{reproduced_hash}{Character scalar containing the SHA-256 hash of
#'       `reproduced_table`.}
#'     \item{message}{Human-readable summary of the comparison result.}
#'   }
#'
#' @details
#' The function supports two verification modes: (1) supply `original_table`
#' and `reproduced_table`, or (2) supply `original_hash` and
#' `reproduced_table`. In both cases, the comparison is performed by exact
#' SHA-256 hash equality; no semantic equivalence, sorting, rounding, or other
#' tolerance rules are applied.
#'
#' @seealso [rp_randomize()]
#'
#' @examples
#' tmp <- tempdir()
#' set.seed(20260528)
#' res1 <- rp_randomize(
#'   project_name = "Demo Trial",
#'   protocol_no = "P-001",
#'   sponsor_name = "Demo Sponsor",
#'   interventions = c(A = "Intervention A", B = "Intervention B"),
#'   method = "simple",
#'   seed = 123,
#'   n_total = 8,
#'   allocation_ratio = c(A = 1, B = 1),
#'   generate_report = FALSE,
#'   generate_reproducibility = FALSE,
#'   output_dir = tmp
#' )
#' set.seed(20260528)
#' res2 <- rp_randomize(
#'   project_name = "Demo Trial",
#'   protocol_no = "P-001",
#'   sponsor_name = "Demo Sponsor",
#'   interventions = c(A = "Intervention A", B = "Intervention B"),
#'   method = "simple",
#'   seed = 123,
#'   n_total = 8,
#'   allocation_ratio = c(A = 1, B = 1),
#'   generate_report = FALSE,
#'   generate_reproducibility = FALSE,
#'   output_dir = tmp
#' )
#' rp_verify_reproducibility(res1$allocation_table, res2$allocation_table)
#'
#' @export
rp_verify_reproducibility <- function(original_table = NULL, reproduced_table = NULL, original_hash = NULL) {
  if (is.null(original_hash)) {
    if (is.null(original_table)) stop("Provide original_table or original_hash.", call. = FALSE)
    original_hash <- .rp_table_hash(original_table)
  }
  if (is.null(reproduced_table)) stop("Provide reproduced_table.", call. = FALSE)
  reproduced_hash <- .rp_table_hash(reproduced_table)
  list(
    identical = identical(original_hash, reproduced_hash),
    original_hash = original_hash,
    reproduced_hash = reproduced_hash,
    message = if (identical(original_hash, reproduced_hash)) "Reproduced result is identical to the original randomization result." else "Reproduced result differs from the original randomization result."
  )
}
