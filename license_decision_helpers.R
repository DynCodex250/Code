# =============================================================================
# Interne Hilfsfunktionen der Lizenz-Entscheidungsanalyse
#
# Verwendet von build_role_license_drivers(), summarize_duty_license_drivers(),
# summarize_role_license_skus(), analyze_user_duty_usage() und
# recommend_user_license_target().
# =============================================================================

# Platzhalter-Duty fuer Privileges, die der Rolle direkt zugewiesen sind
# (gleiche Bezeichnung wie in get_duty_license_breakdown()).
.DIRECT_DUTY_ID   <- "(direkt)"
.DIRECT_DUTY_NAME <- "(direkt zugewiesen)"

.SKU_SEPARATOR  <- " | "
.LIST_SEPARATOR <- ", "

# Lizenzstatus je Entry Point (build_role_license_drivers()).
.LICENSE_LICENSED          <- "LICENSED"            # mindestens eine SKU deckt den Entry Point
.LICENSE_NOT_ENTITLED      <- "NOT_ENTITLED"        # Lizenzzeilen vorhanden, keine SKU deckt
.LICENSE_NOT_LISTED        <- "NOT_LISTED"          # Entry Point fehlt in der Lizenz-View
.LICENSE_UNKNOWN_PRIVILEGE <- "UNKNOWN_PRIVILEGE"   # Privilege fehlt in der Lizenz-View

# Bezeichnung fuer Entry Points mit ungeklaerter Lizenz in UNMEASURABLE_LICENSES.
.UNKNOWN_LICENSE_LABEL <- "(unbekannt)"

# Evidenz je Benutzer und Duty (analyze_user_duty_usage()).
.EVIDENCE_USED              <- "USED"
.EVIDENCE_NOT_OBSERVED      <- "NOT_OBSERVED"
.EVIDENCE_NOT_MEASURABLE    <- "NOT_MEASURABLE"
.EVIDENCE_NO_USER_TELEMETRY <- "NO_USER_TELEMETRY"

# Empfehlung je Benutzer und SKU (recommend_user_license_target()).
.DECISION_KEEP             <- "KEEP"
.DECISION_REVIEW           <- "REVIEW"
.DECISION_REMOVE_CANDIDATE <- "REMOVE_CANDIDATE"

# Spalten der Lizenztreiber-Tabelle aus build_role_license_drivers().
.driver_columns <- function() {
  c(
    "ROLEIDENTIFIER", "DUTYIDENTIFIER", "DUTYNAME",
    "PRIVILEGEIDENTIFIER", "PRIVILEGENAME",
    "ENTRYPOINT", "ENTRYPOINTTYPE", "MEASURABLE", "ACCESSLEVEL",
    "LICENSE_STATUS", "MIN_SKU", "MIN_PRIORITY", "SKU_GROUP",
    "IS_BASE_LICENSE", "COVERING_SKUS",
    "PRIVILEGE_SKU", "DUTY_SKU", "DRIVES_DUTY"
  )
}

# Lizenzteile je Duty, die die Telemetrie nicht sehen kann.
#
# Ein Lizenzteil ist jede Mindestlizenz der Duty sowie die Gruppe der Entry
# Points mit ungeklaerter Lizenz. Hat ein Lizenzteil keinen messbaren Entry
# Point, laesst sich die Nichtnutzung der Duty nicht belegen: Die Duty bleibt
# als Ganzes zugewiesen oder nicht, also zaehlt ihr schwaechster Teil.
# NOT_LISTED-Entry-Points sind nicht lizenzrelevant und zaehlen hier nicht.
.unmeasurable_licenses_per_duty <- function(drivers) {
  drivers |>
    dplyr::filter(LICENSE_STATUS != .LICENSE_NOT_LISTED) |>
    dplyr::mutate(LICENSE_PART = dplyr::coalesce(MIN_SKU, .UNKNOWN_LICENSE_LABEL)) |>
    dplyr::group_by(DUTYIDENTIFIER, LICENSE_PART) |>
    dplyr::summarise(
      MEASURABLE_COUNT = dplyr::n_distinct(ENTRYPOINT[MEASURABLE]),
      .groups          = "drop"
    ) |>
    dplyr::filter(MEASURABLE_COUNT == 0L) |>
    dplyr::group_by(DUTYIDENTIFIER) |>
    dplyr::summarise(
      UNMEASURABLE_LICENSES = .collapse_sorted(LICENSE_PART),
      .groups               = "drop"
    )
}

.check_required_columns <- function(data, required_columns, arg_name) {

  if (!is.data.frame(data)) {
    stop(arg_name, " muss ein Data Frame sein.")
  }

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      "Folgende Spalten fehlen in ", arg_name, ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  invisible(data)
}

.check_single_role <- function(role_identifier) {

  is_single_role <- is.character(role_identifier) &&
    length(role_identifier) == 1L &&
    !is.na(role_identifier) &&
    nzchar(role_identifier)

  if (!is_single_role) {
    stop("role_identifier muss genau eine Rolle (AOT-Name) enthalten.")
  }

  invisible(role_identifier)
}

# TRUE fuer Base-Lizenzen (Commerce, Finance, SCM, ...). Light-Lizenzen wie
# Team Members oder Operations - Activity haben keine Base-Gruppe.
.is_base_group <- function(sku_group, base_group_pattern) {
  !is.na(sku_group) & grepl(base_group_pattern, sku_group)
}

.max_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  as.numeric(max(x, na.rm = TRUE))
}

.max_date <- function(x) {
  if (all(is.na(x))) {
    return(as.Date(NA))
  }
  max(x, na.rm = TRUE)
}

.min_date <- function(x) {
  if (all(is.na(x))) {
    return(as.Date(NA))
  }
  min(x, na.rm = TRUE)
}

# Teuerste SKU einer Gruppe; bei gleicher Priority die alphabetisch erste.
.sku_with_highest_priority <- function(sku, priority) {
  top_priority <- .max_or_na(priority)
  if (is.na(top_priority)) {
    return(NA_character_)
  }
  sort(sku[!is.na(priority) & priority == top_priority])[[1]]
}

# Eindeutige SKUs, teuerste zuerst.
.collapse_skus_by_priority <- function(sku, priority) {
  keep    <- !is.na(sku)
  ordered <- sku[keep][order(-priority[keep], sku[keep])]
  paste(unique(ordered), collapse = .SKU_SEPARATOR)
}

.collapse_sorted <- function(x, separator = .SKU_SEPARATOR) {
  paste(sort(unique(x[!is.na(x)])), collapse = separator)
}
