#' Lizenzanforderung pro Duty einer Rolle
#'
#' Zeigt für jede Duty einer Rolle, welche Lizenzkategorie sie mindestens
#' erfordert — basierend auf den Privileges die die Duty enthält und deren
#' Einträgen in \code{LICENSINGPRIVILEGEREQUIREMENTSDETAILEDVIEW}.
#'
#' @details
#'
#' **Funktionsprinzip**
#'
#' Eine Duty enthält eine oder mehrere Privileges. Jedes Privilege ist in
#' \code{LICENSINGPRIVILEGEREQUIREMENTSDETAILEDVIEW} einer oder mehreren
#' Lizenz-SKUs zugeordnet (\code{ENTITLED = 1}). Die Lizenzkategorie einer
#' Duty ist die höchste Anforderung aller ihrer Privileges:
#'
#' \preformatted{
#' Activity   < Finance/SCM < Commerce < Unbekannt
#' }
#'
#' \describe{
#'   \item{\code{Activity}}{
#'     Alle Privileges der Duty sind durch eine Activity-SKU abgedeckt
#'     (\code{ENTITLED = 1} für einen SKU der \code{activity_pattern} entspricht).
#'   }
#'   \item{\code{Commerce}}{
#'     Mindestens ein Privilege ist nicht Activity-kompatibel, aber durch
#'     einen Commerce-SKU abgedeckt.
#'   }
#'   \item{\code{Finance/SCM}}{
#'     Mindestens ein Privilege ist nicht Activity-kompatibel und nicht
#'     Commerce-spezifisch.
#'   }
#'   \item{\code{Unbekannt}}{
#'     Privilege hat keinen Eintrag in der Lizenz-View (z.B. WIBU-Custom-Privilege).
#'     Wird konservativ behandelt (höchste Priorität bei Aggregation).
#'   }
#' }
#'
#' **Direkte Privileges (ohne Duty)**
#'
#' Privileges die der Rolle direkt (ohne Duty-Umweg) zugewiesen sind,
#' erscheinen in einer eigenen Zeile mit \code{DUTYIDENTIFIER = "(direkt)"}.
#'
#' **Typischer Anwendungsfall: Rollenteilung**
#'
#' \preformatted{
#' breakdown <- get_duty_license_breakdown(cnn, "_WIBU_VERKAUF_INNENDIENST_MITARBEITER")
#'
#' # Welche Duties können in die Activity-Rolle?
#' breakdown |> dplyr::filter(LICENSE_CATEGORY == "Activity")
#'
#' # Welche Duties benötigen Commerce?
#' breakdown |> dplyr::filter(LICENSE_CATEGORY == "Commerce")
#' }
#'
#' @param con
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param role_identifier
#' AOT-Name der Rolle (\code{ROLEIDENTIFIER}).
#' Wird an \code{get_sql_security_hierarchy(role_identifiers = ...)} weitergegeben.
#'
#' @param activity_pattern
#' Regex-Muster für Activity-SKU-Namen.
#' Standard: \code{"Activity"}.
#'
#' @param commerce_pattern
#' Regex-Muster für Commerce-SKU-Namen.
#' Standard: \code{"Commerce"}.
#'
#' @return
#'
#' Tibble mit einer Zeile pro Duty (plus ggf. eine Zeile für direkte
#' Privileges):
#'
#' \describe{
#'   \item{\code{DUTYIDENTIFIER}}{AOT-Name der Duty (\code{"(direkt)"} für
#'     Privileges ohne Duty).}
#'   \item{\code{DUTYNAME}}{Anzeigename der Duty (aus \code{SECURITYDUTY}).}
#'   \item{\code{LICENSE_CATEGORY}}{Höchste Lizenzanforderung der Duty:
#'     \code{"Activity"}, \code{"Finance/SCM"}, \code{"Commerce"} oder
#'     \code{"Unbekannt"}.}
#'   \item{\code{LICENSE_DETAIL}}{Alle vorkommenden Lizenzkategorien der
#'     Privileges innerhalb der Duty (kommagetrennt).}
#'   \item{\code{PRIVILEGE_COUNT}}{Anzahl der Privileges in der Duty.}
#' }
#'
#' Sortiert nach Lizenzkategorie (höchste zuerst), dann alphabetisch nach
#' \code{DUTYIDENTIFIER}.
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' # Lizenz-Breakdown für eine WIBU-Rolle
#' breakdown <- get_duty_license_breakdown(
#'   cnn,
#'   "_WIBU_VERKAUF_INNENDIENST_MITARBEITER"
#' )
#'
#' # Duties für die Activity-Rolle (Leserechte)
#' activity_duties <- breakdown |>
#'   dplyr::filter(LICENSE_CATEGORY == "Activity") |>
#'   dplyr::pull(DUTYIDENTIFIER)
#'
#' # Duties für die Commerce-Rolle
#' commerce_duties <- breakdown |>
#'   dplyr::filter(LICENSE_CATEGORY == "Commerce") |>
#'   dplyr::pull(DUTYIDENTIFIER)
#'
#' # Duties für die SCM-Rolle (Rest)
#' scm_duties <- breakdown |>
#'   dplyr::filter(LICENSE_CATEGORY == "Finance/SCM") |>
#'   dplyr::pull(DUTYIDENTIFIER)
#'
#' }
#'
#' @seealso
#' \code{\link{get_sql_security_hierarchy}}
#' \code{\link{get_license_requirements}}
#' \code{\link{get_role_license_assignments_compact}}
#' \code{\link{create_role_xml}}
#'
#' @export
get_duty_license_breakdown <- function(
  con,
  role_identifier,
  activity_pattern = "Activity",
  commerce_pattern = "Commerce"
) {

  #====================================================
  # 1. Sicherheitshierarchie laden
  #====================================================
  hier <- get_sql_security_hierarchy(con, role_identifiers = role_identifier)

  if (nrow(hier) == 0L) {
    stop("Keine Hierarchie gefunden fuer Rolle: '", role_identifier, "'")
  }

  #====================================================
  # 2. Duty → Privilege und direkte Privilege-Paare
  #====================================================
  # Duties mit Privilege
  duty_priv <- hier |>
    dplyr::filter(
      !is.na(DUTYIDENTIFIER),    DUTYIDENTIFIER    != "",
      !is.na(PRIVILEGEIDENTIFIER), PRIVILEGEIDENTIFIER != ""
    ) |>
    dplyr::distinct(DUTYIDENTIFIER, PRIVILEGEIDENTIFIER)

  # Privileges direkt auf der Rolle (ohne Duty)
  direct_priv <- hier |>
    dplyr::filter(
      is.na(DUTYIDENTIFIER) | DUTYIDENTIFIER == "",
      !is.na(PRIVILEGEIDENTIFIER), PRIVILEGEIDENTIFIER != ""
    ) |>
    dplyr::distinct(PRIVILEGEIDENTIFIER) |>
    dplyr::mutate(DUTYIDENTIFIER = "(direkt)")

  all_pairs <- dplyr::bind_rows(duty_priv, direct_priv)

  if (nrow(all_pairs) == 0L) {
    message("Keine Privileges gefunden fuer Rolle '", role_identifier, "'.")
    return(.empty_breakdown())
  }

  all_priv_ids <- unique(all_pairs$PRIVILEGEIDENTIFIER)

  #====================================================
  # 3. Duty-Namen aus SECURITYDUTY
  #====================================================
  duty_ids_real <- unique(duty_priv$DUTYIDENTIFIER)
  duty_names <- tibble::tibble(DUTYIDENTIFIER = character(), DUTYNAME = character())

  if (length(duty_ids_real) > 0L) {
    ids_sql <- paste0("'", gsub("'", "''", duty_ids_real), "'", collapse = ", ")
    duty_names <- DBI::dbGetQuery(con, paste0(
      "SELECT IDENTIFIER AS DUTYIDENTIFIER, NAME AS DUTYNAME ",
      "FROM SECURITYDUTY ",
      "WHERE IDENTIFIER IN (", ids_sql, ")"
    )) |> tibble::as_tibble()
  }

  # Platzhalter für direkte Privileges
  duty_names <- dplyr::bind_rows(
    duty_names,
    tibble::tibble(DUTYIDENTIFIER = "(direkt)", DUTYNAME = "(direkt zugewiesen)")
  )

  #====================================================
  # 4. Lizenzanforderungen pro Privilege (alle SKUs)
  #====================================================
  lic_req <- get_license_requirements(
    con,
    privilege_identifiers = all_priv_ids,
    sku_name              = NULL   # kein SKU-Filter → alle SKUs
  ) |> tibble::as_tibble()

  #====================================================
  # 5. Pro Privilege: Lizenzkategorie bestimmen
  #====================================================
  if (nrow(lic_req) > 0L) {
    priv_license <- lic_req |>
      dplyr::group_by(IDENTIFIER) |>
      dplyr::summarise(
        activity_covered  = any(
          grepl(activity_pattern, SKUNAME, ignore.case = TRUE) & ENTITLED == 1
        ),
        commerce_entitled = any(
          grepl(commerce_pattern, SKUNAME, ignore.case = TRUE) & ENTITLED == 1
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        PRIV_LICENSE = dplyr::case_when(
          activity_covered                        ~ "Activity",
          !activity_covered & commerce_entitled   ~ "Commerce",
          TRUE                                    ~ "Finance/SCM"
        )
      ) |>
      dplyr::select(IDENTIFIER, PRIV_LICENSE)
  } else {
    # Keine Lizenzinfo → alle unbekannt
    priv_license <- tibble::tibble(
      IDENTIFIER   = all_priv_ids,
      PRIV_LICENSE = "Unbekannt"
    )
  }

  #====================================================
  # 6. Auf Duty-Ebene aggregieren
  #====================================================
  license_rank <- c(
    "Activity"    = 1L,
    "Finance/SCM" = 2L,
    "Commerce"    = 3L,
    "Unbekannt"   = 4L
  )

  duty_license <- all_pairs |>
    dplyr::left_join(priv_license, by = c("PRIVILEGEIDENTIFIER" = "IDENTIFIER")) |>
    dplyr::mutate(
      PRIV_LICENSE  = dplyr::coalesce(PRIV_LICENSE, "Unbekannt"),
      LICENSE_RANK  = license_rank[PRIV_LICENSE]
    ) |>
    dplyr::group_by(DUTYIDENTIFIER) |>
    dplyr::summarise(
      PRIVILEGE_COUNT  = dplyr::n_distinct(PRIVILEGEIDENTIFIER),
      LICENSE_RANK_MAX = max(LICENSE_RANK, na.rm = TRUE),
      LICENSE_DETAIL   = paste(
        sort(unique(PRIV_LICENSE)), collapse = ", "
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      LICENSE_CATEGORY = dplyr::case_when(
        LICENSE_RANK_MAX == 1L ~ "Activity",
        LICENSE_RANK_MAX == 2L ~ "Finance/SCM",
        LICENSE_RANK_MAX == 3L ~ "Commerce",
        TRUE                   ~ "Unbekannt"
      )
    ) |>
    dplyr::left_join(duty_names, by = "DUTYIDENTIFIER") |>
    dplyr::arrange(
      dplyr::desc(LICENSE_RANK_MAX),   # höchste Lizenz zuerst
      DUTYIDENTIFIER
    ) |>
    dplyr::select(
      DUTYIDENTIFIER, DUTYNAME,
      LICENSE_CATEGORY, LICENSE_DETAIL,
      PRIVILEGE_COUNT
    )

  duty_license
}

#' @keywords internal
.empty_breakdown <- function() {
  tibble::tibble(
    DUTYIDENTIFIER   = character(),
    DUTYNAME         = character(),
    LICENSE_CATEGORY = character(),
    LICENSE_DETAIL   = character(),
    PRIVILEGE_COUNT  = integer()
  )
}
