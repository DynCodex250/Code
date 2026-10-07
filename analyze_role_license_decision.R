#' Lizenz-Entscheidungsgrundlage für eine Rolle erstellen
#'
#' Führt die vollständige Analyse für eine Rolle aus, die mehrere Lizenzen
#' erfordert: welche Duty und welches Privilege welche Lizenz auslöst, wer
#' die Rolle besitzt, wer welche Duty tatsächlich nutzt und wie viele
#' Lizenzen sich einsparen lassen.
#'
#' @details
#'
#' **Ablauf**
#'
#' \enumerate{
#'   \item Lizenztreiber der Rolle laden
#'     (\code{\link{get_role_license_drivers}}).
#'   \item Benutzer der Rolle und deren Lizenzen über alle Rollen laden
#'     (\code{\link{get_sql_user_role_assignments}},
#'     \code{\link{get_user_licenses_by_role}}).
#'   \item Zuweisung mit Nutzung vergleichen
#'     (\code{\link{analyze_user_duty_usage}}).
#'   \item Empfehlung und Einsparung ableiten
#'     (\code{\link{recommend_user_license_target}}).
#'   \item Modell mit der Microsoft-Sicht abgleichen
#'     (\code{\link{compare_role_license_skus}}).
#' }
#'
#' Deaktivierte Benutzer (\code{USERENABLED = 0}) werden nicht ausgewertet;
#' ihre Anzahl steht in \code{DataQuality}.
#'
#' **Vor einer Entscheidung prüfen**
#'
#' \itemize{
#'   \item \code{Reconciliation}: Jede Zeile mit \code{STATUS != "match"}
#'     bedeutet, dass Modell und D365FO für die Rolle abweichen.
#'   \item \code{DataQuality}: Wie viele Benutzer und Entry Points die
#'     Telemetrie überhaupt abdeckt.
#' }
#'
#' @param con
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param role_identifier
#' AOT-Name genau einer Rolle (\code{ROLEIDENTIFIER}).
#'
#' @param usage
#' Telemetrie-Berührungspunkte pro Benutzer und Entry Point, siehe
#' \code{\link{analyze_user_duty_usage}}. \code{NULL} = Analyse ohne
#' Telemetrie; alle Empfehlungen lauten dann \code{REVIEW}.
#'
#' @param base_group_pattern
#' Regex-Muster für die Lizenzgruppe von Base-Lizenzen.
#' Standard: \code{"^Base"}.
#'
#' @param measurable_type_pattern
#' Regex-Muster für die Ressourcentypen, die die Telemetrie erfasst.
#' Standard: \code{"display"}.
#'
#' @return
#'
#' Liste von Tibbles, in der Reihenfolge der Arbeitsblätter von
#' \code{\link{export_role_license_decision}}:
#'
#' \describe{
#'   \item{\code{Savings}}{Einsparung pro Base-Lizenz.}
#'   \item{\code{UserSummary}}{Lizenzen vorher und nachher pro Benutzer.}
#'   \item{\code{UserDecisions}}{Empfehlung pro Benutzer und SKU.}
#'   \item{\code{UserDutyUsage}}{Zuweisung und Nutzung pro Benutzer und Duty.}
#'   \item{\code{DutyDrivers}}{Lizenz pro Duty.}
#'   \item{\code{EntryPointDrivers}}{Lizenz pro Privilege und Entry Point.}
#'   \item{\code{RoleLicenses}}{Lizenzen der Rolle und die Duties dahinter.}
#'   \item{\code{Reconciliation}}{Abgleich mit der Microsoft-Sicht.}
#'   \item{\code{DataQuality}}{Abdeckung der Telemetrie.}
#'   \item{\code{Legend}}{Bedeutung der Statuswerte.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn   <- get_connection("PRJ", "db_credentials.xlsx")
#' usage <- D365Licensing::load_telemetry_touches(D365Licensing::load_config())
#'
#' analysis <- analyze_role_license_decision(
#'   con             = cnn,
#'   role_identifier = "_WIBU_VERKAUF_INNENDIENST_MITARBEITER",
#'   usage           = usage
#' )
#'
#' analysis$Savings
#' analysis$Reconciliation
#'
#' export_role_license_decision(
#'   analysis,
#'   role_identifier = "_WIBU_VERKAUF_INNENDIENST_MITARBEITER"
#' )
#'
#' }
#'
#' @seealso
#' \code{\link{export_role_license_decision}}
#' \code{\link{get_role_license_drivers}}
#' \code{\link{analyze_user_duty_usage}}
#' \code{\link{recommend_user_license_target}}
#'
#' @export
analyze_role_license_decision <- function(
  con,
  role_identifier,
  usage                   = NULL,
  base_group_pattern      = "^Base",
  measurable_type_pattern = "display"
) {

  .check_single_role(role_identifier)

  drivers <- get_role_license_drivers(
    con, role_identifier, base_group_pattern, measurable_type_pattern
  )
  role_skus <- summarize_role_license_skus(drivers)

  assigned_ids  <- unique(get_sql_user_role_assignments(con, roles = role_identifier)$USERID)
  user_licenses <- .load_user_licenses(con, assigned_ids)
  disabled_keys <- .disabled_user_keys(user_licenses)

  active_ids      <- assigned_ids[!tolower(assigned_ids) %in% disabled_keys]
  active_licenses <- user_licenses[!tolower(user_licenses$USERID) %in% disabled_keys, ]

  duty_usage     <- analyze_user_duty_usage(drivers, active_ids, usage)
  recommendation <- recommend_user_license_target(
    drivers, duty_usage, active_licenses, role_identifier, base_group_pattern
  )

  list(
    Savings           = recommendation$Savings,
    UserSummary       = recommendation$UserSummary,
    UserDecisions     = recommendation$UserDecisions,
    UserDutyUsage     = duty_usage,
    DutyDrivers       = summarize_duty_license_drivers(drivers),
    EntryPointDrivers = drivers,
    RoleLicenses      = role_skus,
    Reconciliation    = compare_role_license_skus(
      role_skus,
      get_role_license_assignments(con, role_identifiers = role_identifier)
    ),
    DataQuality       = .summarize_usage_coverage(
      drivers, active_ids, active_licenses, length(disabled_keys), usage
    ),
    Legend            = .license_decision_legend()
  )
}

# get_user_licenses_by_role() erzeugt bei leerer Benutzerliste "IN ()".
.load_user_licenses <- function(con, user_ids) {

  if (length(user_ids) == 0L) {
    return(tibble::tibble(
      USERID         = character(),
      USERENABLED    = integer(),
      ROLEIDENTIFIER = character(),
      SKUNAME        = character(),
      SKUGROUP       = character()
    ))
  }

  get_user_licenses_by_role(con, user_ids = user_ids)
}

.disabled_user_keys <- function(user_licenses) {

  if (!"USERENABLED" %in% names(user_licenses)) {
    return(character())
  }

  is_disabled <- !is.na(user_licenses$USERENABLED) & user_licenses$USERENABLED == 0
  unique(tolower(user_licenses$USERID[is_disabled]))
}

# Kennzahlen dazu, wie weit die Telemetrie die Rolle und ihre Benutzer abdeckt.
.summarize_usage_coverage <- function(drivers, user_ids, user_licenses,
                                      disabled_count, usage) {

  users   <- .distinct_users(user_ids)
  touches <- .normalize_usage(usage) |>
    dplyr::filter(USER_KEY %in% users$USER_KEY)

  entrypoints <- .distinct_duty_entrypoints(drivers) |>
    dplyr::group_by(ENTRYPOINT_KEY) |>
    dplyr::summarise(MEASURABLE = any(MEASURABLE), .groups = "drop")

  matched_entrypoints <- intersect(touches$ENTRYPOINT_KEY, entrypoints$ENTRYPOINT_KEY)
  users_without_license_row <- setdiff(users$USER_KEY, tolower(user_licenses$USERID))

  tibble::tribble(
    ~KENNZAHL,                                           ~WERT,
    "Benutzer mit Rolle",                                as.character(nrow(users)),
    "davon in der Telemetrie gefunden",                  as.character(dplyr::n_distinct(touches$USER_KEY)),
    "davon ohne Lizenzzeile in D365FO",                  as.character(length(users_without_license_row)),
    "Deaktivierte Benutzer (nicht ausgewertet)",         as.character(disabled_count),
    "Entry Points der Rolle",                            as.character(nrow(entrypoints)),
    "davon per Telemetrie messbar",                      as.character(sum(entrypoints$MEASURABLE)),
    "Entry Points in der Telemetrie der Rollenbenutzer", as.character(dplyr::n_distinct(touches$ENTRYPOINT_KEY)),
    "davon in der Rolle enthalten",                      as.character(length(matched_entrypoints)),
    "Ältester letzter Zugriff",                          format(.min_date(touches$LAST_SEEN)),
    "Neuester letzter Zugriff",                          format(.max_date(touches$LAST_SEEN))
  )
}

.license_decision_legend <- function() {
  tibble::tribble(
    ~BEGRIFF,                       ~BEDEUTUNG,
    "EVIDENCE = USED",              "Der Benutzer hat mindestens einen Entry Point der Duty geöffnet.",
    "EVIDENCE = NOT_OBSERVED",      "Jeder Lizenzteil der Duty hat messbare Entry Points, und der Benutzer hat keinen Entry Point der Duty geöffnet. Hinweis auf Nichtnutzung, kein Beweis.",
    "EVIDENCE = NOT_MEASURABLE",    "Die Telemetrie kann die Duty nicht vollständig beurteilen: kein messbarer Entry Point, oder ein Lizenzteil (UNMEASURABLE_LICENSES) besteht nur aus Aktionen, Services oder Data Entities. Fachlich klären.",
    "EVIDENCE = NO_USER_TELEMETRY", "Für den Benutzer liegt keine Telemetrie vor: inaktiv oder Benutzer-ID stimmt nicht überein. Nicht als ungenutzt werten.",
    "DRIVER_TOUCHES",               "Aufrufe der Entry Points, die die Lizenz der Duty bestimmen. TOUCHES > 0 bei DRIVER_TOUCHES = 0: Der Benutzer nutzt die Duty, aber nicht ihren teuren Teil.",
    "DECISION = KEEP",              "Mindestens eine Duty, die diese Lizenz braucht, wird genutzt.",
    "DECISION = REMOVE_CANDIDATE",  "Keine Duty, die diese Lizenz braucht, wurde genutzt, und alle sind messbar. Kandidat: vor der Umsetzung mit dem Fachbereich bestätigen.",
    "DECISION = REVIEW",            "Die Telemetrie kann nicht entscheiden. Fachlich klären.",
    "SAVES_LICENSE",                "Base-Lizenz, die D365FO dem Benutzer über diese Rolle zuweist, die keine andere seiner Rollen erfordert und deren DECISION REMOVE_CANDIDATE ist.",
    "SAVES_IF_CONFIRMED",           "Wie SAVES_LICENSE, aber DECISION ist REVIEW: mögliche Einsparung nach fachlicher Prüfung.",
    "Savings",                      "Die Einsparung setzt die Rollenteilung voraus: Die Duties bleiben für die Benutzer erhalten, die sie nutzen, und entfallen nur für die übrigen. Aus der bestehenden Rolle entfernt, verlören alle Benutzer den Zugriff.",
    "LICENSE_STATUS",               "LICENSED: eine SKU deckt den Entry Point. NOT_LISTED: nicht lizenzrelevant. UNKNOWN_PRIVILEGE, NOT_ENTITLED: Lizenz ungeklärt, manuell bewerten.",
    "MIN_SKU",                      "Günstigste Lizenz, die den Entry Point abdeckt.",
    "DUTY_SKU",                     "Teuerste Mindestlizenz innerhalb der Duty.",
    "SOLE_SOURCE_OF",               "Lizenzen, die in der Rolle nur über diese Duty benötigt werden.",
    "Reconciliation",               "STATUS ungleich match: Modell und D365FO weichen für die Rolle ab. Vor einer Entscheidung klären.",
    "SKUS_AFTER, USERS_AFTER",      "Schätzung. Maßgeblich ist LICENSINGUSERLICENSESBYROLE nach der Umstellung."
  )
}
