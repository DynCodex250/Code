#' Lizenzempfehlung pro Benutzer ableiten
#'
#' Leitet aus Nutzungsevidenz und aktuellen Lizenzen ab, welche Lizenz-SKUs
#' jeder Benutzer einer Rolle weiterhin braucht, welche Kandidaten für den
#' Wegfall sind und wie viele Lizenzen sich dadurch einsparen lassen.
#'
#' @details
#'
#' **Empfehlung je Benutzer und SKU**
#'
#' Betrachtet werden alle Duties der Rolle, die die SKU als Mindestlizenz
#' enthalten:
#'
#' \describe{
#'   \item{\code{KEEP}}{Mindestens eine dieser Duties wird genutzt.}
#'   \item{\code{REMOVE_CANDIDATE}}{Alle diese Duties sind messbar und
#'     keine wurde genutzt. Ein Kandidat, keine Tatsache: vor der Umsetzung
#'     mit dem Fachbereich bestätigen.}
#'   \item{\code{REVIEW}}{Die Telemetrie kann nicht entscheiden (Duty nicht
#'     messbar oder Benutzer ohne Telemetrie).}
#' }
#'
#' **Wann zählt eine Einsparung?**
#'
#' \code{SAVES_LICENSE} ist nur \code{TRUE}, wenn alle Bedingungen gelten:
#'
#' \itemize{
#'   \item Die Empfehlung ist \code{REMOVE_CANDIDATE}.
#'   \item Die SKU ist eine Base-Lizenz.
#'   \item D365FO weist die SKU dem Benutzer über diese Rolle zu.
#'   \item Keine andere Rolle des Benutzers erfordert dieselbe SKU.
#' }
#'
#' Die letzte Bedingung ist entscheidend: Die Lizenz eines Benutzers ergibt
#' sich aus allen seinen Rollen. Eine SKU aus dieser Rolle zu entfernen
#' spart nichts, solange eine andere Rolle sie weiter auslöst.
#'
#' **Einordnung**
#'
#' Der Stand vorher stammt aus \code{LICENSINGUSERLICENSESBYROLE} (D365FO).
#' Der Stand nachher ist eine Schätzung und nach der Umstellung über
#' \code{\link{get_user_licenses_by_role}} zu prüfen.
#'
#' @param drivers
#' Ergebnis von \code{\link{get_role_license_drivers}}.
#'
#' @param duty_usage
#' Ergebnis von \code{\link{analyze_user_duty_usage}}.
#'
#' @param user_licenses
#' Ergebnis von \code{\link{get_user_licenses_by_role}} für die Benutzer der
#' Rolle, über alle ihre Rollen. Erforderliche Spalten: \code{USERID},
#' \code{ROLEIDENTIFIER}, \code{SKUNAME}, \code{SKUGROUP}.
#'
#' @param role_identifier
#' AOT-Name der untersuchten Rolle.
#'
#' @param base_group_pattern
#' Regex-Muster, das die \code{SKUGROUP} von Base-Lizenzen erkennt.
#' Standard: \code{"^Base"}.
#'
#' @return
#'
#' Liste mit drei Tibbles:
#'
#' \describe{
#'   \item{\code{UserDecisions}}{Eine Zeile pro Benutzer und SKU mit
#'     \code{DECISION}, \code{LICENSED_VIA_ROLE},
#'     \code{LICENSED_VIA_OTHER_ROLE}, \code{SAVES_LICENSE} und
#'     \code{SAVES_IF_CONFIRMED} (wie \code{SAVES_LICENSE}, aber für
#'     \code{REVIEW}).}
#'   \item{\code{UserSummary}}{Eine Zeile pro Benutzer: Lizenzen vorher und
#'     nachher (\code{SKUS_BEFORE}, \code{SKUS_AFTER}, \code{SAVED_SKUS})
#'     sowie die Anzahl der Base-Lizenzen.}
#'   \item{\code{Savings}}{Eine Zeile pro Base-Lizenz: Benutzer vorher,
#'     eingespart, nachher und zusätzlich möglich nach fachlicher Prüfung
#'     (\code{USERS_REVIEW}).}
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn  <- get_connection("PRJ", "db_credentials.xlsx")
#' role <- "_WIBU_VERKAUF_INNENDIENST_MITARBEITER"
#'
#' drivers    <- get_role_license_drivers(cnn, role)
#' user_ids   <- get_sql_user_role_assignments(cnn, roles = role)$USERID
#' usage      <- D365Licensing::load_telemetry_touches(D365Licensing::load_config())
#' duty_usage <- analyze_user_duty_usage(drivers, user_ids, usage)
#'
#' result <- recommend_user_license_target(
#'   drivers         = drivers,
#'   duty_usage      = duty_usage,
#'   user_licenses   = get_user_licenses_by_role(cnn, user_ids = user_ids),
#'   role_identifier = role
#' )
#'
#' result$Savings
#'
#' }
#'
#' @seealso
#' \code{\link{analyze_user_duty_usage}}
#' \code{\link{analyze_role_license_decision}}
#' \code{\link{get_user_licenses_by_role}}
#'
#' @export
recommend_user_license_target <- function(
  drivers,
  duty_usage,
  user_licenses,
  role_identifier,
  base_group_pattern = "^Base"
) {

  .check_required_columns(drivers, .driver_columns(), "drivers")
  .check_required_columns(
    duty_usage,
    c("USERID", "DUTYIDENTIFIER", "TOUCHES", "LAST_SEEN", "EVIDENCE"),
    "duty_usage"
  )
  .check_required_columns(
    user_licenses,
    c("USERID", "ROLEIDENTIFIER", "SKUNAME", "SKUGROUP"),
    "user_licenses"
  )
  .check_single_role(role_identifier)

  licenses  <- .normalize_user_licenses(user_licenses, role_identifier, base_group_pattern)
  decisions <- .decide_user_skus(drivers, duty_usage) |>
    .flag_license_savings(licenses)

  list(
    UserDecisions = decisions,
    UserSummary   = .summarize_user_licenses(decisions, licenses),
    Savings       = .summarize_license_savings(decisions, licenses)
  )
}

# Lizenzen laut D365FO je Benutzer, getrennt nach dieser und anderen Rollen.
.normalize_user_licenses <- function(user_licenses, role_identifier, base_group_pattern) {
  user_licenses |>
    dplyr::filter(!is.na(USERID), !is.na(SKUNAME)) |>
    dplyr::transmute(
      USER_KEY        = tolower(USERID),
      SKUNAME,
      IN_ROLE         = !is.na(ROLEIDENTIFIER) &
        toupper(ROLEIDENTIFIER) == toupper(role_identifier),
      IS_BASE_LICENSE = .is_base_group(SKUGROUP, base_group_pattern)
    ) |>
    dplyr::distinct()
}

# Empfehlung je Benutzer und SKU aus der Evidenz aller Duties, die die SKU brauchen.
.decide_user_skus <- function(drivers, duty_usage) {

  duty_skus <- drivers |>
    dplyr::filter(!is.na(MIN_SKU)) |>
    dplyr::distinct(
      DUTYIDENTIFIER,
      SKUNAME  = MIN_SKU,
      PRIORITY = MIN_PRIORITY,
      IS_BASE_LICENSE
    )

  duty_usage |>
    dplyr::inner_join(duty_skus, by = "DUTYIDENTIFIER", relationship = "many-to-many") |>
    dplyr::group_by(USERID, SKUNAME, PRIORITY, IS_BASE_LICENSE) |>
    dplyr::summarise(
      DUTY_COUNT          = dplyr::n_distinct(DUTYIDENTIFIER),
      USED_DUTIES         = sum(EVIDENCE == .EVIDENCE_USED),
      NOT_OBSERVED_DUTIES = sum(EVIDENCE == .EVIDENCE_NOT_OBSERVED),
      REVIEW_DUTIES       = sum(
        EVIDENCE %in% c(.EVIDENCE_NOT_MEASURABLE, .EVIDENCE_NO_USER_TELEMETRY)
      ),
      DUTY_TOUCHES        = sum(TOUCHES),
      LAST_SEEN           = .max_date(LAST_SEEN),
      .groups             = "drop"
    ) |>
    dplyr::mutate(
      # Wegfall nur, wenn jede Duty ausdruecklich NOT_OBSERVED ist;
      # alles Unklare bleibt REVIEW.
      DECISION = dplyr::case_when(
        USED_DUTIES > 0                   ~ .DECISION_KEEP,
        NOT_OBSERVED_DUTIES == DUTY_COUNT ~ .DECISION_REMOVE_CANDIDATE,
        TRUE                              ~ .DECISION_REVIEW
      )
    )
}

.flag_license_savings <- function(decisions, licenses) {

  via_role <- licenses |>
    dplyr::filter(IN_ROLE) |>
    dplyr::distinct(USER_KEY, SKUNAME) |>
    dplyr::mutate(LICENSED_VIA_ROLE = TRUE)

  via_other_role <- licenses |>
    dplyr::filter(!IN_ROLE) |>
    dplyr::distinct(USER_KEY, SKUNAME) |>
    dplyr::mutate(LICENSED_VIA_OTHER_ROLE = TRUE)

  decisions |>
    dplyr::mutate(USER_KEY = tolower(USERID)) |>
    dplyr::left_join(via_role, by = c("USER_KEY", "SKUNAME")) |>
    dplyr::left_join(via_other_role, by = c("USER_KEY", "SKUNAME")) |>
    dplyr::mutate(
      LICENSED_VIA_ROLE       = dplyr::coalesce(LICENSED_VIA_ROLE, FALSE),
      LICENSED_VIA_OTHER_ROLE = dplyr::coalesce(LICENSED_VIA_OTHER_ROLE, FALSE),
      ONLY_VIA_THIS_ROLE      = IS_BASE_LICENSE & LICENSED_VIA_ROLE & !LICENSED_VIA_OTHER_ROLE,
      SAVES_LICENSE           = ONLY_VIA_THIS_ROLE & DECISION == .DECISION_REMOVE_CANDIDATE,
      SAVES_IF_CONFIRMED      = ONLY_VIA_THIS_ROLE & DECISION == .DECISION_REVIEW
    ) |>
    dplyr::select(-USER_KEY, -ONLY_VIA_THIS_ROLE) |>
    dplyr::arrange(USERID, dplyr::desc(PRIORITY), SKUNAME)
}

.summarize_user_licenses <- function(decisions, licenses) {

  before <- licenses |>
    dplyr::group_by(USER_KEY) |>
    dplyr::summarise(
      SKU_LIST_BEFORE      = list(sort(unique(SKUNAME))),
      BASE_LICENSES_BEFORE = dplyr::n_distinct(SKUNAME[IS_BASE_LICENSE]),
      .groups              = "drop"
    )

  decisions |>
    dplyr::group_by(USERID) |>
    dplyr::summarise(
      KEEP_SKUS             = .collapse_sorted(SKUNAME[DECISION == .DECISION_KEEP]),
      REVIEW_SKUS           = .collapse_sorted(SKUNAME[DECISION == .DECISION_REVIEW]),
      REMOVE_CANDIDATE_SKUS = .collapse_sorted(SKUNAME[DECISION == .DECISION_REMOVE_CANDIDATE]),
      SKU_LIST_SAVED        = list(sort(unique(SKUNAME[SAVES_LICENSE]))),
      .groups               = "drop"
    ) |>
    dplyr::mutate(USER_KEY = tolower(USERID)) |>
    dplyr::left_join(before, by = "USER_KEY") |>
    dplyr::mutate(
      BASE_LICENSES_BEFORE = dplyr::coalesce(BASE_LICENSES_BEFORE, 0L),
      BASE_LICENSES_AFTER  = BASE_LICENSES_BEFORE - lengths(SKU_LIST_SAVED),
      SKUS_BEFORE          = purrr::map_chr(SKU_LIST_BEFORE, paste, collapse = .SKU_SEPARATOR),
      SAVED_SKUS           = purrr::map_chr(SKU_LIST_SAVED, paste, collapse = .SKU_SEPARATOR),
      SKUS_AFTER           = purrr::map2_chr(
        SKU_LIST_BEFORE, SKU_LIST_SAVED,
        function(before, saved) paste(setdiff(before, saved), collapse = .SKU_SEPARATOR)
      )
    ) |>
    dplyr::select(
      USERID, SKUS_BEFORE, SKUS_AFTER, SAVED_SKUS,
      BASE_LICENSES_BEFORE, BASE_LICENSES_AFTER,
      KEEP_SKUS, REVIEW_SKUS, REMOVE_CANDIDATE_SKUS
    ) |>
    dplyr::arrange(USERID)
}

.summarize_license_savings <- function(decisions, licenses) {

  role_user_keys <- unique(tolower(decisions$USERID))

  users_before <- licenses |>
    dplyr::filter(IS_BASE_LICENSE, USER_KEY %in% role_user_keys) |>
    dplyr::distinct(USER_KEY, SKUNAME) |>
    dplyr::count(SKUNAME, name = "USERS_BEFORE")

  users_saved <- decisions |>
    dplyr::group_by(SKUNAME) |>
    dplyr::summarise(
      USERS_SAVED  = sum(SAVES_LICENSE),
      USERS_REVIEW = sum(SAVES_IF_CONFIRMED),
      .groups      = "drop"
    )

  users_before |>
    dplyr::left_join(users_saved, by = "SKUNAME") |>
    dplyr::mutate(
      USERS_SAVED  = dplyr::coalesce(USERS_SAVED, 0L),
      USERS_REVIEW = dplyr::coalesce(USERS_REVIEW, 0L),
      USERS_AFTER  = USERS_BEFORE - USERS_SAVED
    ) |>
    dplyr::select(SKUNAME, USERS_BEFORE, USERS_SAVED, USERS_AFTER, USERS_REVIEW) |>
    dplyr::arrange(SKUNAME)
}
