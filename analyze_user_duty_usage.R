#' Zugewiesene Duties mit der tatsächlichen Nutzung vergleichen
#'
#' Stellt für jeden Benutzer einer Rolle und jede Duty gegenüber, ob die Duty
#' nur zugewiesen ist oder laut Telemetrie tatsächlich genutzt wird.
#'
#' @details
#'
#' **Evidenz**
#'
#' \describe{
#'   \item{\code{USED}}{Der Benutzer hat mindestens einen Entry Point der
#'     Duty geöffnet, lizenzrelevant oder nicht.}
#'   \item{\code{NOT_OBSERVED}}{Jeder Lizenzteil der Duty hat messbare Entry
#'     Points, und der Benutzer hat keinen Entry Point der Duty geöffnet.
#'     Das ist ein Hinweis auf Nichtnutzung, kein Beweis.}
#'   \item{\code{NOT_MEASURABLE}}{Die Telemetrie kann die Duty nicht
#'     vollständig beurteilen: Die Duty hat keinen messbaren Entry Point,
#'     oder einer ihrer Lizenzteile (\code{UNMEASURABLE_LICENSES}) hat
#'     keinen. Die Entscheidung muss fachlich fallen.}
#'   \item{\code{NO_USER_TELEMETRY}}{Für den Benutzer liegt überhaupt keine
#'     Telemetrie vor. Er ist inaktiv oder seine Benutzer-ID stimmt nicht mit
#'     der Telemetrie überein; er wird nie als "ungenutzt" gewertet.}
#' }
#'
#' **Warum der schwächste Lizenzteil zählt**
#'
#' Eine Duty wird einem Benutzer als Ganzes zugewiesen oder entzogen. Enthält
#' sie ein Formular (messbar) und eine Aktion mit eigener Lizenz (nicht
#' messbar), dann sagt ein ungeöffnetes Formular nichts über die Aktion aus.
#' Solche Duties erhalten \code{NOT_MEASURABLE} statt \code{NOT_OBSERVED}.
#' Welche Ressourcentypen als messbar gelten, legt
#' \code{\link{build_role_license_drivers}} über
#' \code{measurable_type_pattern} fest.
#'
#' **Format von \code{usage}**
#'
#' Akzeptiert werden die Spalten aus
#' \code{D365Licensing::load_telemetry_touches()} (\code{user_Id},
#' \code{name}, \code{touches}, \code{last_seen}) und aus dem Ergebnis von
#' \code{\link{get_insights_page_views_query}} (\code{user_Id}, \code{name},
#' \code{TouchesProUser}, \code{timestamp}). Benutzer-IDs und Entry Points
#' werden ohne Beachtung der Groß-/Kleinschreibung abgeglichen.
#'
#' Kommt kein einziger messbarer Entry Point der Rolle in \code{usage} vor,
#' passen die Namen der Telemetrie vermutlich nicht zu den AOT-Namen. Die
#' Funktion warnt dann und wertet die Telemetrie nicht aus, statt alle
#' Benutzer als \code{NOT_OBSERVED} auszuweisen.
#'
#' @param drivers
#' Ergebnis von \code{\link{build_role_license_drivers}} bzw.
#' \code{\link{get_role_license_drivers}}.
#'
#' @param user_ids
#' Character-Vektor mit den Benutzer-IDs, die die Rolle besitzen
#' (z.B. \code{get_sql_user_role_assignments(cnn, roles = ...)$USERID}).
#'
#' @param usage
#' Tibble mit Telemetrie-Berührungspunkten pro Benutzer und Entry Point.
#' \code{NULL} = keine Telemetrie; alle Benutzer erhalten
#' \code{NO_USER_TELEMETRY}.
#'
#' @return
#'
#' Tibble mit einer Zeile pro Benutzer und Duty:
#'
#' \describe{
#'   \item{\code{USERID}}{Benutzerkennung.}
#'   \item{\code{DUTYIDENTIFIER}, \code{DUTYNAME}, \code{DUTY_SKU}}{Duty und
#'     ihre teuerste Mindestlizenz.}
#'   \item{\code{ENTRYPOINT_COUNT}}{Entry Points der Duty.}
#'   \item{\code{MEASURABLE_COUNT}}{Davon per Telemetrie messbar.}
#'   \item{\code{UNMEASURABLE_LICENSES}}{Lizenzteile der Duty ohne messbaren
#'     Entry Point.}
#'   \item{\code{TOUCHES}}{Aufrufe aller Entry Points der Duty.}
#'   \item{\code{DRIVER_TOUCHES}}{Aufrufe der Entry Points, die die Lizenz
#'     der Duty bestimmen. \code{TOUCHES > 0} bei \code{DRIVER_TOUCHES = 0}
#'     heißt: Der Benutzer nutzt die Duty, aber nicht den teuren Teil.}
#'   \item{\code{LAST_SEEN}}{Datum des letzten Aufrufs.}
#'   \item{\code{EVIDENCE}}{Siehe oben.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn  <- get_connection("PRJ", "db_credentials.xlsx")
#' role <- "_WIBU_VERKAUF_INNENDIENST_MITARBEITER"
#'
#' drivers  <- get_role_license_drivers(cnn, role)
#' user_ids <- get_sql_user_role_assignments(cnn, roles = role)$USERID
#' usage    <- D365Licensing::load_telemetry_touches(D365Licensing::load_config())
#'
#' duty_usage <- analyze_user_duty_usage(drivers, user_ids, usage)
#'
#' # Wer nutzt die Duty tatsächlich?
#' duty_usage |> dplyr::filter(EVIDENCE == "USED")
#'
#' }
#'
#' @seealso
#' \code{\link{get_role_license_drivers}}
#' \code{\link{recommend_user_license_target}}
#' \code{\link{get_insights_page_views_query}}
#'
#' @export
analyze_user_duty_usage <- function(drivers, user_ids, usage = NULL) {

  .check_required_columns(drivers, .driver_columns(), "drivers")

  if (!is.character(user_ids)) {
    stop("user_ids muss ein Character-Vektor sein.")
  }

  users            <- .distinct_users(user_ids)
  duty_entrypoints <- .distinct_duty_entrypoints(drivers)
  touches          <- .normalize_usage(usage) |>
    .drop_unmatched_usage(duty_entrypoints)

  tidyr::expand_grid(users, .describe_duty_measurability(drivers, duty_entrypoints)) |>
    dplyr::left_join(
      .sum_touches_per_user_duty(touches, duty_entrypoints),
      by = c("USER_KEY", "DUTYIDENTIFIER")
    ) |>
    dplyr::mutate(
      TOUCHES        = dplyr::coalesce(TOUCHES, 0),
      DRIVER_TOUCHES = dplyr::coalesce(DRIVER_TOUCHES, 0),
      EVIDENCE       = .classify_evidence(
        touches          = TOUCHES,
        has_telemetry    = USER_KEY %in% touches$USER_KEY,
        fully_measurable = MEASURABLE_COUNT > 0L & UNMEASURABLE_LICENSES == ""
      )
    ) |>
    dplyr::select(
      USERID, DUTYIDENTIFIER, DUTYNAME, DUTY_SKU,
      ENTRYPOINT_COUNT, MEASURABLE_COUNT, UNMEASURABLE_LICENSES,
      TOUCHES, DRIVER_TOUCHES, LAST_SEEN, EVIDENCE
    ) |>
    dplyr::arrange(USERID, DUTYIDENTIFIER)
}

.classify_evidence <- function(touches, has_telemetry, fully_measurable) {
  dplyr::case_when(
    touches > 0       ~ .EVIDENCE_USED,
    !has_telemetry    ~ .EVIDENCE_NO_USER_TELEMETRY,
    !fully_measurable ~ .EVIDENCE_NOT_MEASURABLE,
    TRUE              ~ .EVIDENCE_NOT_OBSERVED
  )
}

# Eindeutige Benutzer; USER_KEY dient dem Abgleich ohne Gross-/Kleinschreibung.
.distinct_users <- function(user_ids) {
  tibble::tibble(USERID = user_ids, USER_KEY = tolower(user_ids)) |>
    dplyr::filter(!is.na(USERID)) |>
    dplyr::distinct(USER_KEY, .keep_all = TRUE)
}

# Ein Entry Point pro Duty, auch wenn mehrere Privileges ihn berechtigen.
.distinct_duty_entrypoints <- function(drivers) {
  drivers |>
    dplyr::filter(!is.na(ENTRYPOINT)) |>
    dplyr::mutate(ENTRYPOINT_KEY = toupper(ENTRYPOINT)) |>
    dplyr::group_by(DUTYIDENTIFIER, ENTRYPOINT_KEY) |>
    dplyr::summarise(
      MEASURABLE  = any(MEASURABLE),
      DRIVES_DUTY = any(DRIVES_DUTY),
      .groups     = "drop"
    )
}

.describe_duty_measurability <- function(drivers, duty_entrypoints) {

  counts <- duty_entrypoints |>
    dplyr::group_by(DUTYIDENTIFIER) |>
    dplyr::summarise(
      ENTRYPOINT_COUNT = dplyr::n(),
      MEASURABLE_COUNT = sum(MEASURABLE),
      .groups          = "drop"
    )

  drivers |>
    dplyr::distinct(DUTYIDENTIFIER, DUTYNAME, DUTY_SKU) |>
    dplyr::left_join(counts, by = "DUTYIDENTIFIER") |>
    dplyr::left_join(.unmeasurable_licenses_per_duty(drivers), by = "DUTYIDENTIFIER") |>
    dplyr::mutate(
      ENTRYPOINT_COUNT      = dplyr::coalesce(ENTRYPOINT_COUNT, 0L),
      MEASURABLE_COUNT      = dplyr::coalesce(MEASURABLE_COUNT, 0L),
      UNMEASURABLE_LICENSES = dplyr::coalesce(UNMEASURABLE_LICENSES, "")
    )
}

# Schutz vor einem Namensproblem: Taucht kein messbarer Entry Point der Rolle
# in der gesamten Telemetrie auf, stimmen die Namen vermutlich nicht ueberein.
# Ohne diesen Schutz wuerden alle Benutzer als NOT_OBSERVED erscheinen.
.drop_unmatched_usage <- function(touches, duty_entrypoints) {

  measurable_keys <- duty_entrypoints$ENTRYPOINT_KEY[duty_entrypoints$MEASURABLE]

  names_cannot_be_checked <- nrow(touches) == 0L || length(measurable_keys) == 0L

  if (names_cannot_be_checked || any(touches$ENTRYPOINT_KEY %in% measurable_keys)) {
    return(touches)
  }

  warning(
    "Kein messbarer Entry Point der Rolle kommt in der Telemetrie vor. ",
    "Vermutlich passen die Namen in 'usage' nicht zu den AOT-Namen. ",
    "Die Telemetrie wird nicht ausgewertet (alle Benutzer: NO_USER_TELEMETRY).",
    call. = FALSE
  )

  touches[0, ]
}

.sum_touches_per_user_duty <- function(touches, duty_entrypoints) {
  touches |>
    dplyr::inner_join(
      duty_entrypoints,
      by           = "ENTRYPOINT_KEY",
      relationship = "many-to-many"
    ) |>
    dplyr::group_by(USER_KEY, DUTYIDENTIFIER) |>
    dplyr::summarise(
      # DRIVER_TOUCHES vor TOUCHES: summarise() ueberschreibt TOUCHES sofort
      DRIVER_TOUCHES = sum(TOUCHES[DRIVES_DUTY], na.rm = TRUE),
      TOUCHES        = sum(TOUCHES, na.rm = TRUE),
      LAST_SEEN      = .max_date(LAST_SEEN),
      .groups        = "drop"
    )
}

# Bringt die Telemetrie auf USER_KEY, ENTRYPOINT_KEY, TOUCHES, LAST_SEEN.
.normalize_usage <- function(usage) {

  if (is.null(usage)) {
    return(tibble::tibble(
      USER_KEY       = character(),
      ENTRYPOINT_KEY = character(),
      TOUCHES        = numeric(),
      LAST_SEEN      = as.Date(character())
    ))
  }

  if (!is.data.frame(usage)) {
    stop("usage muss ein Data Frame oder NULL sein.")
  }

  tibble::tibble(
    USER_KEY       = tolower(as.character(.pick_usage_column(usage, c("USERID", "user_Id")))),
    ENTRYPOINT_KEY = toupper(as.character(.pick_usage_column(usage, c("ENTRYPOINT", "name")))),
    TOUCHES        = .as_touch_count(.pick_usage_column(usage, c("TOUCHES", "touches", "TouchesProUser"))),
    LAST_SEEN      = .as_usage_date(.pick_usage_column(usage, c("LAST_SEEN", "last_seen", "timestamp")))
  ) |>
    dplyr::filter(!is.na(USER_KEY), !is.na(ENTRYPOINT_KEY))
}

# Eine Zeile ohne lesbare Anzahl belegt trotzdem, dass der Benutzer in der
# Telemetrie vorkommt. Als 0 gewertet, entstuende daraus "NOT_OBSERVED".
.as_touch_count <- function(x) {

  touch_count <- suppressWarnings(as.numeric(x))
  unreadable  <- sum(is.na(touch_count))

  if (unreadable > 0L) {
    stop(
      "usage enthaelt ", unreadable,
      " Telemetrie-Zeile(n) ohne lesbare Anzahl der Aufrufe. ",
      "Zeilen korrigieren oder entfernen."
    )
  }

  touch_count
}

.pick_usage_column <- function(usage, candidates) {

  found <- intersect(candidates, names(usage))

  if (length(found) == 0L) {
    stop("usage braucht eine der Spalten: ", paste(candidates, collapse = ", "))
  }

  usage[[found[[1]]]]
}

# Telemetrie liefert ISO 8601 (API) oder dd.MM.yyyy (get_insights_page_views_query()).
.USAGE_DATE_ORDERS <- c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY")

.as_usage_date <- function(x) {

  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (length(x) == 0L) {
    return(as.Date(character()))
  }

  as.Date(lubridate::parse_date_time(
    as.character(x),
    orders = .USAGE_DATE_ORDERS,
    tz     = "UTC",
    quiet  = TRUE
  ))
}
