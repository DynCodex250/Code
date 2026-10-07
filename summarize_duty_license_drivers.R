#' Lizenztreiber pro Duty zusammenfassen
#'
#' Verdichtet die Lizenztreiber-Tabelle auf eine Zeile pro Duty: welche
#' Lizenz die Duty mindestens erfordert, welche Privileges das auslösen und
#' ob die Duty die einzige Quelle einer SKU in der Rolle ist.
#'
#' @details
#'
#' \code{SOLE_SOURCE_OF} ist die zentrale Spalte für die Rollenteilung:
#' Steht dort eine SKU, verschwindet diese Lizenzanforderung aus der Rolle,
#' sobald die Duty entfernt oder in eine eigene Rolle ausgelagert wird.
#'
#' @param drivers
#' Ergebnis von \code{\link{build_role_license_drivers}} bzw.
#' \code{\link{get_role_license_drivers}}.
#'
#' @return
#'
#' Tibble mit einer Zeile pro Duty:
#'
#' \describe{
#'   \item{\code{DUTYIDENTIFIER}, \code{DUTYNAME}}{Duty.}
#'   \item{\code{DUTY_SKU}, \code{DUTY_PRIORITY}}{Teuerste Mindestlizenz der
#'     Duty (\code{NA}, wenn keine Lizenzinformation vorliegt).}
#'   \item{\code{IS_BASE_LICENSE}}{\code{TRUE}, wenn \code{DUTY_SKU} eine
#'     Base-Lizenz ist.}
#'   \item{\code{SKUS_IN_DUTY}}{Alle Mindestlizenzen der Duty, teuerste zuerst.}
#'   \item{\code{PRIVILEGE_COUNT}, \code{ENTRYPOINT_COUNT}}{Umfang der Duty
#'     (alle Entry Points, auch nicht lizenzrelevante).}
#'   \item{\code{DRIVER_PRIVILEGES}}{Privileges, die \code{DUTY_SKU} auslösen.}
#'   \item{\code{UNKNOWN_PRIVILEGE_COUNT}}{Privileges mit ungeklärter Lizenz
#'     (\code{LICENSE_STATUS} \code{UNKNOWN_PRIVILEGE} oder
#'     \code{NOT_ENTITLED}).}
#'   \item{\code{UNMEASURABLE_LICENSES}}{Lizenzteile der Duty, die keinen per
#'     Telemetrie messbaren Entry Point haben. Ist die Spalte gefüllt, kann
#'     die Telemetrie die Nichtnutzung der Duty nicht belegen.}
#'   \item{\code{SOLE_SOURCE_OF}}{SKUs, die in der Rolle nur über diese Duty
#'     benötigt werden.}
#' }
#'
#' Sortiert nach \code{DUTY_PRIORITY} (teuerste zuerst).
#'
#' @examples
#' \dontrun{
#'
#' cnn     <- get_connection("PRJ", "db_credentials.xlsx")
#' drivers <- get_role_license_drivers(cnn, "_WIBU_VERKAUF_INNENDIENST_MITARBEITER")
#'
#' # Duties, deren Entfernung eine Lizenz aus der Rolle nimmt
#' summarize_duty_license_drivers(drivers) |>
#'   dplyr::filter(SOLE_SOURCE_OF != "")
#'
#' }
#'
#' @seealso
#' \code{\link{build_role_license_drivers}}
#' \code{\link{summarize_role_license_skus}}
#'
#' @export
summarize_duty_license_drivers <- function(drivers) {

  .check_required_columns(drivers, .driver_columns(), "drivers")

  drivers |>
    dplyr::group_by(DUTYIDENTIFIER, DUTYNAME) |>
    dplyr::summarise(
      DUTY_SKU        = dplyr::first(DUTY_SKU),
      DUTY_PRIORITY   = .max_or_na(MIN_PRIORITY),
      IS_BASE_LICENSE = any(IS_BASE_LICENSE & DRIVES_DUTY),
      SKUS_IN_DUTY    = .collapse_skus_by_priority(MIN_SKU, MIN_PRIORITY),
      PRIVILEGE_COUNT = dplyr::n_distinct(PRIVILEGEIDENTIFIER),
      ENTRYPOINT_COUNT = dplyr::n_distinct(ENTRYPOINT[!is.na(ENTRYPOINT)]),
      DRIVER_PRIVILEGES = .collapse_sorted(
        PRIVILEGEIDENTIFIER[DRIVES_DUTY], .LIST_SEPARATOR
      ),
      UNKNOWN_PRIVILEGE_COUNT = dplyr::n_distinct(
        PRIVILEGEIDENTIFIER[
          LICENSE_STATUS %in% c(.LICENSE_UNKNOWN_PRIVILEGE, .LICENSE_NOT_ENTITLED)
        ]
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(.unmeasurable_licenses_per_duty(drivers), by = "DUTYIDENTIFIER") |>
    dplyr::left_join(.sole_source_skus(drivers), by = "DUTYIDENTIFIER") |>
    dplyr::mutate(
      UNMEASURABLE_LICENSES = dplyr::coalesce(UNMEASURABLE_LICENSES, ""),
      SOLE_SOURCE_OF        = dplyr::coalesce(SOLE_SOURCE_OF, "")
    ) |>
    dplyr::arrange(dplyr::desc(DUTY_PRIORITY), DUTYIDENTIFIER)
}

# SKUs, die in der Rolle ueber genau eine Duty benoetigt werden.
.sole_source_skus <- function(drivers) {
  drivers |>
    dplyr::filter(!is.na(MIN_SKU)) |>
    dplyr::distinct(DUTYIDENTIFIER, MIN_SKU, MIN_PRIORITY) |>
    dplyr::group_by(MIN_SKU) |>
    dplyr::filter(dplyr::n_distinct(DUTYIDENTIFIER) == 1L) |>
    dplyr::group_by(DUTYIDENTIFIER) |>
    dplyr::summarise(
      SOLE_SOURCE_OF = .collapse_skus_by_priority(MIN_SKU, MIN_PRIORITY),
      .groups        = "drop"
    )
}
