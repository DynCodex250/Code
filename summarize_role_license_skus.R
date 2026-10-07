#' Lizenz-SKUs einer Rolle zusammenfassen
#'
#' Zeigt pro Lizenz-SKU, wie viele Duties, Privileges und Entry Points der
#' Rolle sie als Mindestlizenz auslösen — und welche Duties entfernt werden
#' müssten, damit die Rolle diese SKU nicht mehr benötigt.
#'
#' @details
#'
#' \code{REQUIRED} folgt der Collapse-Regel aus
#' \code{\link{get_role_license_requirements}}: Sobald die Rolle mindestens
#' eine Base-Lizenz benötigt, entfallen Light-Lizenzen (Team Members,
#' Operations - Activity), weil Base-Lizenzen diese einschließen.
#'
#' Das Ergebnis ist ein Modell auf Basis der Lizenz-Views. Die maßgebliche
#' Aussage von D365FO steht in \code{LICENSINGROLELICENSEASSIGNMENTS};
#' \code{\link{compare_role_license_skus}} stellt beide gegenüber.
#'
#' @param drivers
#' Ergebnis von \code{\link{build_role_license_drivers}} bzw.
#' \code{\link{get_role_license_drivers}}.
#'
#' @return
#'
#' Tibble mit einer Zeile pro SKU:
#'
#' \describe{
#'   \item{\code{SKUNAME}, \code{PRIORITY}, \code{SKU_GROUP}}{Lizenz.}
#'   \item{\code{IS_BASE_LICENSE}}{\code{TRUE} für Base-Lizenzen.}
#'   \item{\code{DUTY_COUNT}, \code{PRIVILEGE_COUNT},
#'     \code{ENTRYPOINT_COUNT}}{Anzahl der Objekte, deren Mindestlizenz
#'     diese SKU ist.}
#'   \item{\code{DUTIES}}{Die betroffenen Duties, kommagetrennt.}
#'   \item{\code{REQUIRED}}{\code{TRUE}, wenn die Rolle die SKU nach der
#'     Collapse-Regel benötigt.}
#' }
#'
#' Sortiert nach \code{PRIORITY} (teuerste zuerst).
#'
#' @examples
#' \dontrun{
#'
#' cnn     <- get_connection("PRJ", "db_credentials.xlsx")
#' drivers <- get_role_license_drivers(cnn, "_WIBU_VERKAUF_INNENDIENST_MITARBEITER")
#'
#' summarize_role_license_skus(drivers)
#'
#' }
#'
#' @seealso
#' \code{\link{summarize_duty_license_drivers}}
#' \code{\link{compare_role_license_skus}}
#'
#' @export
summarize_role_license_skus <- function(drivers) {

  .check_required_columns(drivers, .driver_columns(), "drivers")

  role_skus <- drivers |>
    dplyr::filter(!is.na(MIN_SKU)) |>
    dplyr::group_by(
      SKUNAME  = MIN_SKU,
      PRIORITY = MIN_PRIORITY,
      SKU_GROUP,
      IS_BASE_LICENSE
    ) |>
    dplyr::summarise(
      DUTY_COUNT       = dplyr::n_distinct(DUTYIDENTIFIER),
      PRIVILEGE_COUNT  = dplyr::n_distinct(PRIVILEGEIDENTIFIER),
      ENTRYPOINT_COUNT = dplyr::n_distinct(ENTRYPOINT),
      DUTIES           = .collapse_sorted(DUTYIDENTIFIER, .LIST_SEPARATOR),
      .groups          = "drop"
    )

  role_needs_base_license <- any(role_skus$IS_BASE_LICENSE)

  role_skus |>
    dplyr::mutate(
      REQUIRED = if (role_needs_base_license) IS_BASE_LICENSE else TRUE
    ) |>
    dplyr::arrange(dplyr::desc(PRIORITY), SKUNAME)
}
