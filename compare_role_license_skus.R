#' Berechnete Rollenlizenzen mit der Microsoft-Sicht abgleichen
#'
#' Stellt die aus den Lizenztreibern abgeleiteten SKUs einer Rolle den SKUs
#' gegenüber, die D365FO selbst für die Rolle ausweist. Abweichungen werden
#' sichtbar gemacht statt verschwiegen.
#'
#' @details
#'
#' Die Lizenztreiber-Tabelle ist ein Modell. Erst wenn sie dieselben SKUs
#' ergibt wie D365FO, sind die daraus abgeleiteten Aussagen pro Duty und
#' Privilege belastbar. Jede Zeile mit \code{STATUS != "match"} ist vor
#' einer Entscheidung zu klären.
#'
#' @param role_skus
#' Ergebnis von \code{\link{summarize_role_license_skus}}.
#' Erforderliche Spalten: \code{SKUNAME}, \code{REQUIRED}.
#'
#' @param microsoft_assignments
#' Ergebnis von \code{\link{get_role_license_assignments}} für dieselbe
#' Rolle. Erforderliche Spalte: \code{SKUNAME}.
#'
#' @return
#'
#' Tibble mit einer Zeile pro SKU:
#'
#' \describe{
#'   \item{\code{SKUNAME}}{Lizenz.}
#'   \item{\code{IN_DRIVER_TABLE}}{\code{TRUE}, wenn die Lizenztreiber die
#'     SKU als benötigt ausweisen.}
#'   \item{\code{IN_MICROSOFT_VIEW}}{\code{TRUE}, wenn D365FO die SKU für
#'     die Rolle ausweist.}
#'   \item{\code{STATUS}}{\code{"match"}, \code{"only_driver_table"} oder
#'     \code{"only_microsoft_view"}.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn  <- get_connection("PRJ", "db_credentials.xlsx")
#' role <- "_WIBU_VERKAUF_INNENDIENST_MITARBEITER"
#'
#' compare_role_license_skus(
#'   summarize_role_license_skus(get_role_license_drivers(cnn, role)),
#'   get_role_license_assignments(cnn, role_identifiers = role)
#' )
#'
#' }
#'
#' @seealso
#' \code{\link{summarize_role_license_skus}}
#' \code{\link{get_role_license_assignments}}
#'
#' @export
compare_role_license_skus <- function(role_skus, microsoft_assignments) {

  .check_required_columns(role_skus, c("SKUNAME", "REQUIRED"), "role_skus")
  .check_required_columns(microsoft_assignments, "SKUNAME", "microsoft_assignments")

  driver_skus    <- unique(role_skus$SKUNAME[role_skus$REQUIRED])
  microsoft_skus <- unique(microsoft_assignments$SKUNAME)
  microsoft_skus <- microsoft_skus[!is.na(microsoft_skus)]

  tibble::tibble(SKUNAME = sort(union(driver_skus, microsoft_skus))) |>
    dplyr::mutate(
      IN_DRIVER_TABLE   = SKUNAME %in% driver_skus,
      IN_MICROSOFT_VIEW = SKUNAME %in% microsoft_skus,
      STATUS = dplyr::case_when(
        IN_DRIVER_TABLE & IN_MICROSOFT_VIEW ~ "match",
        IN_DRIVER_TABLE                     ~ "only_driver_table",
        TRUE                                ~ "only_microsoft_view"
      )
    )
}
