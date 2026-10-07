#' Lizenztreiber einer Rolle aus der Datenbank laden
#'
#' Lädt die Sicherheitsobjekte einer Rolle und deren Lizenzanforderungen und
#' gibt die Lizenztreiber-Tabelle zurück: welche Duty, welches Privilege und
#' welcher Entry Point welche Lizenz-SKU auslöst.
#'
#' @details
#'
#' **Datenquellen**
#'
#' \describe{
#'   \item{\code{UserSecGovRelatedObjects}}{Rolle, Duty, Privilege und
#'     Ressource. Anders als \code{\link{get_sql_security_hierarchy}} wird
#'     nicht auf Menüelemente eingeschränkt, damit Privileges erhalten
#'     bleiben, die nur Data Entities oder Service Operations berechtigen.}
#'   \item{\code{LICENSINGPRIVILEGEREQUIREMENTSDETAILEDVIEW}}{Lizenz-SKUs je
#'     Privilege und Entry Point, über
#'     \code{\link{get_license_requirements}}.}
#' }
#'
#' Die Berechnung selbst steht in \code{\link{build_role_license_drivers}}.
#'
#' @param con
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param role_identifier
#' AOT-Name genau einer Rolle (\code{ROLEIDENTIFIER}).
#'
#' @param base_group_pattern
#' Regex-Muster, das die \code{GROUPNAME} von Base-Lizenzen erkennt.
#' Standard: \code{"^Base"}.
#'
#' @param measurable_type_pattern
#' Regex-Muster für die Ressourcentypen, deren Nutzung die Telemetrie
#' erfasst. Standard: \code{"display"}.
#'
#' @return
#' Tibble der Lizenztreiber, siehe \code{\link{build_role_license_drivers}}.
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' drivers <- get_role_license_drivers(cnn, "_WIBU_VERKAUF_INNENDIENST_MITARBEITER")
#'
#' # Lizenz pro Duty
#' summarize_duty_license_drivers(drivers)
#'
#' # Lizenzen der Rolle und die Duties dahinter
#' summarize_role_license_skus(drivers)
#'
#' }
#'
#' @seealso
#' \code{\link{build_role_license_drivers}}
#' \code{\link{summarize_duty_license_drivers}}
#' \code{\link{analyze_role_license_decision}}
#'
#' @export
get_role_license_drivers <- function(
  con,
  role_identifier,
  base_group_pattern      = "^Base",
  measurable_type_pattern = "display"
) {

  .check_single_role(role_identifier)

  role_objects <- .load_role_security_objects(con, role_identifier)

  if (nrow(role_objects) == 0L) {
    stop("Keine Hierarchie gefunden fuer Rolle: '", role_identifier, "'")
  }

  privilege_ids <- unique(role_objects$PRIVILEGEIDENTIFIER)
  privilege_ids <- privilege_ids[!is.na(privilege_ids) & nzchar(privilege_ids)]

  # Ohne diese Pruefung wuerde get_license_requirements() ungefiltert
  # die Lizenzzeilen aller Privileges des Systems laden.
  if (length(privilege_ids) == 0L) {
    stop("Rolle '", role_identifier, "' enthaelt keine Privileges.")
  }

  license_requirements <- get_license_requirements(
    con,
    privilege_identifiers = privilege_ids,
    sku_name              = NULL   # kein SKU-Filter → alle SKUs
  ) |> tibble::as_tibble()

  build_role_license_drivers(
    role_objects, license_requirements,
    base_group_pattern      = base_group_pattern,
    measurable_type_pattern = measurable_type_pattern
  )
}

# Alle Sicherheitsobjekte einer Rolle, ohne Einschraenkung auf einen Ressourcentyp.
.load_role_security_objects <- function(con, role_identifier) {

  query <- paste0("
    SELECT DISTINCT
      USGRO.ROLEIDENTIFIER,
      USGRO.DUTYIDENTIFIER,
      USGRO.DUTYNAME,
      USGRO.PRIVILEGEIDENTIFIER,
      USGRO.PRIVILEGENAME,
      USGRO.RESOURCE_,
      USGRO.RESOURCETYPE
    FROM UserSecGovRelatedObjects USGRO
    WHERE USGRO.ROLEIDENTIFIER = '", gsub("'", "''", role_identifier), "'
  ")

  DBI::dbGetQuery(con, query) |>
    tibble::as_tibble()
}
