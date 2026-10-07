#' Lizenztreiber einer Rolle aufbauen
#'
#' Verbindet die Sicherheitsobjekte einer Rolle (Duty, Privilege, Entry Point)
#' mit den Lizenzanforderungen je Entry Point und zeigt, welche Duty und
#' welches Privilege welche Lizenz-SKU auslöst.
#'
#' @details
#'
#' **Funktionsprinzip**
#'
#' \enumerate{
#'   \item Pro Privilege, Entry Point und Access Level wird die günstigste
#'     deckende SKU bestimmt: niedrigste \code{PRIORITY} unter allen Zeilen
#'     mit \code{ENTITLED > 0} (\code{MIN_SKU}).
#'   \item Die teuerste dieser Mindestanforderungen wird auf das Privilege
#'     (\code{PRIVILEGE_SKU}) und auf die Duty (\code{DUTY_SKU}) hochgezogen.
#'   \item \code{DRIVES_DUTY} markiert die Entry Points, die die Lizenz der
#'     Duty bestimmen.
#' }
#'
#' Die Tabelle enthält alle Entry Points der Rolle, nicht nur die
#' lizenzrelevanten. Nur so lässt sich später erkennen, ob ein Benutzer eine
#' Duty überhaupt nutzt (\code{\link{analyze_user_duty_usage}}).
#'
#' Die Funktion rechnet ausschließlich auf den übergebenen Tabellen und
#' greift nicht auf die Datenbank zu. Für den Aufruf mit Datenbankverbindung
#' siehe \code{\link{get_role_license_drivers}}.
#'
#' **Lizenzstatus (\code{LICENSE_STATUS})**
#'
#' \describe{
#'   \item{\code{LICENSED}}{Mindestens eine SKU deckt den Entry Point.}
#'   \item{\code{NOT_ENTITLED}}{Der Entry Point steht in
#'     \code{license_requirements}, aber keine SKU deckt ihn.}
#'   \item{\code{NOT_LISTED}}{Der Entry Point fehlt in
#'     \code{license_requirements}, andere Entry Points desselben Privilege
#'     stehen dort. Er gilt als nicht lizenzrelevant.}
#'   \item{\code{UNKNOWN_PRIVILEGE}}{Das Privilege fehlt vollständig in
#'     \code{license_requirements} (z.B. WIBU-Custom-Privileges). Die Lizenz
#'     ist ungeklärt und muss manuell bewertet werden.}
#' }
#'
#' Privileges, die der Rolle direkt (ohne Duty) zugewiesen sind, erscheinen
#' unter \code{DUTYIDENTIFIER = "(direkt)"}.
#'
#' @param role_objects
#' Tibble mit den Sicherheitsobjekten der Rolle aus
#' \code{UserSecGovRelatedObjects}. Erforderliche Spalten:
#' \code{ROLEIDENTIFIER}, \code{DUTYIDENTIFIER}, \code{DUTYNAME},
#' \code{PRIVILEGEIDENTIFIER}, \code{PRIVILEGENAME}, \code{RESOURCE_},
#' \code{RESOURCETYPE}.
#'
#' @param license_requirements
#' Ergebnis von \code{get_license_requirements(sku_name = NULL)}.
#' Erforderliche Spalten: \code{IDENTIFIER}, \code{AOTNAME}, \code{SKUNAME},
#' \code{PRIORITY}, \code{GROUPNAME}, \code{ENTITLED}, \code{ACCESSLEVEL}.
#'
#' @param base_group_pattern
#' Regex-Muster, das die \code{GROUPNAME} von Base-Lizenzen erkennt.
#' Standard: \code{"^Base"} (z.B. "Base - Commerce, Finance, SCM").
#'
#' @param measurable_type_pattern
#' Regex-Muster (ohne Beachtung der Groß-/Kleinschreibung) für die
#' Ressourcentypen, deren Nutzung die Telemetrie erfasst.
#' Standard: \code{"display"}. Application Insights (\code{pageViews})
#' zeichnet das Öffnen von Formularen auf, also Display Menu Items; Action
#' und Output Menu Items, Service Operations und Data Entities erscheinen
#' dort nicht.
#'
#' @return
#'
#' Tibble mit einer Zeile pro Duty, Privilege, Entry Point und Access Level:
#'
#' \describe{
#'   \item{\code{ROLEIDENTIFIER}, \code{DUTYIDENTIFIER}, \code{DUTYNAME},
#'     \code{PRIVILEGEIDENTIFIER}, \code{PRIVILEGENAME}}{Sicherheitshierarchie.}
#'   \item{\code{ENTRYPOINT}}{AOT-Name des Entry Points.}
#'   \item{\code{ENTRYPOINTTYPE}}{Ressourcentyp (z.B. "Display menu item").}
#'   \item{\code{MEASURABLE}}{\code{TRUE}, wenn die Telemetrie die Nutzung
#'     des Entry Points erfasst.}
#'   \item{\code{ACCESSLEVEL}}{1 = Read, 2 = Write.}
#'   \item{\code{LICENSE_STATUS}}{Siehe oben.}
#'   \item{\code{MIN_SKU}, \code{MIN_PRIORITY}, \code{SKU_GROUP}}{Günstigste
#'     deckende Lizenz des Entry Points.}
#'   \item{\code{IS_BASE_LICENSE}}{\code{TRUE} für Base-Lizenzen.}
#'   \item{\code{COVERING_SKUS}}{Alle deckenden SKUs, günstigste zuerst.}
#'   \item{\code{PRIVILEGE_SKU}}{Teuerste Mindestlizenz des Privilege.}
#'   \item{\code{DUTY_SKU}}{Teuerste Mindestlizenz der Duty.}
#'   \item{\code{DRIVES_DUTY}}{\code{TRUE}, wenn der Entry Point die Lizenz
#'     der Duty bestimmt.}
#' }
#'
#' Sortiert nach Lizenz der Duty (teuerste zuerst).
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' drivers <- get_role_license_drivers(cnn, "_WIBU_VERKAUF_INNENDIENST_MITARBEITER")
#'
#' # Welche Entry Points lösen Commerce aus?
#' drivers |> dplyr::filter(MIN_SKU == "Commerce")
#'
#' # Privileges mit ungeklärter Lizenz
#' drivers |> dplyr::filter(LICENSE_STATUS == "UNKNOWN_PRIVILEGE")
#'
#' }
#'
#' @seealso
#' \code{\link{get_role_license_drivers}}
#' \code{\link{summarize_duty_license_drivers}}
#' \code{\link{summarize_role_license_skus}}
#'
#' @export
build_role_license_drivers <- function(
  role_objects,
  license_requirements,
  base_group_pattern      = "^Base",
  measurable_type_pattern = "display"
) {

  .check_required_columns(
    role_objects,
    c("ROLEIDENTIFIER", "DUTYIDENTIFIER", "DUTYNAME",
      "PRIVILEGEIDENTIFIER", "PRIVILEGENAME", "RESOURCE_", "RESOURCETYPE"),
    "role_objects"
  )
  .check_required_columns(
    license_requirements,
    c("IDENTIFIER", "AOTNAME", "SKUNAME", "PRIORITY",
      "GROUPNAME", "ENTITLED", "ACCESSLEVEL"),
    "license_requirements"
  )

  .distinct_duty_privileges(role_objects) |>
    dplyr::left_join(
      .join_privilege_entrypoints(
        role_objects, license_requirements, measurable_type_pattern
      ),
      by           = "PRIVILEGEIDENTIFIER",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      IS_BASE_LICENSE = .is_base_group(SKU_GROUP, base_group_pattern),
      # Entry Points, die nur die Lizenz-View kennt, haben keinen Typ
      MEASURABLE      = dplyr::coalesce(MEASURABLE, FALSE),
      LICENSE_STATUS  = .classify_license_status(
        MIN_SKU, HAS_LICENSE_ROWS, PRIVILEGE_HAS_LICENSE_ROWS
      )
    ) |>
    .add_driver_columns() |>
    dplyr::select(dplyr::all_of(.driver_columns()))
}

# Eindeutige Duty-Privilege-Paare; direkte Privileges erhalten die Platzhalter-Duty.
.distinct_duty_privileges <- function(role_objects) {
  role_objects |>
    dplyr::filter(!is.na(PRIVILEGEIDENTIFIER), PRIVILEGEIDENTIFIER != "") |>
    dplyr::mutate(
      IS_DIRECT      = is.na(DUTYIDENTIFIER) | DUTYIDENTIFIER == "",
      DUTYIDENTIFIER = dplyr::if_else(IS_DIRECT, .DIRECT_DUTY_ID, DUTYIDENTIFIER),
      DUTYNAME       = dplyr::if_else(IS_DIRECT, .DIRECT_DUTY_NAME, DUTYNAME)
    ) |>
    dplyr::distinct(
      ROLEIDENTIFIER, DUTYIDENTIFIER, DUTYNAME,
      PRIVILEGEIDENTIFIER, PRIVILEGENAME
    )
}

# Alle Entry Points je Privilege: die Ressourcen der Rolle und die Entry
# Points der Lizenz-View. Die Schreibweise der AOT-Namen kann zwischen beiden
# Views abweichen, daher der Abgleich ueber ENTRYPOINT_KEY (Grossschreibung).
.join_privilege_entrypoints <- function(role_objects, license_requirements,
                                        measurable_type_pattern) {
  dplyr::full_join(
    .distinct_privilege_resources(role_objects, measurable_type_pattern),
    .cheapest_sku_per_entrypoint(license_requirements),
    by = c("PRIVILEGEIDENTIFIER", "ENTRYPOINT_KEY")
  ) |>
    dplyr::mutate(
      ENTRYPOINT       = dplyr::coalesce(LICENSE_NAME, RESOURCE_NAME),
      HAS_LICENSE_ROWS = dplyr::coalesce(HAS_LICENSE_ROWS, FALSE)
    ) |>
    dplyr::group_by(PRIVILEGEIDENTIFIER) |>
    dplyr::mutate(PRIVILEGE_HAS_LICENSE_ROWS = any(HAS_LICENSE_ROWS)) |>
    dplyr::ungroup() |>
    dplyr::select(-LICENSE_NAME, -RESOURCE_NAME, -ENTRYPOINT_KEY)
}

# Ein Name kann mehrere Ressourcentypen tragen (Formular und Aktion gleichen
# Namens). Die Telemetrie kennt nur den Namen und kann sie nicht unterscheiden:
# Der Name gilt nur als messbar, wenn alle seine Typen messbar sind.
.distinct_privilege_resources <- function(role_objects, measurable_type_pattern) {
  role_objects |>
    dplyr::filter(
      !is.na(PRIVILEGEIDENTIFIER), PRIVILEGEIDENTIFIER != "",
      !is.na(RESOURCE_), RESOURCE_ != ""
    ) |>
    dplyr::mutate(
      ENTRYPOINT_KEY  = toupper(RESOURCE_),
      TYPE_MEASURABLE = !is.na(RESOURCETYPE) &
        grepl(measurable_type_pattern, RESOURCETYPE, ignore.case = TRUE)
    ) |>
    dplyr::group_by(PRIVILEGEIDENTIFIER, ENTRYPOINT_KEY) |>
    dplyr::summarise(
      RESOURCE_NAME  = dplyr::first(RESOURCE_),
      ENTRYPOINTTYPE = dplyr::na_if(.collapse_sorted(RESOURCETYPE), ""),
      MEASURABLE     = all(TYPE_MEASURABLE),
      .groups        = "drop"
    )
}

# Guenstigste deckende SKU je Privilege, Entry Point und Access Level.
# Entry Points ohne deckende SKU bleiben mit MIN_SKU = NA erhalten.
.cheapest_sku_per_entrypoint <- function(license_requirements) {

  entrypoints <- license_requirements |>
    dplyr::distinct(
      PRIVILEGEIDENTIFIER = IDENTIFIER,
      ENTRYPOINT_KEY      = toupper(AOTNAME),
      LICENSE_NAME        = AOTNAME,
      ACCESSLEVEL
    ) |>
    dplyr::mutate(HAS_LICENSE_ROWS = TRUE)

  cheapest <- license_requirements |>
    dplyr::filter(!is.na(ENTITLED), ENTITLED > 0) |>
    dplyr::distinct(
      PRIVILEGEIDENTIFIER = IDENTIFIER,
      LICENSE_NAME        = AOTNAME,
      ACCESSLEVEL, SKUNAME, PRIORITY, GROUPNAME
    ) |>
    dplyr::arrange(PRIORITY, SKUNAME) |>
    dplyr::group_by(PRIVILEGEIDENTIFIER, LICENSE_NAME, ACCESSLEVEL) |>
    dplyr::summarise(
      MIN_SKU       = dplyr::first(SKUNAME),
      MIN_PRIORITY  = dplyr::first(PRIORITY),
      SKU_GROUP     = dplyr::first(GROUPNAME),
      COVERING_SKUS = paste(unique(SKUNAME), collapse = .SKU_SEPARATOR),
      .groups       = "drop"
    )

  dplyr::left_join(
    entrypoints, cheapest,
    by = c("PRIVILEGEIDENTIFIER", "LICENSE_NAME", "ACCESSLEVEL")
  )
}

.classify_license_status <- function(min_sku, has_license_rows,
                                     privilege_has_license_rows) {
  dplyr::case_when(
    !is.na(min_sku)                                    ~ .LICENSE_LICENSED,
    dplyr::coalesce(has_license_rows, FALSE)           ~ .LICENSE_NOT_ENTITLED,
    dplyr::coalesce(privilege_has_license_rows, FALSE) ~ .LICENSE_NOT_LISTED,
    TRUE                                               ~ .LICENSE_UNKNOWN_PRIVILEGE
  )
}

# Zieht die teuerste Mindestlizenz auf Privilege und Duty hoch und sortiert.
.add_driver_columns <- function(drivers) {
  drivers |>
    dplyr::group_by(ROLEIDENTIFIER, PRIVILEGEIDENTIFIER) |>
    dplyr::mutate(
      PRIVILEGE_SKU = .sku_with_highest_priority(MIN_SKU, MIN_PRIORITY)
    ) |>
    dplyr::group_by(ROLEIDENTIFIER, DUTYIDENTIFIER) |>
    dplyr::mutate(
      DUTY_SKU      = .sku_with_highest_priority(MIN_SKU, MIN_PRIORITY),
      DUTY_PRIORITY = .max_or_na(MIN_PRIORITY),
      DRIVES_DUTY   = !is.na(MIN_PRIORITY) & MIN_PRIORITY == DUTY_PRIORITY
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      dplyr::desc(DUTY_PRIORITY),   # teuerste Duty zuerst
      DUTYIDENTIFIER,
      dplyr::desc(MIN_PRIORITY),
      PRIVILEGEIDENTIFIER,
      ENTRYPOINT
    )
}
