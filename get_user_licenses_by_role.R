#' Lizenzen pro Benutzer und Rolle auslesen
#'
#' Liest die Lizenzinformationen direkt aus der D365FO-Standardsicht
#' \code{LICENSINGUSERLICENSESBYROLE}.
#'
#' Die Lizenzberechnung erfolgt durch D365FO selbst —
#' diese Funktion liest das Ergebnis nur aus.
#'
#' @param connection
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param user_ids
#' Optionaler Character-Vektor mit Benutzer-IDs.
#' \code{NULL} = alle Benutzer.
#'
#' @param role_identifiers
#' Optionaler Character-Vektor mit Rollen-AOTNAMEs (ROLEIDENTIFIER).
#' \code{NULL} = alle Rollen.
#'
#' @return
#'
#' Tibble mit einer Zeile pro Benutzer-Rollen-Kombination.
#'
#' Spalten aus \code{LICENSINGUSERLICENSESBYROLE}:
#'
#' \describe{
#'   \item{USERID}{Benutzerkennung.}
#'   \item{USERENABLED}{1 = aktiv, 0 = deaktiviert.}
#'   \item{SECURITYROLERECID}{Interne Rollen-ID.}
#'   \item{ROLEIDENTIFIER}{AOT-Name der Rolle.}
#'   \item{SECURITYROLENAME}{Anzeigename der Rolle.}
#'   \item{SKURECID}{Interne Lizenz-ID.}
#'   \item{SKUNAME}{Name der Lizenz (z.B. \code{"Team Members"}, \code{"Operations - Activity"}).}
#'   \item{SKUGROUP}{Lizenzgruppe.}
#'   \item{REQUIREDUSERLICENSE}{Pflichtlizenz-Kennzeichen.}
#'   \item{USERREQUIREDLICENSEQTY}{Anzahl benötigter Lizenzen.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' # Alle Lizenzen aller Benutzer
#' get_user_licenses_by_role(cnn)
#'
#' # Nur bestimmte Benutzer
#' get_user_licenses_by_role(cnn, user_ids = c("wibule01", "wibule02"))
#'
#' # Nur eine bestimmte Rolle
#' get_user_licenses_by_role(cnn, role_identifiers = "_WIBU_VERKAUF_KEYACCOUNT")
#'
#' }
#'
#' @seealso
#' \code{\link{get_connection}}
#' \code{\link{sec_check_license_impact}}
#'
#' @export
get_user_licenses_by_role <- function(
    connection,
    user_ids         = NULL,
    role_identifiers = NULL) {

  where_clauses <- character(0)

  if (!is.null(user_ids)) {
    placeholders <- paste(paste0("'", gsub("'", "''", user_ids), "'"), collapse = ", ")
    where_clauses <- c(where_clauses, paste0("lulr.USERID IN (", placeholders, ")"))
  }

  if (!is.null(role_identifiers)) {
    placeholders <- paste(paste0("'", gsub("'", "''", role_identifiers), "'"), collapse = ", ")
    where_clauses <- c(where_clauses, paste0("sr.AOTNAME IN (", placeholders, ")"))
  }

  where_sql <- if (length(where_clauses) > 0L) {
    paste("WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    ""
  }

  sql <- paste0("
    SELECT
      lulr.USERID,
      lulr.USERENABLED,
      lulr.SECURITYROLERECID,
      sr.AOTNAME          AS ROLEIDENTIFIER,
      lulr.SECURITYROLENAME,
      lulr.SKURECID,
      lulr.SKUNAME,
      lulr.SKUGROUP,
      lulr.REQUIREDUSERLICENSE,
      lulr.USERREQUIREDLICENSEQTY
    FROM  LICENSINGUSERLICENSESBYROLE lulr
    LEFT JOIN SECURITYROLE sr ON sr.RECID = lulr.SECURITYROLERECID
    ", where_sql, "
    ORDER BY lulr.USERID, lulr.SKUNAME
  ")

  DBI::dbGetQuery(connection, sql) |>
    tibble::as_tibble()
}
