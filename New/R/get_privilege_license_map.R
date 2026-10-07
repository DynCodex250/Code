#' Lizenz-SKU-Zuordnung je Privilege
#'
#' Gibt für jedes Privilege alle deckenden Lizenz-SKUs zurück —
#' geordnet nach PRIORITY (günstigste zuerst) — und markiert
#' die Mindestlizenz je Privilege (\code{minimum_required = TRUE}).
#'
#' @details
#'
#' Die Funktion liest aus
#' \code{LICENSINGPRIVILEGEREQUIREMENTSSUMMARYVIEW}
#' und filtert auf \code{ENTITLED = 1}.
#'
#' Im Gegensatz zu \code{get_license_requirements()},
#' das pro SKU alle damit abgedeckten Privileges liefert,
#' dreht diese Funktion die Perspektive:
#' pro Privilege werden alle deckenden SKUs aufgelistet.
#'
#' Das Feld \code{minimum_required} zeigt die günstigste Lizenz,
#' die dieses Privilege abdeckt. Mehrere Zeilen mit
#' \code{minimum_required = TRUE} pro Privilege entstehen,
#' wenn zwei SKUs dieselbe PRIORITY haben.
#'
#' @param con
#' Datenbankverbindung. Ergebnis von \code{get_connection()}.
#'
#' @param privilege_identifiers
#' Optionaler Character-Vektor mit Privilege-Identifiern
#' (\code{IDENTIFIER} aus \code{SECURITYPRIVILEGE}).
#' Wenn leer, werden alle Privileges zurückgegeben.
#'
#' @return
#'
#' Tibble mit einer Zeile pro Privilege-SKU-Kombination
#' (nur \code{ENTITLED = 1}).
#'
#' \describe{
#'   \item{PRIVILEGEIDENTIFIER}{Privilege-Identifier (AOT-Name).}
#'   \item{SECURITYPRIVILEGE}{Anzeigename des Privilege.}
#'   \item{SKUNAME}{Name der Lizenz.}
#'   \item{GROUPNAME}{Lizenzgruppe (z.B. "Base - Commerce, Finance, SCM").}
#'   \item{PRIORITY}{Lizenz-Priorität (niedrig = günstiger).}
#'   \item{minimum_required}{
#'     \code{TRUE} wenn diese SKU die günstigste deckende
#'     Lizenz für dieses Privilege ist.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#'
#' cnn <- get_connection("PRJ", "db_credentials.xlsx")
#'
#' # Alle Privileges mit ihren Lizenzen
#' get_privilege_license_map(cnn)
#'
#' # Nur bestimmte Privileges
#' get_privilege_license_map(
#'   cnn,
#'   privilege_identifiers = c(
#'     "VENDEDITINVOICE",
#'     "RETAILSMBACCOUNTEXPORTRESULTENTITYVIEW"
#'   )
#' )
#'
#' # Nur Mindestlizenzen
#' get_privilege_license_map(cnn, "VENDEDITINVOICE") |>
#'   dplyr::filter(minimum_required)
#'
#' }
#'
#' @seealso
#' \code{\link{get_license_requirements}}
#' \code{\link{get_role_license_requirements}}
#'
#' @export
get_privilege_license_map <- function(
  con,
  privilege_identifiers = character()
) {

  priv_filter <- if (length(privilege_identifiers) > 0L) {
    quoted <- paste0("'", gsub("'", "''", privilege_identifiers), "'", collapse = ", ")
    paste0("AND sp.IDENTIFIER IN (", quoted, ")")
  } else {
    ""
  }

  query <- paste0("
    SELECT
      sp.IDENTIFIER  AS PRIVILEGEIDENTIFIER,
      sp.NAME        AS SECURITYPRIVILEGE,
      lask.SKUNAME,
      lprs.GROUPNAME,
      lprs.PRIORITY
    FROM LICENSINGPRIVILEGEREQUIREMENTSSUMMARYVIEW lprs
    LEFT JOIN SECURITYPRIVILEGE sp   ON sp.RECID   = lprs.SECURITYPRIVILEGE
    LEFT JOIN LICENSINGALLSKUS  lask ON lask.RECID = lprs.SKURECID
    WHERE lprs.ENTITLED = 1
    ", priv_filter, "
    ORDER BY sp.IDENTIFIER, lprs.PRIORITY ASC
  ")

  DBI::dbGetQuery(con, query) |>
    tibble::as_tibble() |>
    dplyr::group_by(PRIVILEGEIDENTIFIER) |>
    dplyr::mutate(minimum_required = PRIORITY == min(PRIORITY)) |>
    dplyr::ungroup()
}
