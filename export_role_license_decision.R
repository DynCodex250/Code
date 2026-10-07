#' Lizenz-Entscheidungsgrundlage nach Excel exportieren
#'
#' Schreibt das Ergebnis von \code{\link{analyze_role_license_decision}} in
#' eine Excel-Arbeitsmappe mit einem Arbeitsblatt pro Tabelle.
#'
#' @details
#'
#' Der Dateiname wird aus der Rolle gebildet:
#' \code{D365FO_License_Decision_<Rolle>.xlsx}. Zeichen, die in Dateinamen
#' nicht erlaubt sind, werden durch \code{_} ersetzt. Eine vorhandene Datei
#' gleichen Namens wird überschrieben. Leere Tabellen erhalten kein
#' Arbeitsblatt.
#'
#' @param analysis
#' Ergebnis von \code{\link{analyze_role_license_decision}}.
#'
#' @param role_identifier
#' AOT-Name der Rolle; wird für den Dateinamen verwendet.
#'
#' @param export_folder
#' Ausgabeverzeichnis. Wird angelegt, falls nicht vorhanden.
#' Standard: \code{"Data"}.
#'
#' @return
#' Pfad der geschriebenen Excel-Datei.
#'
#' @examples
#' \dontrun{
#'
#' cnn  <- get_connection("PRJ", "db_credentials.xlsx")
#' role <- "_WIBU_VERKAUF_INNENDIENST_MITARBEITER"
#'
#' analysis <- analyze_role_license_decision(cnn, role, usage = usage)
#'
#' export_role_license_decision(analysis, role)
#'
#' }
#'
#' @seealso
#' \code{\link{analyze_role_license_decision}}
#'
#' @export
export_role_license_decision <- function(
  analysis,
  role_identifier,
  export_folder = "Data"
) {

  if (!is.list(analysis) || is.data.frame(analysis)) {
    stop("analysis muss das Ergebnis von analyze_role_license_decision() sein.")
  }

  .check_single_role(role_identifier)

  if (!dir.exists(export_folder)) {
    dir.create(export_folder, recursive = TRUE)
  }

  export_path <- file.path(
    export_folder,
    paste0(
      "D365FO_License_Decision_",
      gsub("[^A-Za-z0-9_-]", "_", role_identifier),
      ".xlsx"
    )
  )

  workbook <- openxlsx::createWorkbook()

  for (sheet_name in names(analysis)) {
    if (is.data.frame(analysis[[sheet_name]])) {
      write_report_sheet(workbook, sheet_name, analysis[[sheet_name]])
    }
  }

  openxlsx::saveWorkbook(workbook, export_path, overwrite = TRUE)

  export_path
}
