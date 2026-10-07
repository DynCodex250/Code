# Ersetzt DBI::dbGetQuery() fuer die Dauer eines Tests.
#
# responder: function(statement) -> data.frame
# Die Tests entscheiden anhand des SQL-Textes, welche Daten zurueckkommen.
local_mocked_query <- function(responder, env = parent.frame()) {
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) responder(statement),
    .package = "DBI",
    .env = env
  )
}
