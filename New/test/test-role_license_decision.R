local_mocked_decision_sources <- function(user_licenses = fixture_user_licenses(),
                                          assigned_ids  = fixture_user_ids(),
                                          env           = parent.frame()) {
  local_mocked_bindings(
    get_role_license_drivers = function(con, role_identifier, ...) fixture_drivers(),
    get_sql_user_role_assignments = function(con, ...) {
      tibble::tibble(USERID = assigned_ids, SECURITYROLEIDENTIFIER = "ROLE_SALES")
    },
    get_user_licenses_by_role = function(connection, user_ids = NULL, ...) {
      # die echte Funktion erzeugt bei leerer Liste ungueltiges SQL ("IN ()")
      if (length(user_ids) == 0L) stop("get_user_licenses_by_role ohne Benutzer aufgerufen")
      user_licenses[user_licenses$USERID %in% user_ids, ]
    },
    get_role_license_assignments = function(con, ...) {
      tibble::tibble(SKUNAME = c("Commerce", "Finance", "Supply Chain Management"))
    },
    .env = env
  )
}

test_that("analyze_role_license_decision returns every table of the decision workbook", {
  local_mocked_decision_sources()

  res <- analyze_role_license_decision(con = NULL, "ROLE_SALES", usage = fixture_usage())

  expect_named(
    res,
    c(
      "Savings", "UserSummary", "UserDecisions", "UserDutyUsage",
      "DutyDrivers", "EntryPointDrivers", "RoleLicenses", "Reconciliation",
      "DataQuality", "Legend"
    )
  )
  expect_equal(res$EntryPointDrivers, fixture_drivers())
  expect_equal(res$UserDutyUsage, fixture_duty_usage())
  expect_equal(unique(res$Reconciliation$STATUS), "match")
  expect_equal(res$Savings$USERS_SAVED, c(2L, 0L, 0L))
})

test_that("analyze_role_license_decision excludes disabled users", {
  # Arrange
  user_licenses <- fixture_user_licenses() |>
    dplyr::mutate(USERENABLED = dplyr::if_else(USERID == "carol", 0L, 1L))
  local_mocked_decision_sources(user_licenses = user_licenses)

  # Act
  res <- analyze_role_license_decision(con = NULL, "ROLE_SALES", usage = fixture_usage())

  # Assert
  expect_false("carol" %in% res$UserSummary$USERID)
  expect_false("carol" %in% res$UserDutyUsage$USERID)
  expect_equal(res$Savings$USERS_BEFORE, c(3L, 3L, 3L))
})

test_that("analyze_role_license_decision reports how far telemetry can be trusted", {
  local_mocked_decision_sources()

  res     <- analyze_role_license_decision(con = NULL, "ROLE_SALES", usage = fixture_usage())
  quality <- stats::setNames(res$DataQuality$WERT, res$DataQuality$KENNZAHL)

  expect_equal(quality[["Benutzer mit Rolle"]], "4")
  expect_equal(quality[["davon in der Telemetrie gefunden"]], "3")
  expect_equal(quality[["davon ohne Lizenzzeile in D365FO"]], "0")
  expect_equal(quality[["Entry Points der Rolle"]], "6")
  expect_equal(quality[["davon per Telemetrie messbar"]], "4")
  # alice: 3 Namen, bob: 1, carol: 1 -> 4 verschiedene, davon 3 in der Rolle
  expect_equal(quality[["Entry Points in der Telemetrie der Rollenbenutzer"]], "4")
  expect_equal(quality[["davon in der Rolle enthalten"]], "3")
  expect_equal(quality[["Deaktivierte Benutzer (nicht ausgewertet)"]], "0")
  expect_equal(quality[["Ältester letzter Zugriff"]], "2026-06-30")
  expect_equal(quality[["Neuester letzter Zugriff"]], "2026-09-20")
})

test_that("analyze_role_license_decision counts role users D365FO has no license row for", {
  # Arrange: dave hat die Rolle, aber keine Zeile in LICENSINGUSERLICENSESBYROLE
  user_licenses <- fixture_user_licenses() |>
    dplyr::filter(USERID != "dave")
  local_mocked_decision_sources(user_licenses = user_licenses)

  # Act
  res     <- analyze_role_license_decision(con = NULL, "ROLE_SALES", usage = fixture_usage())
  quality <- stats::setNames(res$DataQuality$WERT, res$DataQuality$KENNZAHL)

  # Assert
  expect_equal(quality[["davon ohne Lizenzzeile in D365FO"]], "1")
  expect_equal(res$UserSummary$BASE_LICENSES_BEFORE[res$UserSummary$USERID == "dave"], 0L)
  expect_false(any(res$UserDecisions$SAVES_IF_CONFIRMED[res$UserDecisions$USERID == "dave"]))
})

test_that("analyze_role_license_decision passes the measurable types to the driver loader", {
  # Arrange
  captured <- NULL
  local_mocked_decision_sources()
  local_mocked_bindings(
    get_role_license_drivers = function(con, role_identifier, ...) {
      captured <<- list(...)
      fixture_drivers()
    }
  )

  # Act
  analyze_role_license_decision(
    con = NULL, "ROLE_SALES",
    base_group_pattern = "^Basis", measurable_type_pattern = "display|output"
  )

  # Assert
  expect_equal(unlist(captured, use.names = FALSE), c("^Basis", "display|output"))
})

test_that("analyze_role_license_decision works without telemetry and without users", {
  local_mocked_decision_sources(assigned_ids = character())

  res <- analyze_role_license_decision(con = NULL, "ROLE_SALES")

  expect_equal(nrow(res$UserDutyUsage), 0L)
  expect_equal(nrow(res$UserSummary), 0L)
  expect_equal(nrow(res$Savings), 0L)
  expect_equal(nrow(res$DutyDrivers), 5L)
})

test_that("export_role_license_decision writes one sheet per non-empty table", {
  # Arrange
  local_mocked_decision_sources()
  analysis      <- analyze_role_license_decision(con = NULL, "ROLE_SALES", usage = fixture_usage())
  export_folder <- tempfile("license-decision-")
  dir.create(export_folder)

  # Act
  path <- export_role_license_decision(analysis, "ROLE_SALES", export_folder)

  # Assert
  expect_true(file.exists(path))
  expect_equal(basename(path), "D365FO_License_Decision_ROLE_SALES.xlsx")
  expect_equal(openxlsx::getSheetNames(path), names(analysis))
  written <- openxlsx::read.xlsx(path, sheet = "Savings")
  expect_equal(written$SKUNAME, c("Commerce", "Finance", "Supply Chain Management"))
})

test_that("export_role_license_decision creates the folder and sanitises the file name", {
  # Arrange
  local_mocked_decision_sources()
  analysis      <- analyze_role_license_decision(con = NULL, "ROLE_SALES", usage = fixture_usage())
  export_folder <- file.path(tempfile("license-decision-"), "new", "folder")

  # Act
  path <- export_role_license_decision(analysis, "_WIBU Verkauf/Innendienst", export_folder)

  # Assert
  expect_true(file.exists(path))
  expect_equal(basename(path), "D365FO_License_Decision__WIBU_Verkauf_Innendienst.xlsx")
})

test_that("export_role_license_decision rejects anything but an analysis result", {
  expect_error(
    export_role_license_decision(tibble::tibble(x = 1), "ROLE_SALES", tempdir()),
    "analyze_role_license_decision"
  )
})
