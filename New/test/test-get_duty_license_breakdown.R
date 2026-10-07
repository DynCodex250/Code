mock_breakdown_hierarchy <- function() {
  tibble::tibble(
    ROLEIDENTIFIER      = "ROLE_A",
    DUTYIDENTIFIER      = c("DutyRead", "DutyCommerce", "DutySCM", "DutySCM", NA),
    PRIVILEGEIDENTIFIER = c("PrivRead", "PrivRetail", "PrivWhs", "PrivRead", "PrivDirect")
  )
}

mock_breakdown_licenses <- function() {
  tibble::tribble(
    ~IDENTIFIER,  ~SKUNAME,                  ~ENTITLED,
    "PrivRead",   "Operations - Activity",   1,
    "PrivRead",   "Supply Chain Management", 1,
    "PrivRetail", "Operations - Activity",   0,
    "PrivRetail", "Commerce",                1,
    "PrivWhs",    "Operations - Activity",   0,
    "PrivWhs",    "Commerce",                0,
    "PrivWhs",    "Supply Chain Management", 1
  )
}

mock_duty_names <- function(statement) {
  data.frame(
    DUTYIDENTIFIER = c("DutyRead", "DutyCommerce", "DutySCM"),
    DUTYNAME       = c("Read duty", "Commerce duty", "SCM duty")
  )
}

test_that("get_duty_license_breakdown filters the hierarchy by role", {
  # Arrange
  captured <- NULL
  local_mocked_bindings(
    get_sql_security_hierarchy = function(con, ...) {
      captured <<- list(...)
      mock_breakdown_hierarchy()
    },
    get_license_requirements = function(con, ...) mock_breakdown_licenses()
  )
  local_mocked_query(mock_duty_names)

  # Act
  get_duty_license_breakdown(con = NULL, role_identifier = "ROLE_A")

  # Assert
  expect_equal(captured$role_identifiers, "ROLE_A")
  expect_null(captured$duty_identifiers)
})

test_that("get_duty_license_breakdown returns one row per duty, highest license first", {
  # Arrange
  local_mocked_bindings(
    get_sql_security_hierarchy = function(con, ...) mock_breakdown_hierarchy(),
    get_license_requirements   = function(con, ...) mock_breakdown_licenses()
  )
  local_mocked_query(mock_duty_names)

  # Act
  res <- get_duty_license_breakdown(con = NULL, role_identifier = "ROLE_A")

  # Assert
  expect_equal(
    names(res),
    c("DUTYIDENTIFIER", "DUTYNAME", "LICENSE_CATEGORY", "LICENSE_DETAIL", "PRIVILEGE_COUNT")
  )
  expect_equal(res$DUTYIDENTIFIER, c("(direkt)", "DutyCommerce", "DutySCM", "DutyRead"))
  expect_equal(res$LICENSE_CATEGORY, c("Unbekannt", "Commerce", "Finance/SCM", "Activity"))
  expect_equal(res$LICENSE_DETAIL[res$DUTYIDENTIFIER == "DutySCM"], "Activity, Finance/SCM")
  expect_equal(res$PRIVILEGE_COUNT[res$DUTYIDENTIFIER == "DutySCM"], 2L)
  expect_equal(res$DUTYNAME[res$DUTYIDENTIFIER == "DutyRead"], "Read duty")
})

test_that("get_duty_license_breakdown stops when the role has no hierarchy", {
  local_mocked_bindings(
    get_sql_security_hierarchy = function(con, ...) mock_breakdown_hierarchy()[0, ]
  )

  expect_error(
    get_duty_license_breakdown(con = NULL, role_identifier = "UNKNOWN_ROLE"),
    "Keine Hierarchie gefunden"
  )
})
