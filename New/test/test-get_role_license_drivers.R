test_that("get_role_license_drivers loads the role objects and their license rows", {
  # Arrange
  statement_seen <- NULL
  captured       <- NULL
  local_mocked_query(function(statement) {
    statement_seen <<- statement
    fixture_role_objects()
  })
  local_mocked_bindings(
    get_license_requirements = function(con, ...) {
      captured <<- list(...)
      fixture_license_requirements()
    }
  )

  # Act
  res <- get_role_license_drivers(con = NULL, role_identifier = "ROLE_SALES")

  # Assert
  expect_equal(res, fixture_drivers())
  expect_match(statement_seen, "ROLEIDENTIFIER = 'ROLE_SALES'", fixed = TRUE)
  expect_setequal(
    captured$privilege_identifiers,
    unique(fixture_role_objects()$PRIVILEGEIDENTIFIER)
  )
  expect_null(captured$sku_name)
})

test_that("get_role_license_drivers does not restrict the hierarchy to menu items", {
  # Arrange
  statement_seen <- NULL
  local_mocked_query(function(statement) {
    statement_seen <<- statement
    fixture_role_objects()
  })
  local_mocked_bindings(
    get_license_requirements = function(con, ...) fixture_license_requirements()
  )

  # Act
  get_role_license_drivers(con = NULL, role_identifier = "ROLE_SALES")

  # Assert
  expect_no_match(statement_seen, "menu item", fixed = TRUE)
  expect_match(statement_seen, "RESOURCETYPE", fixed = TRUE)
})

test_that("get_role_license_drivers escapes quotes in the role identifier", {
  # Arrange
  statement_seen <- NULL
  local_mocked_query(function(statement) {
    statement_seen <<- statement
    fixture_role_objects()
  })
  local_mocked_bindings(
    get_license_requirements = function(con, ...) fixture_license_requirements()
  )

  # Act
  get_role_license_drivers(con = NULL, role_identifier = "O'Role")

  # Assert
  expect_match(statement_seen, "ROLEIDENTIFIER = 'O''Role'", fixed = TRUE)
})

test_that("get_role_license_drivers stops when the role has no security objects", {
  local_mocked_query(function(statement) fixture_role_objects()[0, ])

  expect_error(
    get_role_license_drivers(con = NULL, role_identifier = "UNKNOWN_ROLE"),
    "Keine Hierarchie gefunden"
  )
})

test_that("get_role_license_drivers never asks for license rows without a privilege filter", {
  # Arrange: Rollenobjekte ohne Privilege (z.B. reine Duty-Zeilen)
  without_privileges <- fixture_role_objects() |>
    dplyr::mutate(PRIVILEGEIDENTIFIER = NA_character_)
  local_mocked_query(function(statement) without_privileges)
  local_mocked_bindings(
    get_license_requirements = function(con, ...) stop("ungefilterter Aufruf")
  )

  # Act + Assert
  expect_error(
    get_role_license_drivers(con = NULL, role_identifier = "ROLE_SALES"),
    "enthaelt keine Privileges"
  )
})

test_that("get_role_license_drivers drops missing privilege identifiers from the filter", {
  # Arrange
  captured     <- NULL
  with_missing <- dplyr::bind_rows(
    fixture_role_objects(),
    fixture_role_objects()[1, ] |> dplyr::mutate(PRIVILEGEIDENTIFIER = NA_character_)
  )
  local_mocked_query(function(statement) with_missing)
  local_mocked_bindings(
    get_license_requirements = function(con, ...) {
      captured <<- list(...)
      fixture_license_requirements()
    }
  )

  # Act
  res <- get_role_license_drivers(con = NULL, role_identifier = "ROLE_SALES")

  # Assert
  expect_false(anyNA(captured$privilege_identifiers))
  expect_equal(res, fixture_drivers())
})

test_that("get_role_license_drivers accepts exactly one role", {
  expect_error(
    get_role_license_drivers(con = NULL, role_identifier = c("ROLE_A", "ROLE_B")),
    "genau eine Rolle"
  )
  expect_error(
    get_role_license_drivers(con = NULL, role_identifier = character()),
    "genau eine Rolle"
  )
})
