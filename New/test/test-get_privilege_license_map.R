mock_privilege_sku_rows <- function() {
  data.frame(
    PRIVILEGEIDENTIFIER = c("PrivA", "PrivA", "PrivA", "PrivB"),
    SECURITYPRIVILEGE   = c("Priv A", "Priv A", "Priv A", "Priv B"),
    SKUNAME             = c("Finance", "Supply Chain Management", "Finance Premium", "Commerce"),
    GROUPNAME           = "Base - Commerce, Finance, SCM",
    PRIORITY            = c(70L, 70L, 90L, 60L)
  )
}

test_that("get_privilege_license_map reads the privilege summary view", {
  # Arrange
  statement_seen <- NULL
  local_mocked_query(function(statement) {
    statement_seen <<- statement
    mock_privilege_sku_rows()
  })

  # Act
  get_privilege_license_map(con = NULL)

  # Assert
  expect_match(statement_seen, "LICENSINGPRIVILEGEREQUIREMENTSSUMMARYVIEW", fixed = TRUE)
  expect_no_match(statement_seen, "LICENSINGPRIVILEGESREQUIREMENTSSUMMARYVIEW", fixed = TRUE)
})

test_that("get_privilege_license_map flags the cheapest SKUs per privilege", {
  # Arrange
  local_mocked_query(function(statement) mock_privilege_sku_rows())

  # Act
  res <- get_privilege_license_map(con = NULL)

  # Assert
  expect_equal(res$minimum_required, c(TRUE, TRUE, FALSE, TRUE))
})

test_that("get_privilege_license_map escapes and applies the privilege filter", {
  # Arrange
  statement_seen <- NULL
  local_mocked_query(function(statement) {
    statement_seen <<- statement
    mock_privilege_sku_rows()
  })

  # Act
  get_privilege_license_map(con = NULL, privilege_identifiers = c("PrivA", "O'Brien"))

  # Assert
  expect_match(statement_seen, "sp.IDENTIFIER IN ('PrivA', 'O''Brien')", fixed = TRUE)
})
