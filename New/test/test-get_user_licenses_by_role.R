test_that("get_user_licenses_by_role escapes quotes in user and role filters", {
  # Arrange
  statement_seen <- NULL
  local_mocked_query(function(statement) {
    statement_seen <<- statement
    data.frame(USERID = character(), SKUNAME = character())
  })

  # Act
  get_user_licenses_by_role(
    connection       = NULL,
    user_ids         = c("alice", "o'brien"),
    role_identifiers = "O'Role"
  )

  # Assert
  expect_match(statement_seen, "lulr.USERID IN ('alice', 'o''brien')", fixed = TRUE)
  expect_match(statement_seen, "sr.AOTNAME IN ('O''Role')", fixed = TRUE)
})

test_that("get_user_licenses_by_role applies no filter when none is given", {
  # Arrange
  statement_seen <- NULL
  local_mocked_query(function(statement) {
    statement_seen <<- statement
    data.frame(USERID = character(), SKUNAME = character())
  })

  # Act
  get_user_licenses_by_role(connection = NULL)

  # Assert
  expect_no_match(statement_seen, "WHERE", fixed = TRUE)
})
