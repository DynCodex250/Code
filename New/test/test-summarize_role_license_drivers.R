test_that("summarize_duty_license_drivers returns one row per duty, most expensive first", {
  res <- summarize_duty_license_drivers(fixture_drivers())

  expect_equal(
    names(res),
    c(
      "DUTYIDENTIFIER", "DUTYNAME", "DUTY_SKU", "DUTY_PRIORITY", "IS_BASE_LICENSE",
      "SKUS_IN_DUTY", "PRIVILEGE_COUNT", "ENTRYPOINT_COUNT",
      "DRIVER_PRIVILEGES", "UNKNOWN_PRIVILEGE_COUNT",
      "UNMEASURABLE_LICENSES", "SOLE_SOURCE_OF"
    )
  )
  expect_equal(
    res$DUTYIDENTIFIER,
    c("DutyOrders", "(direkt)", "DutyRetail", "DutyInquire", "DutyCustom")
  )
  expect_equal(
    res$DUTY_SKU,
    c("Supply Chain Management", "Finance", "Commerce", "Team Members", NA)
  )
})

test_that("summarize_duty_license_drivers names the privileges that drive the duty license", {
  res    <- summarize_duty_license_drivers(fixture_drivers())
  orders <- res[res$DUTYIDENTIFIER == "DutyOrders", ]

  expect_equal(orders$SKUS_IN_DUTY, "Supply Chain Management | Finance")
  expect_equal(orders$DRIVER_PRIVILEGES, "PrivSalesMaintain")
  expect_equal(orders$PRIVILEGE_COUNT, 2L)
  expect_equal(orders$ENTRYPOINT_COUNT, 2L)
  expect_true(orders$IS_BASE_LICENSE)
})

test_that("summarize_duty_license_drivers marks duties that are the only source of a SKU", {
  res <- summarize_duty_license_drivers(fixture_drivers())
  sole <- stats::setNames(res$SOLE_SOURCE_OF, res$DUTYIDENTIFIER)

  expect_equal(sole[["DutyOrders"]], "Supply Chain Management")
  expect_equal(sole[["DutyRetail"]], "Commerce")
  # Finance kommt aus DutyOrders und aus den direkten Privileges
  expect_equal(sole[["(direkt)"]], "")
})

test_that("summarize_duty_license_drivers counts privileges without license information", {
  res    <- summarize_duty_license_drivers(fixture_drivers())
  custom <- res[res$DUTYIDENTIFIER == "DutyCustom", ]

  expect_equal(custom$UNKNOWN_PRIVILEGE_COUNT, 1L)
  expect_equal(custom$ENTRYPOINT_COUNT, 1L)
  expect_false(custom$IS_BASE_LICENSE)
  expect_equal(res$UNKNOWN_PRIVILEGE_COUNT[res$DUTYIDENTIFIER == "DutyOrders"], 0L)
})

test_that("summarize_duty_license_drivers does not count unlisted entry points as unknown", {
  # Arrange: zweites Formular im selben Privilege, ohne Zeile in der Lizenz-View
  unlisted <- fixture_role_objects() |>
    dplyr::filter(PRIVILEGEIDENTIFIER == "PrivSalesMaintain") |>
    dplyr::mutate(RESOURCE_ = "SalesTableDetails")
  drivers <- build_role_license_drivers(
    dplyr::bind_rows(fixture_role_objects(), unlisted),
    fixture_license_requirements()
  )

  # Act
  orders <- summarize_duty_license_drivers(drivers) |>
    dplyr::filter(DUTYIDENTIFIER == "DutyOrders")

  # Assert
  expect_equal(orders$ENTRYPOINT_COUNT, 3L)
  expect_equal(orders$UNKNOWN_PRIVILEGE_COUNT, 0L)
})

test_that("summarize_duty_license_drivers names the license parts telemetry cannot see", {
  res          <- summarize_duty_license_drivers(fixture_drivers())
  unmeasurable <- stats::setNames(res$UNMEASURABLE_LICENSES, res$DUTYIDENTIFIER)

  # Finance haengt in DutyOrders an einer Aktion, direkt an einer Service Operation
  expect_equal(unmeasurable[["DutyOrders"]], "Finance")
  expect_equal(unmeasurable[["(direkt)"]], "Finance")
  expect_equal(unmeasurable[["DutyRetail"]], "")
  expect_equal(unmeasurable[["DutyCustom"]], "")
})

test_that("summarize_role_license_skus lists every SKU the role requires", {
  res <- summarize_role_license_skus(fixture_drivers())

  expect_equal(
    res$SKUNAME,
    c("Supply Chain Management", "Finance", "Commerce", "Team Members")
  )
  expect_equal(res$PRIORITY, c(80, 70, 60, 20))
  expect_equal(res$DUTY_COUNT, c(1L, 2L, 1L, 1L))
  expect_equal(res$PRIVILEGE_COUNT, c(1L, 2L, 1L, 1L))
  expect_equal(res$DUTIES[res$SKUNAME == "Finance"], "(direkt), DutyOrders")
})

test_that("summarize_role_license_skus drops light licenses once a base license is required", {
  res <- summarize_role_license_skus(fixture_drivers())

  expect_equal(res$REQUIRED, c(TRUE, TRUE, TRUE, FALSE))
})

test_that("summarize_role_license_skus keeps light licenses when the role needs no base license", {
  # Arrange
  light_only <- fixture_drivers() |>
    dplyr::filter(DUTYIDENTIFIER == "DutyInquire")

  # Act
  res <- summarize_role_license_skus(light_only)

  # Assert
  expect_equal(res$SKUNAME, "Team Members")
  expect_true(res$REQUIRED)
})

test_that("compare_role_license_skus reports matches and differences to the Microsoft view", {
  # Arrange
  role_skus <- summarize_role_license_skus(fixture_drivers())
  microsoft <- tibble::tibble(SKUNAME = c("Commerce", "Supply Chain Management", "Human Resources"))

  # Act
  res <- compare_role_license_skus(role_skus, microsoft)

  # Assert
  status <- stats::setNames(res$STATUS, res$SKUNAME)
  expect_equal(status[["Commerce"]], "match")
  expect_equal(status[["Supply Chain Management"]], "match")
  expect_equal(status[["Finance"]], "only_driver_table")
  expect_equal(status[["Human Resources"]], "only_microsoft_view")
  # Team Members ist nicht REQUIRED und darf nicht als Abweichung erscheinen
  expect_false("Team Members" %in% res$SKUNAME)
})

test_that("the summary functions validate their inputs", {
  expect_error(
    summarize_duty_license_drivers(tibble::tibble(DUTYIDENTIFIER = "x")),
    "Folgende Spalten fehlen in drivers"
  )
  expect_error(
    summarize_role_license_skus(tibble::tibble(DUTYIDENTIFIER = "x")),
    "Folgende Spalten fehlen in drivers"
  )
  expect_error(
    compare_role_license_skus(tibble::tibble(SKUNAME = "x"), tibble::tibble(SKUNAME = "x")),
    "Folgende Spalten fehlen in role_skus"
  )
})
