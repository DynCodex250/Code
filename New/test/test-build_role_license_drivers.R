driver_row <- function(drivers, entrypoint) {
  drivers[!is.na(drivers$ENTRYPOINT) & drivers$ENTRYPOINT == entrypoint, ]
}

test_that("build_role_license_drivers returns one row per duty, privilege and entry point", {
  res <- fixture_drivers()

  expect_s3_class(res, "tbl_df")
  expect_equal(
    names(res),
    c(
      "ROLEIDENTIFIER", "DUTYIDENTIFIER", "DUTYNAME",
      "PRIVILEGEIDENTIFIER", "PRIVILEGENAME",
      "ENTRYPOINT", "ENTRYPOINTTYPE", "MEASURABLE", "ACCESSLEVEL",
      "LICENSE_STATUS", "MIN_SKU", "MIN_PRIORITY", "SKU_GROUP",
      "IS_BASE_LICENSE", "COVERING_SKUS",
      "PRIVILEGE_SKU", "DUTY_SKU", "DRIVES_DUTY"
    )
  )
  expect_equal(nrow(res), 6L)
})

test_that("build_role_license_drivers picks the cheapest entitled SKU per entry point", {
  res <- fixture_drivers()

  expect_equal(driver_row(res, "CUSTTABLELISTPAGE")$MIN_SKU, "Team Members")
  expect_equal(driver_row(res, "CUSTTABLELISTPAGE")$LICENSE_STATUS, "LICENSED")
  expect_equal(
    driver_row(res, "CUSTTABLELISTPAGE")$COVERING_SKUS,
    "Team Members | Operations - Activity | Commerce | Finance | Supply Chain Management"
  )
  expect_equal(driver_row(res, "SALESFORMLETTER_INVOICE")$MIN_SKU, "Finance")
  expect_equal(driver_row(res, "SALESFORMLETTER_INVOICE")$MIN_PRIORITY, 70)
})

test_that("build_role_license_drivers ignores SKUs that do not entitle the entry point", {
  res <- fixture_drivers()

  expect_equal(driver_row(res, "SALESTABLELISTPAGE")$MIN_SKU, "Supply Chain Management")
  expect_equal(driver_row(res, "SALESTABLELISTPAGE")$COVERING_SKUS, "Supply Chain Management")
})

test_that("build_role_license_drivers flags base licenses by SKU group", {
  res <- fixture_drivers()

  expect_false(driver_row(res, "CUSTTABLELISTPAGE")$IS_BASE_LICENSE)
  expect_true(driver_row(res, "RETAILSTORETABLE")$IS_BASE_LICENSE)
})

test_that("build_role_license_drivers rolls the most expensive SKU up to privilege and duty", {
  res    <- fixture_drivers()
  orders <- res[res$DUTYIDENTIFIER == "DutyOrders", ]

  expect_equal(unique(orders$DUTY_SKU), "Supply Chain Management")
  expect_equal(orders$PRIVILEGE_SKU[orders$PRIVILEGEIDENTIFIER == "PrivSalesPost"], "Finance")
  expect_true(orders$DRIVES_DUTY[orders$PRIVILEGEIDENTIFIER == "PrivSalesMaintain"])
  expect_false(orders$DRIVES_DUTY[orders$PRIVILEGEIDENTIFIER == "PrivSalesPost"])
})

test_that("build_role_license_drivers labels privileges assigned without a duty", {
  res    <- fixture_drivers()
  direct <- res[res$PRIVILEGEIDENTIFIER == "PrivService", ]

  expect_equal(direct$DUTYIDENTIFIER, "(direkt)")
  expect_equal(direct$DUTYNAME, "(direkt zugewiesen)")
  expect_equal(direct$DUTY_SKU, "Finance")
})

test_that("build_role_license_drivers keeps privileges without license rows as unknown", {
  res    <- fixture_drivers()
  custom <- res[res$PRIVILEGEIDENTIFIER == "PrivWibuCustom", ]

  expect_equal(nrow(custom), 1L)
  expect_equal(custom$LICENSE_STATUS, "UNKNOWN_PRIVILEGE")
  # der Entry Point bleibt sichtbar, damit seine Nutzung gemessen werden kann
  expect_equal(custom$ENTRYPOINT, "WibuCustomForm")
  expect_true(is.na(custom$MIN_SKU))
  expect_true(is.na(custom$DUTY_SKU))
  expect_false(custom$DRIVES_DUTY)
  expect_false(custom$IS_BASE_LICENSE)
})

test_that("build_role_license_drivers keeps privileges that have neither resources nor license rows", {
  # Arrange
  empty_privilege <- fixture_role_objects()[1, ] |>
    dplyr::mutate(
      DUTYIDENTIFIER = "DutyEmpty", DUTYNAME = "Empty",
      PRIVILEGEIDENTIFIER = "PrivEmpty", PRIVILEGENAME = "Empty",
      RESOURCE_ = NA_character_, RESOURCETYPE = NA_character_
    )
  role_objects <- dplyr::bind_rows(fixture_role_objects(), empty_privilege)

  # Act
  res <- build_role_license_drivers(role_objects, fixture_license_requirements())

  # Assert
  empty <- res[res$PRIVILEGEIDENTIFIER == "PrivEmpty", ]
  expect_equal(nrow(empty), 1L)
  expect_true(is.na(empty$ENTRYPOINT))
  expect_equal(empty$LICENSE_STATUS, "UNKNOWN_PRIVILEGE")
  expect_false(empty$MEASURABLE)
})

test_that("build_role_license_drivers keeps entry points that no SKU entitles", {
  # Arrange
  locked <- tibble::tibble(
    IDENTIFIER = "PrivSalesMaintain", AOTNAME = "LOCKEDFORM", SKUNAME = "Finance",
    PRIORITY = 70L, GROUPNAME = FIXTURE_BASE_GROUP, ENTITLED = 0L, ACCESSLEVEL = 2L
  )
  license_requirements <- dplyr::bind_rows(fixture_license_requirements(), locked)

  # Act
  res <- build_role_license_drivers(fixture_role_objects(), license_requirements)

  # Assert
  expect_equal(nrow(driver_row(res, "LOCKEDFORM")), 1L)
  expect_true(is.na(driver_row(res, "LOCKEDFORM")$MIN_SKU))
  expect_equal(driver_row(res, "LOCKEDFORM")$LICENSE_STATUS, "NOT_ENTITLED")
  expect_equal(driver_row(res, "LOCKEDFORM")$DUTY_SKU, "Supply Chain Management")
})

test_that("build_role_license_drivers keeps role entry points the license view does not list", {
  # Arrange: zweites Formular im selben Privilege, ohne Zeile in der Lizenz-View
  unlisted <- fixture_role_objects() |>
    dplyr::filter(PRIVILEGEIDENTIFIER == "PrivSalesMaintain") |>
    dplyr::mutate(RESOURCE_ = "SalesTableDetails")
  role_objects <- dplyr::bind_rows(fixture_role_objects(), unlisted)

  # Act
  res <- build_role_license_drivers(role_objects, fixture_license_requirements())

  # Assert
  details <- driver_row(res, "SalesTableDetails")
  expect_equal(nrow(details), 1L)
  expect_equal(details$LICENSE_STATUS, "NOT_LISTED")
  expect_true(is.na(details$MIN_SKU))
  expect_true(details$MEASURABLE)
  expect_equal(details$DUTY_SKU, "Supply Chain Management")
  expect_false(details$DRIVES_DUTY)
})

test_that("build_role_license_drivers matches entry point types case-insensitively", {
  res <- fixture_drivers()

  expect_equal(driver_row(res, "CUSTTABLELISTPAGE")$ENTRYPOINTTYPE, "Display menu item")
  expect_equal(driver_row(res, "SOMESERVICEOPERATION")$ENTRYPOINTTYPE, "Service operation")
})

test_that("build_role_license_drivers marks what telemetry can measure", {
  res <- fixture_drivers()

  expect_true(driver_row(res, "CUSTTABLELISTPAGE")$MEASURABLE)
  expect_false(driver_row(res, "SALESFORMLETTER_INVOICE")$MEASURABLE)
  expect_false(driver_row(res, "SOMESERVICEOPERATION")$MEASURABLE)
})

test_that("build_role_license_drivers does not call a name measurable that is also an action", {
  # Arrange: Formular und Aktion mit demselben AOT-Namen im selben Privilege
  same_name_action <- fixture_role_objects() |>
    dplyr::filter(PRIVILEGEIDENTIFIER == "PrivRetailMaintain") |>
    dplyr::mutate(RESOURCETYPE = "Action menu item")
  role_objects <- dplyr::bind_rows(fixture_role_objects(), same_name_action)

  # Act
  res <- build_role_license_drivers(role_objects, fixture_license_requirements())

  # Assert
  retail <- driver_row(res, "RETAILSTORETABLE")
  expect_equal(nrow(retail), 1L)
  expect_equal(retail$ENTRYPOINTTYPE, "Action menu item | Display menu item")
  expect_false(retail$MEASURABLE)
})

test_that("build_role_license_drivers accepts a custom measurable type pattern", {
  res <- build_role_license_drivers(
    fixture_role_objects(),
    fixture_license_requirements(),
    measurable_type_pattern = "display|service"
  )

  expect_true(driver_row(res, "SOMESERVICEOPERATION")$MEASURABLE)
  expect_false(driver_row(res, "SALESFORMLETTER_INVOICE")$MEASURABLE)
})

test_that("build_role_license_drivers sorts duties by their most expensive license", {
  res <- fixture_drivers()

  expect_equal(
    unique(res$DUTYIDENTIFIER),
    c("DutyOrders", "(direkt)", "DutyRetail", "DutyInquire", "DutyCustom")
  )
})

test_that("build_role_license_drivers accepts a custom base group pattern", {
  res <- build_role_license_drivers(
    fixture_role_objects(),
    fixture_license_requirements(),
    base_group_pattern = "^Attach"
  )

  expect_false(any(res$IS_BASE_LICENSE))
})

test_that("build_role_license_drivers validates its inputs", {
  expect_error(
    build_role_license_drivers(fixture_role_objects()[, 1:3], fixture_license_requirements()),
    "Folgende Spalten fehlen in role_objects"
  )
  expect_error(
    build_role_license_drivers(fixture_role_objects(), fixture_license_requirements()[, 1:3]),
    "Folgende Spalten fehlen in license_requirements"
  )
  expect_error(
    build_role_license_drivers("ROLE_SALES", fixture_license_requirements()),
    "role_objects muss ein Data Frame sein"
  )
})
