usage_row <- function(duty_usage, user_id, duty) {
  duty_usage[duty_usage$USERID == user_id & duty_usage$DUTYIDENTIFIER == duty, ]
}

test_that("analyze_user_duty_usage returns one row per user and duty", {
  res <- fixture_duty_usage()

  expect_s3_class(res, "tbl_df")
  expect_equal(
    names(res),
    c(
      "USERID", "DUTYIDENTIFIER", "DUTYNAME", "DUTY_SKU",
      "ENTRYPOINT_COUNT", "MEASURABLE_COUNT", "UNMEASURABLE_LICENSES",
      "TOUCHES", "DRIVER_TOUCHES", "LAST_SEEN", "EVIDENCE"
    )
  )
  expect_equal(nrow(res), 4L * 5L)
  expect_setequal(res$USERID, fixture_user_ids())
})

test_that("analyze_user_duty_usage marks duties with telemetry touches as used", {
  res    <- fixture_duty_usage()
  orders <- usage_row(res, "alice", "DutyOrders")

  expect_equal(orders$EVIDENCE, "USED")
  expect_equal(orders$TOUCHES, 40)
  expect_equal(orders$LAST_SEEN, as.Date("2026-09-01"))
  expect_equal(orders$ENTRYPOINT_COUNT, 2L)
  expect_equal(orders$MEASURABLE_COUNT, 1L)
  expect_equal(usage_row(res, "bob", "DutyInquire")$EVIDENCE, "USED")
})

test_that("analyze_user_duty_usage matches user ids case-insensitively", {
  res <- fixture_duty_usage()

  # Telemetrie liefert "Alice", die Rollenzuweisung "alice"
  expect_equal(usage_row(res, "alice", "DutyInquire")$TOUCHES, 10)
})

test_that("analyze_user_duty_usage reports a fully measurable, untouched duty as not observed", {
  res <- fixture_duty_usage()

  expect_equal(usage_row(res, "alice", "DutyRetail")$EVIDENCE, "NOT_OBSERVED")
  expect_equal(usage_row(res, "alice", "DutyRetail")$UNMEASURABLE_LICENSES, "")
  # Custom-Privilege mit messbarem, ungeoeffnetem Formular
  expect_equal(usage_row(res, "alice", "DutyCustom")$EVIDENCE, "NOT_OBSERVED")
})

test_that("analyze_user_duty_usage reports duties without measurable entry points as not measurable", {
  res    <- fixture_duty_usage()
  direct <- usage_row(res, "alice", "(direkt)")

  # nur eine Service Operation: die Telemetrie kann das nicht sehen
  expect_equal(direct$EVIDENCE, "NOT_MEASURABLE")
  expect_equal(direct$MEASURABLE_COUNT, 0L)
  expect_equal(direct$UNMEASURABLE_LICENSES, "Finance")
})

test_that("analyze_user_duty_usage does not call a duty unused when one license part is invisible", {
  res    <- fixture_duty_usage()
  orders <- usage_row(res, "bob", "DutyOrders")

  # Das SCM-Formular ist messbar und ungeoeffnet, die Finance-Aktion ist
  # nicht messbar: ueber die Duty als Ganzes laesst sich nichts sagen.
  expect_equal(orders$TOUCHES, 0)
  expect_equal(orders$MEASURABLE_COUNT, 1L)
  expect_equal(orders$UNMEASURABLE_LICENSES, "Finance")
  expect_equal(orders$EVIDENCE, "NOT_MEASURABLE")
})

test_that("analyze_user_duty_usage counts touches on entry points outside the license view", {
  # Arrange: reger Gebrauch des Custom-Formulars, das keine Lizenzzeile hat
  custom_use <- tibble::tibble(
    user_Id = "alice", name = "WibuCustomForm",
    touches = 50, last_seen = "2026-09-05T08:00:00Z"
  )
  usage <- dplyr::bind_rows(fixture_usage(), custom_use)

  # Act
  res <- analyze_user_duty_usage(fixture_drivers(), fixture_user_ids(), usage)

  # Assert
  custom <- usage_row(res, "alice", "DutyCustom")
  expect_equal(custom$EVIDENCE, "USED")
  expect_equal(custom$TOUCHES, 50)
})

test_that("analyze_user_duty_usage treats unknown licenses without measurable entry points as not measurable", {
  # Arrange: das Custom-Privilege berechtigt nur eine Aktion
  role_objects <- fixture_role_objects() |>
    dplyr::mutate(
      RESOURCETYPE = dplyr::if_else(
        PRIVILEGEIDENTIFIER == "PrivWibuCustom", "Action menu item", RESOURCETYPE
      )
    )
  drivers <- build_role_license_drivers(role_objects, fixture_license_requirements())

  # Act
  res <- analyze_user_duty_usage(drivers, fixture_user_ids(), fixture_usage())

  # Assert
  custom <- usage_row(res, "alice", "DutyCustom")
  expect_equal(custom$EVIDENCE, "NOT_MEASURABLE")
  expect_equal(custom$UNMEASURABLE_LICENSES, "(unbekannt)")
})

test_that("analyze_user_duty_usage does not trust a name shared by a form and an action", {
  # Arrange: Telemetrie kennt nur den Namen. Ob das Formular oder die Aktion
  # gemeint ist, laesst sich nicht unterscheiden.
  role_objects <- tibble::tribble(
    ~ROLEIDENTIFIER, ~DUTYIDENTIFIER, ~DUTYNAME, ~PRIVILEGEIDENTIFIER, ~PRIVILEGENAME, ~RESOURCE_, ~RESOURCETYPE,
    "ROLE_X",        "DutyX",         "X",       "PrivForm",           "Form",         "SameName", "Display menu item",
    "ROLE_X",        "DutyX",         "X",       "PrivAction",         "Action",       "SameName", "Action menu item"
  )
  license_requirements <- tibble::tribble(
    ~IDENTIFIER,  ~AOTNAME,   ~SKUNAME,  ~PRIORITY, ~GROUPNAME,         ~ENTITLED, ~ACCESSLEVEL,
    "PrivAction", "SameName", "Finance", 70L,       FIXTURE_BASE_GROUP, 1L,        2L
  )
  usage <- tibble::tibble(
    user_Id = c("ivy", "jon"), name = c("Other", "SameName"),
    touches = 5, last_seen = "2026-09-01T08:00:00Z"
  )
  drivers <- build_role_license_drivers(role_objects, license_requirements)

  # Act
  res <- analyze_user_duty_usage(drivers, "ivy", usage)

  # Assert
  expect_equal(res$EVIDENCE, "NOT_MEASURABLE")
  expect_equal(res$UNMEASURABLE_LICENSES, "Finance")
})

test_that("analyze_user_duty_usage rejects telemetry rows without a readable touch count", {
  # Arrange: die Zeile belegt einen Aufruf, die Anzahl fehlt aber
  usage <- tibble::tibble(
    user_Id = "alice", name = "RetailStoreTable",
    touches = NA_real_, last_seen = "2026-09-01T08:00:00Z"
  )

  # Act + Assert: weder als 0 Aufrufe werten noch stillschweigend verwerfen
  expect_error(
    analyze_user_duty_usage(fixture_drivers(), fixture_user_ids(), usage),
    "1 Telemetrie-Zeile\\(n\\) ohne lesbare Anzahl der Aufrufe"
  )
  expect_error(
    analyze_user_duty_usage(
      fixture_drivers(), fixture_user_ids(),
      dplyr::mutate(fixture_usage(), touches = "viele")
    ),
    "ohne lesbare Anzahl der Aufrufe"
  )
})

test_that("analyze_user_duty_usage never treats a user without telemetry as inactive", {
  res  <- fixture_duty_usage()
  dave <- res[res$USERID == "dave", ]

  expect_equal(unique(dave$EVIDENCE), "NO_USER_TELEMETRY")
  expect_true(all(dave$TOUCHES == 0))
  expect_true(all(is.na(dave$LAST_SEEN)))
})

test_that("analyze_user_duty_usage ignores users who do not hold the role", {
  res <- fixture_duty_usage()

  expect_false("zed" %in% res$USERID)
})

test_that("analyze_user_duty_usage counts touches on license-driving entry points separately", {
  # Arrange: PrivSalesPost (Finance) treibt die SCM-Lizenz von DutyOrders nicht
  extra <- tibble::tibble(
    user_Id = "alice", name = "SalesFormLetter_Invoice",
    touches = 3, last_seen = "2026-09-10T08:00:00Z"
  )
  usage <- dplyr::bind_rows(fixture_usage(), extra)

  # Act
  res <- analyze_user_duty_usage(fixture_drivers(), fixture_user_ids(), usage)

  # Assert
  orders <- usage_row(res, "alice", "DutyOrders")
  expect_equal(orders$TOUCHES, 43)
  expect_equal(orders$DRIVER_TOUCHES, 40)
  expect_equal(orders$LAST_SEEN, as.Date("2026-09-10"))
})

test_that("analyze_user_duty_usage accepts the output of get_insights_page_views_query", {
  # Arrange: Spalten timestamp (dd.MM.yyyy), name, duration, TouchesProUser, user_Id
  usage <- tibble::tibble(
    timestamp      = "01.09.2026",
    name           = "SalesTableListPage",
    duration       = 120L,
    TouchesProUser = 40L,
    user_Id        = "alice"
  )

  # Act
  res <- analyze_user_duty_usage(fixture_drivers(), "alice", usage)

  # Assert
  orders <- usage_row(res, "alice", "DutyOrders")
  expect_equal(orders$EVIDENCE, "USED")
  expect_equal(orders$TOUCHES, 40)
  expect_equal(orders$LAST_SEEN, as.Date("2026-09-01"))
})

test_that("analyze_user_duty_usage works without telemetry", {
  without_usage <- analyze_user_duty_usage(fixture_drivers(), fixture_user_ids())
  empty_usage   <- analyze_user_duty_usage(
    fixture_drivers(), fixture_user_ids(), fixture_usage()[0, ]
  )

  expect_equal(unique(without_usage$EVIDENCE), "NO_USER_TELEMETRY")
  expect_equal(unique(empty_usage$EVIDENCE), "NO_USER_TELEMETRY")
  expect_equal(nrow(without_usage), 20L)
})

test_that("analyze_user_duty_usage refuses telemetry whose names match nothing in the role", {
  # Arrange: Telemetrie mit Formularbeschriftungen statt AOT-Namen
  captions <- tibble::tibble(
    user_Id = c("alice", "bob"), name = c("All sales orders", "All customers"),
    touches = c(40, 5), last_seen = "2026-09-01T08:00:00Z"
  )

  # Act + Assert: Warnung statt "alle Benutzer nutzen nichts"
  expect_warning(
    res <- analyze_user_duty_usage(fixture_drivers(), fixture_user_ids(), captions),
    "Kein messbarer Entry Point der Rolle kommt in der Telemetrie vor"
  )
  expect_equal(unique(res$EVIDENCE), "NO_USER_TELEMETRY")
})

test_that("analyze_user_duty_usage follows the measurable types of the driver table", {
  # Arrange
  drivers <- build_role_license_drivers(
    fixture_role_objects(), fixture_license_requirements(),
    measurable_type_pattern = "display|service"
  )

  # Act
  res <- analyze_user_duty_usage(drivers, fixture_user_ids(), fixture_usage())

  # Assert
  expect_equal(usage_row(res, "alice", "(direkt)")$EVIDENCE, "NOT_OBSERVED")
})

test_that("analyze_user_duty_usage removes duplicate user ids", {
  res <- analyze_user_duty_usage(
    fixture_drivers(), c("alice", "ALICE", "alice"), fixture_usage()
  )

  expect_equal(nrow(res), 5L)
})

test_that("analyze_user_duty_usage validates its inputs", {
  expect_error(
    analyze_user_duty_usage(tibble::tibble(x = 1), "alice", fixture_usage()),
    "Folgende Spalten fehlen in drivers"
  )
  expect_error(
    analyze_user_duty_usage(fixture_drivers(), 42, fixture_usage()),
    "user_ids muss ein Character-Vektor sein"
  )
  expect_error(
    analyze_user_duty_usage(fixture_drivers(), "alice", tibble::tibble(name = "x", touches = 1)),
    "usage braucht eine der Spalten"
  )
  expect_error(
    analyze_user_duty_usage(fixture_drivers(), "alice", "not a data frame"),
    "usage muss ein Data Frame oder NULL sein"
  )
})
