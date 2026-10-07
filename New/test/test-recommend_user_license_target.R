fixture_recommendation <- function() {
  recommend_user_license_target(
    drivers         = fixture_drivers(),
    duty_usage      = fixture_duty_usage(),
    user_licenses   = fixture_user_licenses(),
    role_identifier = "ROLE_SALES"
  )
}

decision_of <- function(decisions, user_id, sku) {
  decisions[decisions$USERID == user_id & decisions$SKUNAME == sku, ]
}

# Empfehlung fuer abgewandelte Rollenobjekte und Telemetrie.
recommend_for <- function(role_objects, usage = fixture_usage()) {
  drivers <- build_role_license_drivers(role_objects, fixture_license_requirements())
  recommend_user_license_target(
    drivers         = drivers,
    duty_usage      = analyze_user_duty_usage(drivers, fixture_user_ids(), usage),
    user_licenses   = fixture_user_licenses(),
    role_identifier = "ROLE_SALES"
  )
}

test_that("recommend_user_license_target returns decisions, a user summary and savings", {
  res <- fixture_recommendation()

  expect_named(res, c("UserDecisions", "UserSummary", "Savings"))
  expect_equal(
    names(res$UserDecisions),
    c(
      "USERID", "SKUNAME", "PRIORITY", "IS_BASE_LICENSE",
      "DUTY_COUNT", "USED_DUTIES", "NOT_OBSERVED_DUTIES", "REVIEW_DUTIES",
      "DUTY_TOUCHES", "LAST_SEEN", "DECISION",
      "LICENSED_VIA_ROLE", "LICENSED_VIA_OTHER_ROLE",
      "SAVES_LICENSE", "SAVES_IF_CONFIRMED"
    )
  )
  # 4 Benutzer x 4 SKUs der Rolle
  expect_equal(nrow(res$UserDecisions), 16L)
})

test_that("recommend_user_license_target keeps a SKU while any duty needing it is used", {
  decisions <- fixture_recommendation()$UserDecisions

  expect_equal(decision_of(decisions, "alice", "Supply Chain Management")$DECISION, "KEEP")
  # Finance steckt auch in der genutzten DutyOrders
  expect_equal(decision_of(decisions, "alice", "Finance")$DECISION, "KEEP")
  expect_equal(decision_of(decisions, "carol", "Commerce")$DECISION, "KEEP")
})

test_that("recommend_user_license_target proposes removal only when nothing was observed", {
  decisions <- fixture_recommendation()$UserDecisions

  expect_equal(decision_of(decisions, "alice", "Commerce")$DECISION, "REMOVE_CANDIDATE")
  expect_equal(decision_of(decisions, "bob", "Commerce")$DECISION, "REMOVE_CANDIDATE")
})

test_that("recommend_user_license_target asks for review when telemetry cannot decide", {
  decisions <- fixture_recommendation()$UserDecisions

  # (direkt) ist nicht messbar und braucht Finance
  expect_equal(decision_of(decisions, "bob", "Finance")$DECISION, "REVIEW")
  expect_equal(unique(decisions$DECISION[decisions$USERID == "dave"]), "REVIEW")
})

test_that("recommend_user_license_target never removes a SKU whose duty has an invisible license part", {
  decisions <- fixture_recommendation()$UserDecisions
  bob_scm   <- decision_of(decisions, "bob", "Supply Chain Management")

  # DutyOrders: SCM-Formular ungeoeffnet, aber die Finance-Aktion derselben
  # Duty ist nicht messbar. Die Duty kann in Gebrauch sein.
  expect_equal(bob_scm$DECISION, "REVIEW")
  expect_false(bob_scm$SAVES_LICENSE)
  expect_true(bob_scm$SAVES_IF_CONFIRMED)
})

test_that("recommend_user_license_target never removes a SKU that only an unmeasurable entry point requires", {
  # Arrange: Finance kommt ausschliesslich aus der Aktion in DutyOrders
  role_objects <- fixture_role_objects() |>
    dplyr::filter(PRIVILEGEIDENTIFIER != "PrivService")

  # Act
  decisions <- recommend_for(role_objects)$UserDecisions

  # Assert: carol hat Telemetrie, aber nichts in DutyOrders geoeffnet
  carol_finance <- decision_of(decisions, "carol", "Finance")
  expect_equal(carol_finance$DECISION, "REVIEW")
  expect_false(carol_finance$SAVES_LICENSE)
})

test_that("recommend_user_license_target keeps a SKU when the duty is used through an unlicensed privilege", {
  # Arrange: Custom-Privilege ohne Lizenzzeilen in derselben Duty wie Commerce
  role_objects <- fixture_role_objects() |>
    dplyr::mutate(
      IS_CUSTOM      = PRIVILEGEIDENTIFIER == "PrivWibuCustom",
      DUTYIDENTIFIER = dplyr::if_else(IS_CUSTOM, "DutyRetail", DUTYIDENTIFIER),
      DUTYNAME       = dplyr::if_else(IS_CUSTOM, "Maintain stores", DUTYNAME)
    ) |>
    dplyr::select(-IS_CUSTOM)
  custom_use <- tibble::tibble(
    user_Id = "alice", name = "WibuCustomForm",
    touches = 50, last_seen = "2026-09-05T08:00:00Z"
  )

  # Act
  decisions <- recommend_for(
    role_objects,
    usage = dplyr::bind_rows(fixture_usage(), custom_use)
  )$UserDecisions

  # Assert: alice arbeitet in der Duty, also bleibt deren Lizenz
  alice_commerce <- decision_of(decisions, "alice", "Commerce")
  expect_equal(alice_commerce$DECISION, "KEEP")
  expect_false(alice_commerce$SAVES_LICENSE)
})

test_that("recommend_user_license_target asks for review when an unlicensed privilege cannot be measured", {
  # Arrange: Custom-Privilege in DutyRetail, das nur eine Aktion berechtigt
  role_objects <- fixture_role_objects() |>
    dplyr::mutate(
      IS_CUSTOM      = PRIVILEGEIDENTIFIER == "PrivWibuCustom",
      DUTYIDENTIFIER = dplyr::if_else(IS_CUSTOM, "DutyRetail", DUTYIDENTIFIER),
      DUTYNAME       = dplyr::if_else(IS_CUSTOM, "Maintain stores", DUTYNAME),
      RESOURCETYPE   = dplyr::if_else(IS_CUSTOM, "Action menu item", RESOURCETYPE)
    ) |>
    dplyr::select(-IS_CUSTOM)

  # Act
  decisions <- recommend_for(role_objects)$UserDecisions

  # Assert
  alice_commerce <- decision_of(decisions, "alice", "Commerce")
  expect_equal(alice_commerce$DECISION, "REVIEW")
  expect_false(alice_commerce$SAVES_LICENSE)
})

test_that("recommend_user_license_target asks for review when a form and an action share a name", {
  # Arrange: Commerce haengt an RetailStoreTable, das es als Formular und als
  # Aktion gibt. Ein ungeoeffnetes Formular sagt nichts ueber die Aktion.
  same_name_action <- fixture_role_objects() |>
    dplyr::filter(PRIVILEGEIDENTIFIER == "PrivRetailMaintain") |>
    dplyr::mutate(RESOURCETYPE = "Action menu item")

  # Act
  decisions <- recommend_for(
    dplyr::bind_rows(fixture_role_objects(), same_name_action)
  )$UserDecisions

  # Assert
  alice_commerce <- decision_of(decisions, "alice", "Commerce")
  expect_equal(alice_commerce$DECISION, "REVIEW")
  expect_false(alice_commerce$SAVES_LICENSE)
})

test_that("recommend_user_license_target counts a saving only for base licenses", {
  decisions <- fixture_recommendation()$UserDecisions
  carol_tm  <- decision_of(decisions, "carol", "Team Members")

  expect_equal(carol_tm$DECISION, "REMOVE_CANDIDATE")
  expect_false(carol_tm$SAVES_LICENSE)
  expect_true(decision_of(decisions, "alice", "Commerce")$SAVES_LICENSE)
})

test_that("recommend_user_license_target sees licenses required by other roles", {
  decisions   <- fixture_recommendation()$UserDecisions
  bob_finance <- decision_of(decisions, "bob", "Finance")

  expect_true(bob_finance$LICENSED_VIA_OTHER_ROLE)
  expect_false(bob_finance$SAVES_IF_CONFIRMED)
  expect_true(decision_of(decisions, "carol", "Finance")$SAVES_IF_CONFIRMED)
})

test_that("recommend_user_license_target claims no saving when another role needs the SKU", {
  # Arrange: alice braucht Commerce zusaetzlich ueber eine andere Rolle
  other_role <- tibble::tibble(
    USERID = "alice", SKUNAME = "Commerce", ROLEIDENTIFIER = "ROLE_RETAIL",
    SKUGROUP = FIXTURE_BASE_GROUP, USERENABLED = 1L
  )
  user_licenses <- dplyr::bind_rows(fixture_user_licenses(), other_role)

  # Act
  res <- recommend_user_license_target(
    fixture_drivers(), fixture_duty_usage(), user_licenses, "ROLE_SALES"
  )

  # Assert
  alice_commerce <- decision_of(res$UserDecisions, "alice", "Commerce")
  expect_equal(alice_commerce$DECISION, "REMOVE_CANDIDATE")
  expect_false(alice_commerce$SAVES_LICENSE)
})

test_that("recommend_user_license_target claims no saving for a SKU Microsoft does not count", {
  # Arrange: laut Microsoft-Sicht braucht alice ueber die Rolle kein Commerce
  user_licenses <- fixture_user_licenses() |>
    dplyr::filter(!(USERID == "alice" & SKUNAME == "Commerce"))

  # Act
  res <- recommend_user_license_target(
    fixture_drivers(), fixture_duty_usage(), user_licenses, "ROLE_SALES"
  )

  # Assert
  alice_commerce <- decision_of(res$UserDecisions, "alice", "Commerce")
  expect_false(alice_commerce$LICENSED_VIA_ROLE)
  expect_false(alice_commerce$SAVES_LICENSE)
})

test_that("recommend_user_license_target treats unknown evidence values as review", {
  # Arrange: ein Evidenzwert, den die Funktion nicht kennt
  duty_usage <- fixture_duty_usage() |>
    dplyr::mutate(EVIDENCE = "SOMETHING_NEW")

  # Act
  res <- recommend_user_license_target(
    fixture_drivers(), duty_usage, fixture_user_licenses(), "ROLE_SALES"
  )

  # Assert
  expect_equal(unique(res$UserDecisions$DECISION), "REVIEW")
  expect_false(any(res$UserDecisions$SAVES_LICENSE))
})

test_that("recommend_user_license_target summarises licenses before and after per user", {
  users <- fixture_recommendation()$UserSummary
  user  <- function(id) users[users$USERID == id, ]

  expect_equal(users$USERID, fixture_user_ids())
  expect_equal(user("alice")$SKUS_BEFORE, "Commerce | Finance | Supply Chain Management")
  expect_equal(user("alice")$SAVED_SKUS, "Commerce")
  expect_equal(user("alice")$SKUS_AFTER, "Finance | Supply Chain Management")
  expect_equal(user("alice")$BASE_LICENSES_BEFORE, 3L)
  expect_equal(user("alice")$BASE_LICENSES_AFTER, 2L)

  expect_equal(user("bob")$SAVED_SKUS, "Commerce")
  expect_equal(user("bob")$REVIEW_SKUS, "Finance | Supply Chain Management")
  expect_equal(user("bob")$BASE_LICENSES_AFTER, 2L)

  expect_equal(user("dave")$SAVED_SKUS, "")
  expect_equal(user("dave")$BASE_LICENSES_AFTER, 3L)
  expect_equal(user("dave")$REVIEW_SKUS, "Commerce | Finance | Supply Chain Management | Team Members")
})

test_that("recommend_user_license_target totals the savings per SKU", {
  savings <- fixture_recommendation()$Savings
  sku     <- function(name) savings[savings$SKUNAME == name, ]

  expect_equal(savings$SKUNAME, c("Commerce", "Finance", "Supply Chain Management"))
  expect_equal(sku("Commerce")$USERS_BEFORE, 4L)
  expect_equal(sku("Commerce")$USERS_SAVED, 2L)
  expect_equal(sku("Commerce")$USERS_AFTER, 2L)
  expect_equal(sku("Commerce")$USERS_REVIEW, 1L)

  expect_equal(sku("Finance")$USERS_SAVED, 0L)
  expect_equal(sku("Finance")$USERS_REVIEW, 2L)

  expect_equal(sku("Supply Chain Management")$USERS_SAVED, 0L)
  expect_equal(sku("Supply Chain Management")$USERS_AFTER, 4L)
  expect_equal(sku("Supply Chain Management")$USERS_REVIEW, 3L)
})

test_that("recommend_user_license_target validates its inputs", {
  expect_error(
    recommend_user_license_target(
      tibble::tibble(x = 1), fixture_duty_usage(), fixture_user_licenses(), "ROLE_SALES"
    ),
    "Folgende Spalten fehlen in drivers"
  )
  expect_error(
    recommend_user_license_target(
      fixture_drivers(), tibble::tibble(x = 1), fixture_user_licenses(), "ROLE_SALES"
    ),
    "Folgende Spalten fehlen in duty_usage"
  )
  expect_error(
    recommend_user_license_target(
      fixture_drivers(), fixture_duty_usage(), tibble::tibble(USERID = "alice"), "ROLE_SALES"
    ),
    "Folgende Spalten fehlen in user_licenses"
  )
  expect_error(
    recommend_user_license_target(
      fixture_drivers(), fixture_duty_usage(), fixture_user_licenses(), c("A", "B")
    ),
    "genau eine Rolle"
  )
})
