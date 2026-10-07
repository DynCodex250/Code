# Synthetische Lizenzdaten fuer die Tests der Lizenz-Entscheidungsfunktionen.
#
# Rolle ROLE_SALES benoetigt drei Base-Lizenzen:
#   DutyOrders  -> Supply Chain Management ueber ein Formular (messbar) und
#                  Finance ueber eine Aktion (PrivSalesPost, nicht messbar)
#   (direkt)    -> Finance (Service Operation, nicht per Telemetrie messbar)
#   DutyRetail  -> Commerce
#   DutyInquire -> Team Members
#   DutyCustom  -> Custom-Privilege ohne Lizenzzeilen, Formular messbar

FIXTURE_BASE_GROUP <- "Base - Commerce, Finance, SCM"

fixture_role_objects <- function() {
  tibble::tribble(
    ~DUTYIDENTIFIER, ~DUTYNAME,         ~PRIVILEGEIDENTIFIER, ~PRIVILEGENAME,    ~RESOURCE_,                ~RESOURCETYPE,
    "DutyInquire",   "Inquire",         "PrivCustView",       "View customers",  "CustTableListPage",       "Display menu item",
    "DutyOrders",    "Maintain orders", "PrivSalesMaintain",  "Maintain orders", "SalesTableListPage",      "Display menu item",
    "DutyOrders",    "Maintain orders", "PrivSalesPost",      "Post invoices",   "SalesFormLetter_Invoice", "Action menu item",
    "DutyRetail",    "Maintain stores", "PrivRetailMaintain", "Maintain stores", "RetailStoreTable",        "Display menu item",
    "DutyCustom",    "WIBU custom",     "PrivWibuCustom",     "WIBU custom",     "WibuCustomForm",          "Display menu item",
    NA_character_,   NA_character_,     "PrivService",        "Call service",    "SomeServiceOperation",    "Service operation"
  ) |>
    dplyr::mutate(ROLEIDENTIFIER = "ROLE_SALES", .before = 1)
}

fixture_license_requirements <- function() {
  tibble::tribble(
    ~IDENTIFIER,          ~AOTNAME,                  ~SKUNAME,                  ~PRIORITY, ~GROUPNAME,         ~ENTITLED, ~ACCESSLEVEL,
    "PrivCustView",       "CUSTTABLELISTPAGE",       "Team Members",            20L,       NA_character_,      1L,        1L,
    "PrivCustView",       "CUSTTABLELISTPAGE",       "Operations - Activity",   30L,       NA_character_,      1L,        1L,
    "PrivCustView",       "CUSTTABLELISTPAGE",       "Commerce",                60L,       FIXTURE_BASE_GROUP, 1L,        1L,
    "PrivCustView",       "CUSTTABLELISTPAGE",       "Finance",                 70L,       FIXTURE_BASE_GROUP, 1L,        1L,
    "PrivCustView",       "CUSTTABLELISTPAGE",       "Supply Chain Management", 80L,       FIXTURE_BASE_GROUP, 1L,        1L,
    "PrivSalesMaintain",  "SALESTABLELISTPAGE",      "Team Members",            20L,       NA_character_,      0L,        2L,
    "PrivSalesMaintain",  "SALESTABLELISTPAGE",      "Supply Chain Management", 80L,       FIXTURE_BASE_GROUP, 1L,        2L,
    "PrivSalesPost",      "SALESFORMLETTER_INVOICE", "Supply Chain Management", 80L,       FIXTURE_BASE_GROUP, 1L,        2L,
    "PrivSalesPost",      "SALESFORMLETTER_INVOICE", "Finance",                 70L,       FIXTURE_BASE_GROUP, 1L,        2L,
    "PrivRetailMaintain", "RETAILSTORETABLE",        "Commerce",                60L,       FIXTURE_BASE_GROUP, 1L,        2L,
    "PrivService",        "SOMESERVICEOPERATION",    "Finance",                 70L,       FIXTURE_BASE_GROUP, 1L,        2L
  )
}

fixture_drivers <- function() {
  build_role_license_drivers(fixture_role_objects(), fixture_license_requirements())
}

fixture_user_ids <- function() {
  c("alice", "bob", "carol", "dave")
}

# Form wie D365Licensing::load_telemetry_touches(): user_Id, name, touches, last_seen.
# "zed" hat die Rolle nicht, "dave" taucht in der Telemetrie nicht auf.
fixture_usage <- function() {
  tibble::tribble(
    ~user_Id, ~name,                ~touches, ~last_seen,
    "Alice",  "SalesTableListPage", 40,       "2026-09-01T08:00:00Z",
    "Alice",  "CustTableListPage",  10,       "2026-08-15T08:00:00Z",
    "Alice",  "SomeOtherForm",      7,        "2026-09-20T08:00:00Z",
    "bob",    "CustTableListPage",  5,        "2026-07-01T08:00:00Z",
    "carol",  "RetailStoreTable",   3,        "2026-06-30T08:00:00Z",
    "zed",    "SalesTableListPage", 99,       "2026-09-30T08:00:00Z"
  )
}

fixture_duty_usage <- function() {
  analyze_user_duty_usage(fixture_drivers(), fixture_user_ids(), fixture_usage())
}

# Form wie get_user_licenses_by_role(). "bob" braucht Finance zusaetzlich ueber ROLE_OTHER.
fixture_user_licenses <- function() {
  role_rows <- tidyr::expand_grid(
    USERID  = fixture_user_ids(),
    SKUNAME = c("Commerce", "Finance", "Supply Chain Management")
  ) |>
    dplyr::mutate(ROLEIDENTIFIER = "ROLE_SALES")

  other_rows <- tibble::tibble(
    USERID         = "bob",
    SKUNAME        = "Finance",
    ROLEIDENTIFIER = "ROLE_OTHER"
  )

  dplyr::bind_rows(role_rows, other_rows) |>
    dplyr::mutate(SKUGROUP = FIXTURE_BASE_GROUP, USERENABLED = 1L)
}
