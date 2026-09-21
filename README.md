---
title: "MD3-1004 Wiederbeschaffungszeit – Playbook (DMF-Import)"
author: "Cedric Kameni"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_depth: 3
---

# Zielsetzung

Artikel, die in **0800/0900** als Direktlieferungsartikel geführt werden
(`PROCUREMENTWAREHOUSEID = 'STRECKE'`), sollen in **0360** mit der
entsprechenden Lieferdatumskontrolle aktualisiert werden.

Gesetzt wird in `InventProductSpecificOrderSettingsV3Entity`:

| Feld | Wert |
|------|------|
| `SALESATPTIMEFENCEDAYS` | = `PROCUREMENTLEADTIMEDAYS` aus DE (VfZ-Planungszeitraum) |
| `ARESALESDEFAULTORDERSETTINGSOVERRIDDEN` | `"Yes"` |
| `SALESORDERPROMISINGMETHOD` | `"ATPPlusIssueMargin"` (= VfZ + Sicherheitszuschlag) |

> **Hinweis – Enum-Konvertierung:** `SALESORDERPROMISINGMETHOD` wird aus der
> Entity-View als Integer gelesen (0 = Keine, 1 = Verkaufslieferzeit, 2 = VfZ,
> 3 = VfZ + Sicherheitszuschlag, 4 = CTP, 5 = Batch-CTP). Für den
> Datenverwaltungs-Import ist der String `"ATPPlusIssueMargin"` erforderlich.
> Die Konvertierung erfolgt in R.

> **Hinweis – Datenquelle:** Diese Playbook-Variante lädt die Daten **nicht** aus
> der Datenbank, sondern aus **DMF-Exporten der Data Entities** (`.xlsx`) im
> Ordner `path_in`. Je Entity liegt eine Datei pro Mandant vor; der Mandant
> (`DATAAREAID`) steckt im Zahlen-Suffix des Dateinamens (z. B. `..._0360.xlsx`).
> `PARTITION` ist in den Exporten nicht enthalten und wird mit einem festen
> Dummy-Wert (`DUMMY_PARTITION`) belegt. Ab Schritt 2 ist der Code identisch zur
> DB-Variante.

> **Rollback:** Schritt 6 erzeugt zusätzlich eine Rollback-Importdatei, mit
> der die in Schritt 3/4 vorgenommenen Änderungen in 0360 wieder auf ihren
> Ursprungszustand zurückgesetzt werden können.

---

# Einrichtung

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(dplyr)
library(readxl)
library(writexl)
```

```{r paths}
devtools::load_all("C:/Users/CKAM/Documents/RProjects/D365Security")

# Eingabe: Ordner mit den DMF-Exporten der Data Entities (.xlsx)
path_in  <- "C:/Users/CKAM/Documents/RProjects/Data/MD3-1004 Wiederbeschaffungszeit/PRJ/"

# Ausgabe: Import- und Testdateien
path_out <- "C:/Users/CKAM/Documents/RProjects/Data/MD3-1004 Wiederbeschaffungszeit/"
dir.create(path_out, showWarnings = FALSE, recursive = TRUE)

# PARTITION ist in den DMF-Exporten nicht enthalten -> fester Dummy-Wert
DUMMY_PARTITION <- "1111111"
```

---

# Schritt 1 — Daten laden (DMF-Exporte)

Statt SQL-Queries werden die DMF-Exporte der Data Entities eingelesen.
Erwartet werden je Entity drei Dateien (Mandanten 0360, 0800, 0900):

* `... - Released products V2_<DATAAREAID>.xlsx`
* `... - Released product variants V2_<DATAAREAID>.xlsx`
* `... - Product default order settings V3_<DATAAREAID>.xlsx`

`df_all_prod` und `df_all_pdos` werden so aufgebaut, dass sie exakt dem Ergebnis
der bisherigen SQL-Queries entsprechen (Schlüssel `ID` / `KEY` / `KEY2`,
`VARIANT`/`MASTER`-Union, Normalisierung `LOWER` + `NULL -> ""`).

```{r dmf-reader}
# Liest alle DMF-Exporte eines Entity-Typs aus path_in und haengt
#   DATAAREAID = Zahlen-Suffix nach dem letzten "_" im Dateinamen
#   PARTITION  = fester Dummy-Wert (DUMMY_PARTITION)
# an. Alle Spalten werden als Text gelesen (wie SQL-String-Handling).
read_dmf_entity <- function(name_contains) {
  files <- list.files(path_in, pattern = "\\.xlsx$", full.names = TRUE)
  files <- files[grepl(name_contains, basename(files), fixed = TRUE)]
  if (length(files) == 0L)
    stop("Keine Datei mit '", name_contains, "' in ", path_in)

  dplyr::bind_rows(lapply(files, function(f) {
    da <- sub(".*_(\\d{3,4})\\.xlsx$", "\\1", basename(f))
    readxl::read_excel(f, col_types = "text") |>
      dplyr::mutate(DATAAREAID = da, PARTITION = DUMMY_PARTITION)
  }))
}

# Normalisierung wie SQL: LOWER(ISNULL(x, ''))
norm <- function(x) tolower(dplyr::coalesce(as.character(x), ""))
```

## Produkte (alle Mandanten)

```{r load-products}
prod_master_raw  <- read_dmf_entity("Released products V2")
prod_variant_raw <- read_dmf_entity("Released product variants V2")

# VARIANT-Ebene: Varianten + Dimensionsgruppen/Nummer vom Master (LEFT JOIN)
df_prod_variant <- prod_variant_raw |>
  dplyr::left_join(
    prod_master_raw |>
      dplyr::select(
        ITEMNUMBER, DATAAREAID, PARTITION,
        PRODUCTNUMBER,
        PRODUCTDIMENSIONGROUPNAME,
        STORAGEDIMENSIONGROUPNAME,
        TRACKINGDIMENSIONGROUPNAME,
        MASTER_LIFECYCLE = PRODUCTLIFECYCLESTATEID
      ),
    by = c("ITEMNUMBER", "DATAAREAID", "PARTITION")
  ) |>
  dplyr::transmute(
    ITEMNUMBER                = tolower(ITEMNUMBER),
    PRODUCTNUMBER,
    PRODUCTMASTERNUMBER,
    PRODUCTLIFECYCLESTATEID   = dplyr::coalesce(PRODUCTLIFECYCLESTATEID, MASTER_LIFECYCLE),
    PRODUCTDIMENSIONGROUPNAME,
    STORAGEDIMENSIONGROUPNAME,
    TRACKINGDIMENSIONGROUPNAME,
    PRODUCTCONFIGURATIONID    = norm(PRODUCTCONFIGURATIONID),
    PRODUCTSIZEID             = norm(PRODUCTSIZEID),
    PRODUCTCOLORID            = norm(PRODUCTCOLORID),
    PRODUCTSTYLEID            = norm(PRODUCTSTYLEID),
    PRODUCTVERSIONID          = norm(PRODUCTVERSIONID),
    WIBURETAILVARIANTID,
    LEVEL                     = "VARIANT",
    ID = paste(ITEMNUMBER, PRODUCTCONFIGURATIONID, PRODUCTCOLORID,
               PRODUCTSIZEID, PRODUCTSTYLEID, PRODUCTVERSIONID,
               DATAAREAID, PARTITION, sep = "-"),
    DATAAREAID,
    PARTITION
  )

# MASTER-Ebene: Master-Datensaetze ohne Variantendimensionen
df_prod_master <- prod_master_raw |>
  dplyr::transmute(
    ITEMNUMBER                = tolower(ITEMNUMBER),
    PRODUCTNUMBER,
    PRODUCTMASTERNUMBER       = NA_character_,
    PRODUCTLIFECYCLESTATEID,
    PRODUCTDIMENSIONGROUPNAME,
    STORAGEDIMENSIONGROUPNAME,
    TRACKINGDIMENSIONGROUPNAME,
    PRODUCTCONFIGURATIONID    = "",
    PRODUCTSIZEID             = "",
    PRODUCTCOLORID            = "",
    PRODUCTSTYLEID            = "",
    PRODUCTVERSIONID          = "",
    WIBURETAILVARIANTID       = NA_character_,
    LEVEL                     = "MASTER",
    # entspricht SQL: LOWER(ITEMNUMBER) + '-' + '-' + '-' + '-' + '-' + DATAAREAID + '-' + PARTITION
    ID = paste0(tolower(ITEMNUMBER), "-----", DATAAREAID, "-", PARTITION),
    DATAAREAID,
    PARTITION
  )

df_all_prod <- dplyr::bind_rows(df_prod_variant, df_prod_master)

cat("Produktdatensätze gesamt:", nrow(df_all_prod), "\n")
```

## PDOS (alle Mandanten)

```{r load-pdos}
pdos_raw <- read_dmf_entity("Product default order settings V3")

# Schritt 1: Felder selektieren und normalisieren (CTE "src")
src <- pdos_raw |>
  dplyr::transmute(
    ITEMNUMBER              = tolower(ITEMNUMBER),
    OPERATIONALSITEID       = dplyr::coalesce(OPERATIONALSITEID, ""),
    PRODUCTCONFIGURATIONID  = norm(PRODUCTCONFIGURATIONID),
    PRODUCTSIZEID           = norm(PRODUCTSIZEID),
    PRODUCTCOLORID          = norm(PRODUCTCOLORID),
    PRODUCTSTYLEID          = norm(PRODUCTSTYLEID),
    PRODUCTVERSIONID        = norm(PRODUCTVERSIONID),
    ORDERSETTINGSRANK       = as.character(ORDERSETTINGSRANK),
    ISSALESLEADTIMEOVERRIDDEN,
    PROCUREMENTLEADTIMEDAYS = as.numeric(PROCUREMENTLEADTIMEDAYS),
    SALESLEADTIMEDAYS       = as.numeric(SALESLEADTIMEDAYS),
    ARESALESDEFAULTORDERSETTINGSOVERRIDDEN,
    SALESORDERPROMISINGMETHOD,
    SALESATPTIMEFENCEDAYS   = as.numeric(SALESATPTIMEFENCEDAYS),
    PROCUREMENTWAREHOUSEID,
    DATAAREAID,
    PARTITION
  )

# Schritt 2: Schluessel berechnen (CTE "with_keys") + Duplikat-Flags, dann finale Auswahl
df_all_pdos <- src |>
  dplyr::mutate(
    ID = paste(ITEMNUMBER, PRODUCTCONFIGURATIONID, PRODUCTCOLORID,
               PRODUCTSIZEID, PRODUCTSTYLEID, PRODUCTVERSIONID,
               DATAAREAID, PARTITION, sep = "-"),
    KEY = paste0(ITEMNUMBER, OPERATIONALSITEID, PRODUCTCONFIGURATIONID,
                 PRODUCTCOLORID, PRODUCTSIZEID, PRODUCTSTYLEID,
                 PRODUCTVERSIONID, DATAAREAID, PARTITION),
    KEY2 = paste0(ITEMNUMBER, OPERATIONALSITEID, PRODUCTCONFIGURATIONID,
                  PRODUCTCOLORID, PRODUCTSIZEID, PRODUCTSTYLEID,
                  PRODUCTVERSIONID, ORDERSETTINGSRANK)
  ) |>
  dplyr::group_by(KEY)  |>
  dplyr::mutate(is_duplicate  = as.integer(dplyr::row_number() > 1)) |>
  dplyr::ungroup() |>
  dplyr::group_by(KEY2) |>
  dplyr::mutate(is_duplicate2 = as.integer(dplyr::row_number() > 1)) |>
  dplyr::ungroup() |>
  dplyr::select(
    ITEMNUMBER, OPERATIONALSITEID,
    PRODUCTCONFIGURATIONID, PRODUCTSIZEID, PRODUCTCOLORID,
    PRODUCTSTYLEID, PRODUCTVERSIONID, ORDERSETTINGSRANK,
    ISSALESLEADTIMEOVERRIDDEN, PROCUREMENTLEADTIMEDAYS,
    SALESLEADTIMEDAYS, ARESALESDEFAULTORDERSETTINGSOVERRIDDEN,
    SALESORDERPROMISINGMETHOD, SALESATPTIMEFENCEDAYS,
    PROCUREMENTWAREHOUSEID, DATAAREAID, PARTITION,
    ID, KEY, KEY2, is_duplicate, is_duplicate2
  )

cat("PDOS-Datensätze gesamt:", nrow(df_all_pdos), "\n")
```

---

# Schritt 2 — Zieldaten (0360) aufbereiten

## Mandantenfilter

```{r filter-0360}
final_data <- df_all_prod |> dplyr::filter(DATAAREAID == "0360")
pdos360    <- df_all_pdos  |> dplyr::filter(DATAAREAID == "0360")

cat("Produkte 0360:", nrow(final_data), "\n")
cat("PDOS 0360:    ", nrow(pdos360),    "\n")
```

## Technische Plausibilisierung

```{r plausibility}
# Master- vs. Variantenzeilen
dim(final_data |> dplyr::filter(is.na(PRODUCTMASTERNUMBER)))   # Master
dim(final_data |> dplyr::filter(!is.na(PRODUCTMASTERNUMBER)))  # Varianten

# Ausschlussmenge
AntiProd <- final_data |>
  dplyr::filter(
    is.na(TRACKINGDIMENSIONGROUPNAME)
    | is.na(STORAGEDIMENSIONGROUPNAME)
    | PRODUCTLIFECYCLESTATEID %in% c("Gesperrt", "Final gesperrt")
  )

cat("Ausschlussmenge:", nrow(AntiProd), "\n")
```

## Anreicherung

```{r enrichment}
pdos360_enriched <- pdos360 |>
  dplyr::left_join(
    final_data |>
      dplyr::distinct(
        ID,
        ITEMNUMBER,
        PRODUCTCONFIGURATIONID,
        PRODUCTCOLORID,
        PRODUCTSIZEID,
        PRODUCTSTYLEID,
        PRODUCTVERSIONID,
        TRACKINGDIMENSIONGROUPNAME,
        STORAGEDIMENSIONGROUPNAME,
        PRODUCTDIMENSIONGROUPNAME,
        PRODUCTLIFECYCLESTATEID,
        LEVEL
      ),
    by = c(
      "ITEMNUMBER",
      "PRODUCTCONFIGURATIONID",
      "PRODUCTCOLORID",
      "PRODUCTSIZEID",
      "PRODUCTSTYLEID",
      "PRODUCTVERSIONID"
    )
  )

# Datensätze ohne Lifecycle State nach Anreicherung
cat("Ohne Lifecycle State:", pdos360_enriched |>
  dplyr::filter(is.na(PRODUCTLIFECYCLESTATEID)) |> nrow(), "\n")
```

## Bearbeitbarkeitsprüfung

```{r editable}
pdos360_not_editable <- pdos360_enriched |>
  dplyr::filter(
    is.na(TRACKINGDIMENSIONGROUPNAME)
    | is.na(STORAGEDIMENSIONGROUPNAME)
    | PRODUCTLIFECYCLESTATEID %in% c("Gesperrt", "Final gesperrt")
  )

pdos360_editable <- pdos360_enriched |>
  dplyr::filter(
    !PRODUCTLIFECYCLESTATEID %in% c("Gesperrt", "Final gesperrt"),
    !is.na(TRACKINGDIMENSIONGROUPNAME),
    !is.na(STORAGEDIMENSIONGROUPNAME)
  ) |>
  dplyr::select(
    -ARESALESDEFAULTORDERSETTINGSOVERRIDDEN,
    -ISSALESLEADTIMEOVERRIDDEN,
    -SALESORDERPROMISINGMETHOD,
    -TRACKINGDIMENSIONGROUPNAME,
    -STORAGEDIMENSIONGROUPNAME,
    -PRODUCTDIMENSIONGROUPNAME,
    -LEVEL,
    -PROCUREMENTLEADTIMEDAYS,
    -SALESATPTIMEFENCEDAYS,
    -SALESLEADTIMEDAYS,
    -PROCUREMENTWAREHOUSEID,
    -PARTITION,
    -KEY,
    -dplyr::starts_with("is_"),
    -dplyr::ends_with(".x"),
    -dplyr::ends_with(".y")
  )

cat("Nicht bearbeitbar:", nrow(pdos360_not_editable), "\n")
cat("Bearbeitbar:      ", nrow(pdos360_editable),     "\n")
```

---

# Schritt 3 — Quelldaten DE (0800 / 0900)

## Direktlieferungsartikel

```{r source-data}
pdos_other <- df_all_pdos |>
  dplyr::filter(
    PROCUREMENTWAREHOUSEID == "STRECKE",
    DATAAREAID %in% c("0800", "0900")
  )

cat("Direktlieferungsartikel DE:", nrow(pdos_other), "\n")
```

## Konfliktprüfung (0800 vs. 0900)

```{r conflicts}
pdos_conflicts <- pdos_other |>
  dplyr::group_by(KEY2) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::summarise(
    Quellen      = paste(sort(unique(DATAAREAID)), collapse = " / "),
    Lieferzeiten = paste(PROCUREMENTLEADTIMEDAYS, collapse = " / "),
    .groups      = "drop"
  )

if (nrow(pdos_conflicts) > 0) {
  message("WARNUNG: ", nrow(pdos_conflicts), " Konflikte zwischen 0800 und 0900!")
} else {
  message("Keine Konflikte.")
}
```

## Matching Quelle → Ziel

```{r matching}
dup <- pdos_other |> dplyr::filter(is_duplicate == 1)

if (nrow(dup) > 0) {
  message("WARNUNG: ", nrow(dup), " Duplikate in Quelldaten!")
}

# Quelldaten: relevante Felder für den Import-Join
pdos_other_p <- pdos_other |>
  dplyr::select(
    KEY2,
    PROCUREMENTWAREHOUSEID,
    PROCUREMENTLEADTIMEDAYS,
    DATAAREAID
  )


# Join über KEY2 (mandantenübergreifender Schlüssel)
target_joined <- pdos360_editable |>
  dplyr::left_join(pdos_other_p, by = "KEY2") |>
  dplyr::select(-KEY2)

only_0360  <- target_joined |> dplyr::filter(is.na(PROCUREMENTLEADTIMEDAYS))
target_prep <- target_joined |> dplyr::filter(!is.na(PROCUREMENTLEADTIMEDAYS))

cat("Nur in 0360 (kein DE-Match):", nrow(only_0360),   "\n")
cat("Erfolgreich gematcht:       ", nrow(target_prep), "\n")
```

## Importdatei

```{r import}
# SALESORDERPROMISINGMETHOD: SQL-Integer 3 → Import-String "ATPPlusIssueMargin"
# Zieldaten: PROCUREMENTLEADTIMEDAYS / SALESATPTIMEFENCEDAYS werden aus DE übernommen)
target_prep <- target_prep |>
  dplyr::mutate(
   ARESALESDEFAULTORDERSETTINGSOVERRIDDEN = "Yes",
    SALESORDERPROMISINGMETHOD             = "ATPPlusIssueMargin"
  ) |>
  rename(
     SALESATPTIMEFENCEDAYS = PROCUREMENTLEADTIMEDAYS
  )

prod_ord_0360_import <- target_prep|>
  dplyr::select(
    -PRODUCTLIFECYCLESTATEID,
    -PROCUREMENTWAREHOUSEID,
    -dplyr::ends_with(".x"),
    -dplyr::ends_with(".y")
  )

cat("Importdatei 1:", nrow(prod_ord_0360_import), "Datensätze\n")
```

---

# Schritt 4 — Excel-Export

```{r export}
writexl::write_xlsx(
  list(
    pdos360_not_editable = pdos360_not_editable,
    pdos360_editable     = pdos360_editable,
    all                  = target_joined,
    only_0360            = only_0360,
    Konflikte_DE         = pdos_conflicts,
    prod_ord_0360_import          = prod_ord_0360_import
  ),
  paste0(
    path_out,
    "MD3-1004 - 0360 Product default order settings ",
    format(Sys.Date(), "%Y-%m-%d"),
    ".xlsx"
  )
)
```

---

# Schritt 5 — Testdateien

```{r test-files}
if (nrow(prod_ord_0360_import) >= 10) {
  set.seed(123)
  idx <- sample(nrow(prod_ord_0360_import), min(50, nrow(prod_ord_0360_import)))
  n_files <- min(5, floor(length(idx) / 10))

  for (i in seq_len(n_files)) {
    rows <- idx[((i - 1) * 10 + 1):(i * 10)]
    writexl::write_xlsx(
      list(Import1_Test = prod_ord_0360_import[rows, ]),
      paste0(path_out, "p3_import_test_", i, ".xlsx")
    )
  }
  cat(n_files, "Testdateien erstellt.\n")
} else {
  cat("Zu wenige Datensätze (", nrow(prod_ord_0360_import), ").\n")
}
```

---

# Schritt 6 — Rollback-Importdatei (Rücksetzung 0360)

## Zielsetzung

Für den Fall, dass die in Schritt 3/4 vorgenommenen Änderungen in **0360**
zurückgenommen werden müssen, wird zusätzlich eine Rollback-Importdatei
erzeugt. Sie enthält für exakt dieselben Datensätze wie
`prod_ord_0360_import` die **ursprünglichen** (unveränderten) Werte der drei
gesetzten Felder:

| Feld | Rollback-Wert |
|------|----------------|
| `SALESATPTIMEFENCEDAYS` | Originalwert aus dem 0360-DMF-Export (vor der Änderung) |
| `ARESALESDEFAULTORDERSETTINGSOVERRIDDEN` | Originalwert aus dem 0360-DMF-Export |
| `SALESORDERPROMISINGMETHOD` | Originalwert aus dem 0360-DMF-Export (Integer → Import-String) |

Die Zuordnung erfolgt über `KEY2` (Artikel/Varianten-/Standortschlüssel +
`ORDERSETTINGSRANK`), denselben Schlüssel, über den in Schritt 3 auch die
DE-Quelldaten gematcht wurden.

> **WICHTIG — Zeitpunkt:** Die Rollback-Datei muss aus **demselben Lauf**
> erzeugt werden wie die Forward-Importdatei, da sie auf den zu diesem
> Zeitpunkt eingelesenen `pdos360`-Rohdaten (Zustand **vor** der Änderung)
> basiert. Wird dieses Rmd später mit neu gezogenen DMF-Exporten erneut
> ausgeführt, spiegeln diese ggf. bereits den **geänderten** Zustand wider
> und eignen sich dann nicht mehr als Rollback-Quelle. Die erzeugte
> Rollback-Datei ist daher zusammen mit der Forward-Importdatei zu
> archivieren, bevor der Forward-Import produktiv ausgeführt wird.

> **WICHTIG — Enum-Konvertierung `SALESORDERPROMISINGMETHOD`:** Aus dem
> produktiven Forward-Import ist nur die Zuordnung `3 -> "ATPPlusIssueMargin"`
> bestätigt (siehe Hinweis oben in "Einrichtung"). Die übrigen Werte
> (`0`, `1`, `2`, `4`, `5`) im Mapping unten sind anhand der dort
> dokumentierten AX-Enum-Reihenfolge abgeleitet, aber **nicht produktiv
> verifiziert**. Vor einem produktiven Rollback-Import erst an einem
> Testsystem gegenprüfen, insbesondere falls `rollback_missing` bzw. die
> gemappten Werte unplausibel erscheinen.

## Originalwerte laden (Zustand vor Änderung)

```{r rollback-original}
if (any(pdos360$is_duplicate2 == 1)) {
  message(
    "WARNUNG: ", sum(pdos360$is_duplicate2 == 1),
    " Duplikate (KEY2) in pdos360 - Rollback verwendet je Duplikat den ersten Datensatz."
  )
}

pdos360_original <- pdos360 |>
  dplyr::distinct(KEY2, .keep_all = TRUE) |>
  dplyr::select(
    KEY2,
    ARESALESDEFAULTORDERSETTINGSOVERRIDDEN_ORIG = ARESALESDEFAULTORDERSETTINGSOVERRIDDEN,
    SALESORDERPROMISINGMETHOD_ORIG              = SALESORDERPROMISINGMETHOD,
    SALESATPTIMEFENCEDAYS_ORIG                  = SALESATPTIMEFENCEDAYS
  )
```

## Enum-Mapping SALESORDERPROMISINGMETHOD

```{r rollback-enum-map}
# Integer (Entity-View / DMF-Export) -> Import-String.
# NUR Wert 3 ist aus dem produktiven Forward-Import bestaetigt - siehe
# Hinweis oben. Rest vor produktivem Rollback-Import verifizieren!
map_sales_order_promising_method <- function(x) {
  dplyr::case_when(
    x == "0" ~ "None",
    x == "1" ~ "SalesLeadTime",
    x == "2" ~ "ATP",
    x == "3" ~ "ATPPlusIssueMargin",
    x == "4" ~ "CTP",
    x == "5" ~ "CTPPlusIssueMargin",
    TRUE     ~ NA_character_
  )
}
```

## Rollback-Importdatei erstellen

```{r rollback-build}
prod_ord_0360_rollback <- prod_ord_0360_import |>
  dplyr::select(
    -ARESALESDEFAULTORDERSETTINGSOVERRIDDEN,
    -SALESORDERPROMISINGMETHOD,
    -SALESATPTIMEFENCEDAYS
  ) |>
  dplyr::left_join(pdos360_original, by = "KEY2") |>
  dplyr::mutate(
    ARESALESDEFAULTORDERSETTINGSOVERRIDDEN = ARESALESDEFAULTORDERSETTINGSOVERRIDDEN_ORIG,
    SALESATPTIMEFENCEDAYS                  = SALESATPTIMEFENCEDAYS_ORIG,
    SALESORDERPROMISINGMETHOD              = map_sales_order_promising_method(SALESORDERPROMISINGMETHOD_ORIG)
  ) |>
  dplyr::select(-dplyr::ends_with("_ORIG"))

# Datensaetze, fuer die kein Originalwert gefunden wurde (z. B. Key-Mismatch)
# oder deren Enum-Wert nicht im Mapping oben enthalten ist - vor dem
# Rollback-Import manuell pruefen, nicht blind mit importieren.
rollback_missing <- prod_ord_0360_rollback |>
  dplyr::filter(
    is.na(SALESATPTIMEFENCEDAYS)
    | is.na(SALESORDERPROMISINGMETHOD)
    | is.na(ARESALESDEFAULTORDERSETTINGSOVERRIDDEN)
  )

prod_ord_0360_rollback <- prod_ord_0360_rollback |>
  dplyr::anti_join(rollback_missing, by = "KEY2")

cat("Rollback-Importdatei:              ", nrow(prod_ord_0360_rollback), "Datensätze\n")
cat("Ohne Originalwert (manuell prüfen):", nrow(rollback_missing),       "\n")
```

## Excel-Export

```{r rollback-export}
writexl::write_xlsx(
  list(
    prod_ord_0360_rollback = prod_ord_0360_rollback,
    rollback_missing       = rollback_missing
  ),
  paste0(
    path_out,
    "MD3-1004 - 0360 Rollback Product default order settings ",
    format(Sys.Date(), "%Y-%m-%d"),
    ".xlsx"
  )
)
```
