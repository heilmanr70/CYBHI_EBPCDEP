# D1, D3 and D4: data window, missing provider_id, unassigned funding rounds
#
# Runs on the cleaned outputs of report_data_cleaning.Rmd (all_data) and the
# satisfaction cleaning script, so the DEMO filter, ID fixes and funding-round
# fixes made there are already applied.
#   D1  applies the final data window and counts what it excludes
#   D3  counts rows without a provider_id and how many could be recovered
#   D4  lists providers whose funding round is still unassigned (CSV for Rikke)
# The only file written is the D4 list: provider IDs, names and counts, no client data.

library(tidyverse)   # dplyr, readr, stringr, lubridate, purrr
library(janitor)     # excel_numeric_to_date(), used by parse_excel_date()

# ---- Configuration -------------------------------------------------------------
out_dir           <- "/Users/cristin/CWS Dropbox/Cristin Young/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data"
all_data_path     <- file.path(out_dir, "demographic_discharge_clean.csv")
satisfaction_path <- file.path(out_dir, "combined_satisfaction_clean.csv")

start_date         <- as.Date("2024-01-01")  # D1: waits on the grant period
end_date           <- as.Date("2027-03-31")  # D1: final data window, applied to intervention start
keep_missing_start <- TRUE                   # D0: enrollment doesn't require a start date

date_cols     <- c("intervention_start", "intervention_end", "pre_completion_date", "post_completion_date")
pair_keys     <- c("client_system_id", "intervention_name", "intervention_start", "pre_measure_s")
provider_keys <- c("provider_system_id", "provider", "location")  # D3: fields tried, in order, to recover provider_id
test_pattern  <- regex("demo|test|train|sandbox", ignore_case = TRUE)

# ---- Helpers -------------------------------------------------------------------
# Converts an eInsight/Excel date column to Date. Handles columns that arrive as
# Date/POSIXct, as Excel serial numbers, or as a character mix of serials and
# text dates (m/d/y or y-m-d). Years below 100 (e.g., 0025) are shifted by 2000.
# Warns with a count of non-blank values that could not be parsed.
# (Copied verbatim from report_data_cleaning.Rmd.)
parse_excel_date <- function(x, label) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x))         return(janitor::excel_numeric_to_date(floor(x)))

  x   <- str_squish(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))

  is_serial <- !is.na(x) & str_detect(x, "^[0-9]+(\\.[0-9]+)?$")
  if (any(is_serial)) {
    out[is_serial] <- janitor::excel_numeric_to_date(floor(as.numeric(x[is_serial])))
  }
  is_text <- !is.na(x) & !is_serial
  if (any(is_text)) {
    out[is_text] <- as.Date(parse_date_time(x[is_text], orders = c("mdy", "Ymd"), quiet = TRUE))
  }

  # Fix 2-digit years parsed as e.g. year 25 instead of 2025
  fix <- !is.na(out) & year(out) < 100
  out[fix] <- out[fix] + years(2000)

  n_bad <- sum(!is.na(x) & is.na(out))
  if (n_bad > 0) {
    warning(label, ": ", n_bad, " non-blank value(s) could not be parsed as dates and were set to NA.",
            call. = FALSE)
  }
  out
}

# Reads a cleaned output with every column as text, so IDs keep their exact form
read_clean <- function(path) read_csv(path, col_types = cols(.default = col_character()), na = c("", "NA"))

# provider_id lookup from one field, keeping only values that point to a single provider_id
unique_provider_map <- function(df, key) {
  df |>
    filter(!is.na(.data[[key]]), !is.na(provider_id)) |>
    distinct(across(all_of(c(key, "provider_id")))) |>
    filter(n() == 1, .by = all_of(key))
}

# provider_id, filled where blank from the first provider field that maps to one provider_id
recover_provider_id <- function(df, reference) {
  keys       <- intersect(provider_keys, names(df))
  candidates <- map(keys, \(k) left_join(select(df, all_of(k)), unique_provider_map(reference, k), by = k)$provider_id)
  coalesce(df$provider_id, !!!candidates)
}

# D3 summary for one file: rows without a provider_id, the provider fields they
# still have, how many could be recovered, and how many look like test entries
summarise_missing_provider <- function(df, label, reference) {
  df |>
    filter(is.na(provider_id)) |>
    summarise(
      rows_missing_provider = n(),
      clients               = n_distinct(client_system_id),
      across(any_of(provider_keys), \(x) sum(!is.na(x)), .names = "with_{.col}"),
      recoverable           = sum(!is.na(recover_provider_id(pick(everything()), reference))),
      look_like_test        = sum(if_any(any_of(c(provider_keys, "client_system_id")),
                                         \(x) str_detect(x, test_pattern) %in% TRUE))
    ) |>
    mutate(file = label, rows = nrow(df), .before = 1)
}

# ---- Read ----------------------------------------------------------------------
all_data <- read_clean(all_data_path) |>
  mutate(across(any_of(date_cols), \(x) parse_excel_date(x, cur_column())))
satisfaction <- read_clean(satisfaction_path)

# ---- D1: data window -----------------------------------------------------------
# start_before and start_after are NA when there is no start date
all_data <- all_data |>
  mutate(
    start_before = intervention_start < start_date,
    start_after  = intervention_start > end_date,
    in_window    = coalesce(!start_before & !start_after, keep_missing_start)
  )

# Rows and clients by where the intervention start falls (NA, NA = no start date)
all_data |>
  summarise(rows = n(), clients = n_distinct(client_system_id), .by = c(start_before, start_after, in_window)) |>
  print()

# Action item: people excluded by the start date ("entirely" = no intervention left in the window)
before_start <- all_data |>
  summarise(any_before = any(start_before, na.rm = TRUE), any_kept = any(in_window), .by = client_system_id) |>
  filter(any_before)
cat("Clients with an intervention starting before", format(start_date), ":", nrow(before_start),
    "| excluded entirely:", sum(!before_start$any_kept), "\n")

# The same people by quarter of intervention start, for the grant-period decision
all_data |>
  filter(start_before) |>
  mutate(start_quarter = floor_date(intervention_start, "quarter")) |>
  summarise(clients = n_distinct(client_system_id), rows = n(), .by = start_quarter) |>
  arrange(start_quarter) |>
  print(n = Inf)

# Window rule: keep rows whose intervention starts in the window. A follow-up
# completed after end_date is blanked, so the pair falls back to an earlier
# follow-up (on another row) or, with none, drops out of the outcomes.
all_data_window <- all_data |>
  filter(in_window) |>
  mutate(
    fu_after_cutoff = (post_completion_date > end_date) %in% TRUE,
    across(c(matches("(^|_)post_"), any_of("positive_outcomes")), \(x) replace(x, fu_after_cutoff, NA))
  )

late_pairs <- all_data_window |>
  summarise(any_late = any(fu_after_cutoff), earlier_fu = any(!is.na(post_completion_date)),
            .by = all_of(pair_keys)) |>
  filter(any_late)
cat("Follow-ups after", format(end_date), "blanked:", sum(all_data_window$fu_after_cutoff),
    "| pairs falling back to an earlier follow-up:", sum(late_pairs$earlier_fu),
    "| pairs left with no follow-up:", sum(!late_pairs$earlier_fu), "\n")
cat("Rows discharged after", format(end_date), "(left as is, not yet decided):",
    sum(all_data_window$intervention_end > end_date, na.rm = TRUE), "\n")

# ---- D3: rows without a provider_id --------------------------------------------
reference <- bind_rows(
  select(all_data, any_of(c("provider_id", provider_keys))),
  select(satisfaction, any_of(c("provider_id", provider_keys)))
)

bind_rows(
  summarise_missing_provider(all_data_window, "all_data (in window)", reference),
  summarise_missing_provider(satisfaction, "satisfaction", reference)
) |>
  print(width = Inf)

# ---- D4: funding rounds still unassigned ---------------------------------------
# A round is assigned when its label holds a number ("2", "Round 2"); anything
# else, such as "No Funding Round Selected" or a blank, is unassigned.
rounds <- all_data_window |>
  mutate(round = str_extract(funding_round, "\\d+"))

rounds |> count(funding_round, round) |> print(n = Inf)  # label check, e.g. "2" vs "Round 2"

round_by_provider <- rounds |>
  filter(!is.na(provider_id)) |>
  summarise(
    provider           = str_c(sort(unique(na.omit(provider))), collapse = " / "),
    clients            = n_distinct(client_system_id),
    clients_unassigned = n_distinct(client_system_id[is.na(round)]),
    rounds_assigned    = str_c(sort(unique(na.omit(round))), collapse = ", "),
    n_rounds           = n_distinct(round, na.rm = TRUE),
    .by = provider_id
  )

# For Rikke: providers with any client still unassigned, and rounds seen on their other rows
unassigned <- round_by_provider |>
  filter(clients_unassigned > 0) |>
  arrange(desc(clients_unassigned)) |>
  select(provider_id, provider, clients_unassigned, rounds_assigned)
print(unassigned, n = Inf)
write_csv(unassigned, file.path(out_dir, paste0(Sys.Date(), "-providers_unassigned_funding_round.csv")), na = "")

# Providers with more than one round across their rows (N10: round may need to come from the grant)
one_plus_round <- round_by_provider |> filter(n_rounds > 1) |> print(n = Inf)
write_csv(one_plus_round, file.path(out_dir, paste0(Sys.Date(), "-providers_more_than_one_funding_round.csv")), na = "")


cat("Rows with no provider_id and no assigned round (not in the list; see D3):",
    sum(is.na(rounds$provider_id) & is.na(rounds$round)), "\n")
