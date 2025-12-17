# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)
library(haven)

# Create Directories ------------------------------------------------------

#dir_create("grantee-characteristics")


# read files and clean field names --------------------------------------------------------------

master_grantee_file_raw <-
read_sav("/Users/heilm/Dropbox/CYBHI Project/Data/MASTER Grantee Files/Quant Team File/MasterGranteeSpreadsheet_forBHESQuantTeam_12-15-25.sav")

master_grantee_file_raw |> 
  glimpse() |> 
  clean_names() |> 
  glimpse()

names(master_grantee_file_raw)

grantees <- master_grantee_file_raw |> 
  rename_with(~ .x |> 
                str_remove("^@\\d+[a-z]?") |>                  # drop leading @ + number + optional letter
                str_replace_all("([a-z0-9])([A-Z])", "\\1_\\2") |>  # camel -> snake
                str_replace_all("__+", "_") |>                 # collapse double underscores
                str_to_lower()
  )

names(grantees)

# count grantees by round -------------------------------------------------

grantees_by_round <- grantees |> 
filter(!is.na(funding_round), funding_round != "") |> 
  count(funding_round, name = "n_grantees") |> 
  mutate(pct_grantees = n_grantees / sum(n_grantees)*100) |> 
  arrange(funding_round)

grantees_by_round


# grantees by track and round ---------------------------------------------

#first, cleaning for table 
grantees <- grantees |> 
  mutate(
    grant_track_ex_st_int = case_when(
      str_detect(str_to_lower(grant_track_integrated), "integrated") ~ grant_track_integrated,
      TRUE ~ grant_track
    )
  )

grantees <- grantees %>%
  mutate(
    grant_track_ex_st_int_orig = grant_track_ex_st_int,  # optional: keep original
    .gt = grant_track_ex_st_int %>%
      as.character() %>%
      str_squish() %>%
      str_to_lower(),
    
    grant_track_ex_st_int = case_when(
      str_detect(.gt, "integrat") ~ "Integrated",                      # e.g., "Integrated", "Expansion - Integrated"
      str_detect(.gt, "^start\\s*[- ]?up$|^startup$") ~ "Start-up",    # "Start up", "Start-up", "Startup"
      str_detect(.gt, "expan") ~ "Expansion",                          # "Expansion", "Expanding", etc.
      str_detect(.gt, "^other$|unknown|n/a|not\\s*applicable") ~ "Other",
      TRUE ~ "Other"                                                   # force everything else into the 4 buckets
    )
  ) %>%
  select(-.gt) 

grantees %>%
  filter(grant_track_ex_st_int == "Other") %>%
  count(grant_track_ex_st_int_orig, sort = TRUE)


#between rounds table of percents
round_levels <- paste("Round", 1:5)
track_levels <- c("Start-up", "Expansion", "Integrated", "Other")
 
table3 <- grantees |> 
 # keep only rows with the fields you need
 filter(!is.na(funding_round), funding_round != "",
        !is.na(grant_track_ex_st_int), grant_track_ex_st_int != "") |> 
   mutate(
     # normalize round labels (edit/remove if yours are already "Round 1"..."Round 5")
     funding_round = str_trim(as.character(funding_round)),
     funding_round = case_when(
     funding_round %in% as.character(1:5) ~ paste("Round", funding_round),
     str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", str_extract(funding_round, "\\d")),
     TRUE ~ funding_round
     ),
     funding_round = factor(funding_round, levels = round_levels),
   
     grant_track_ex_st_int = str_trim(as.character(grant_track_ex_st_int)),
     grant_track_ex_st_int = factor(grant_track_ex_st_int, levels = track_levels)
     ) |> 
     # counts by track x round
     count(grant_track_ex_st_int, funding_round, name = "n") |> 
     complete(grant_track_ex_st_int, funding_round, fill = list(n = 0)) |> 
     group_by(grant_track_ex_st_int) |> 
     mutate(
       track_n = sum(n),
       row_pct = if_else(track_n > 0, n / track_n, 0),
       cell = sprintf("%d (%.1f%%)", n, 100 * row_pct)
     ) |> 
   ungroup() |> 
   # pivot rounds into columns
   select(grant_track_ex_st_int, funding_round, cell, track_n) |> 
   pivot_wider(names_from = funding_round, values_from = cell) |> 
   # add "All Rounds" column: track total and % of all grantees
   mutate(
     grand_n = sum(track_n, na.rm = TRUE),
     `All Rounds` = sprintf("%d (%.1f%%)", track_n, 100 * track_n / grand_n)
     ) |> 
   select(-track_n, -grand_n) |> 
   arrange(grant_track_ex_st_int)

table3

#within rounds table of percents
round_levels <- paste("Round", 1:5)
track_levels <- c("Start-up", "Expansion", "Integrated", "Other")

table3 <- grantees %>%
  filter(!is.na(funding_round), funding_round != "",
         !is.na(grant_track_ex_st_int), grant_track_ex_st_int != "") %>%
  mutate(
    funding_round = str_trim(as.character(funding_round)),
    funding_round = case_when(
      funding_round %in% as.character(1:5) ~ paste("Round", funding_round),
      str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", str_extract(funding_round, "\\d")),
      TRUE ~ funding_round
    ),
    funding_round = factor(funding_round, levels = round_levels),
    grant_track_ex_st_int = factor(grant_track_ex_st_int, levels = track_levels)
  ) %>%
  count(grant_track_ex_st_int, funding_round, name = "n") %>%
  complete(grant_track_ex_st_int, funding_round, fill = list(n = 0)) %>%
  group_by(funding_round) %>%
  mutate(
    round_n = sum(n),
    col_pct = if_else(round_n > 0, n / round_n, 0),
    cell = sprintf("%d (%.1f%%)", n, 100 * col_pct)
  ) %>%
  ungroup() %>%
  # Pivot rounds into columns
  select(grant_track_ex_st_int, funding_round, cell) %>%
  pivot_wider(names_from = funding_round, values_from = cell) %>%
  # Add “All Rounds” column: overall track breakdown across the full dataset
  left_join(
    grantees %>%
      filter(!is.na(funding_round), funding_round != "",
             !is.na(grant_track_ex_st_int), grant_track_ex_st_int != "") %>%
      count(grant_track_ex_st_int, name = "track_n") %>%
      mutate(all_rounds = sprintf("%d (%.1f%%)", track_n, 100 * track_n / sum(track_n))) %>%
      select(grant_track_ex_st_int, all_rounds),
    by = "grant_track_ex_st_int"
  ) %>%
  rename(`All Rounds` = all_rounds) %>%
  arrange(grant_track_ex_st_int)

table3


# grantees by round and implementation type -------------------------------

#first, cleaning for the table
grantees <- grantees %>%
  mutate(
    cdep_ebp = case_when(
      str_to_lower(str_trim(as.character(cdep))) %in% c("yes", "y", "true", "1") ~ "CDEP",
      TRUE ~ "EBP"
    )
  )

# table 5 BETWEEN rounds table of percents ----------------------------------

round_levels <- paste("Round", 1:5)
cdep_levels  <- c("EBP", "CDEP")

table5_between <- grantees |>
  filter(!is.na(funding_round), funding_round != "") |>
  mutate(
    funding_round = str_trim(as.character(funding_round)),
    funding_round = case_when(
      funding_round %in% as.character(1:5) ~ paste("Round", funding_round),
      str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", str_extract(funding_round, "\\d")),
      TRUE ~ funding_round
    ),
    funding_round = factor(funding_round, levels = round_levels),
    
    cdep_ebp = str_trim(as.character(cdep_ebp)),
    cdep_ebp = factor(cdep_ebp, levels = cdep_levels)
  ) |>
  count(cdep_ebp, funding_round, name = "n") |>
  complete(cdep_ebp, funding_round, fill = list(n = 0)) |>
  group_by(cdep_ebp) |>
  mutate(
    type_n = sum(n),
    row_pct = if_else(type_n > 0, n / type_n, 0),
    cell = sprintf("%d (%.1f%%)", n, 100 * row_pct)
  ) |>
  ungroup() |>
  select(cdep_ebp, funding_round, cell, type_n) |>
  pivot_wider(names_from = funding_round, values_from = cell) |>
  mutate(
    grand_n = sum(type_n, na.rm = TRUE),
    `All Rounds` = sprintf("%d (%.1f%%)", type_n, 100 * type_n / grand_n)
  ) |>
  select(-type_n, -grand_n) |>
  arrange(cdep_ebp)

table5_between

# table 5 WITHIN rounds table of percents -----------------------------------

round_levels <- paste("Round", 1:5)
cdep_levels  <- c("EBP", "CDEP")

table5_within <- grantees %>%
  filter(!is.na(funding_round), funding_round != "") %>%
  mutate(
    funding_round = str_trim(as.character(funding_round)),
    funding_round = case_when(
      funding_round %in% as.character(1:5) ~ paste("Round", funding_round),
      str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", funding_round, "\\1_\\2") %>% str_replace("\\\\1_\\\\2",""), # (noop safety)
      TRUE ~ funding_round
    ),
    # re-run the correct R# normalization (kept separate to avoid the noop line above if you prefer)
    funding_round = case_when(
      funding_round %in% round_levels ~ funding_round,
      str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", str_extract(funding_round, "\\d")),
      TRUE ~ funding_round
    ),
    funding_round = factor(funding_round, levels = round_levels),
    
    cdep_ebp = factor(str_trim(as.character(cdep_ebp)), levels = cdep_levels)
  ) %>%
  count(cdep_ebp, funding_round, name = "n") %>%
  complete(cdep_ebp, funding_round, fill = list(n = 0)) %>%
  group_by(funding_round) %>%
  mutate(
    round_n = sum(n),
    col_pct = if_else(round_n > 0, n / round_n, 0),
    cell = sprintf("%d (%.1f%%)", n, 100 * col_pct)
  ) %>%
  ungroup() %>%
  select(cdep_ebp, funding_round, cell) %>%
  pivot_wider(names_from = funding_round, values_from = cell) %>%
  left_join(
    grantees %>%
      filter(!is.na(funding_round), funding_round != "") %>%
      count(cdep_ebp, name = "type_n") %>%
      mutate(all_rounds = sprintf("%d (%.1f%%)", type_n, 100 * type_n / sum(type_n))) %>%
      select(cdep_ebp, all_rounds),
    by = "cdep_ebp"
  ) %>%
  rename(`All Rounds` = all_rounds) %>%
  arrange(cdep_ebp)

table5_within

# grantee by round and region ---------------------------------------------

#first, prep for table
region_levels <- c(paste("Region", 1:10), "Statewide")

grantees <- grantees |>
  mutate(
    region_label = case_when(
      as.integer(regions) == 11 ~ "Statewide",
      as.integer(regions) %in% 1:10 ~ paste("Region", as.integer(regions)),
      TRUE ~ NA_character_
    )
  )

# table 6 BETWEEN rounds table of percents ----------------------------------

round_levels <- paste("Round", 1:5)

table6_between <- grantees |>
  filter(!is.na(funding_round), funding_round != "",
         !is.na(region_label), region_label != "") |>
  mutate(
    funding_round = str_trim(as.character(funding_round)),
    funding_round = case_when(
      funding_round %in% as.character(1:5) ~ paste("Round", funding_round),
      str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", str_extract(funding_round, "\\d")),
      TRUE ~ funding_round
    ),
    funding_round = factor(funding_round, levels = round_levels),
    
    region_label = str_trim(as.character(region_label)),
    region_label = factor(region_label, levels = region_levels)
  ) |>
  count(region_label, funding_round, name = "n") |>
  complete(region_label, funding_round, fill = list(n = 0)) |>
  group_by(region_label) |>
  mutate(
    region_n = sum(n),
    row_pct = if_else(region_n > 0, n / region_n, 0),
    cell = sprintf("%d (%.1f%%)", n, 100 * row_pct)
  ) |>
  ungroup() |>
  select(region_label, funding_round, cell, region_n) |>
  pivot_wider(names_from = funding_round, values_from = cell) |>
  mutate(
    grand_n = sum(region_n, na.rm = TRUE),
    `All Rounds` = sprintf("%d (%.1f%%)", region_n, 100 * region_n / grand_n)
  ) |>
  select(-region_n, -grand_n) |>
  arrange(region_label)

table6_between

# table 6 WITHIN rounds table of percents -----------------------------------

round_levels <- paste("Round", 1:5)

table6_within <- grantees %>%
  filter(!is.na(funding_round), funding_round != "",
         !is.na(region_label), region_label != "") %>%
  mutate(
    funding_round = str_trim(as.character(funding_round)),
    funding_round = case_when(
      funding_round %in% as.character(1:5) ~ paste("Round", funding_round),
      str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", str_extract(funding_round, "\\d")),
      TRUE ~ funding_round
    ),
    funding_round = factor(funding_round, levels = round_levels),
    
    region_label = factor(str_trim(as.character(region_label)), levels = region_levels)
  ) %>%
  count(region_label, funding_round, name = "n") %>%
  complete(region_label, funding_round, fill = list(n = 0)) %>%
  group_by(funding_round) %>%
  mutate(
    round_n = sum(n),
    col_pct = if_else(round_n > 0, n / round_n, 0),
    cell = sprintf("%d (%.1f%%)", n, 100 * col_pct)
  ) %>%
  ungroup() %>%
  select(region_label, funding_round, cell) %>%
  pivot_wider(names_from = funding_round, values_from = cell) %>%
  left_join(
    grantees %>%
      filter(!is.na(funding_round), funding_round != "",
             !is.na(region_label), region_label != "") %>%
      count(region_label, name = "region_n") %>%
      mutate(all_rounds = sprintf("%d (%.1f%%)", region_n, 100 * region_n / sum(region_n))) %>%
      select(region_label, all_rounds),
    by = "region_label"
  ) %>%
  rename(`All Rounds` = all_rounds) %>%
  arrange(region_label)

table6_within


# grantee by entity type and round ----------------------------------------

#first, prep for table
entity_levels <- c(
  "Community-based organization (CBO)",
  "County or City Government",
  "Family Resource Center",
  "Hospital and Hospital System",
  "Institution of Higher Education",
  "Local Education Agency",
  "Provider Clinic",
  "Statewide and Local Agency",
  "Tribal Entity",
  "Other"
)

grantees <- grantees |>
  mutate(
    entity_type_label = case_when(
      as.integer(grantee_entity_type_for_report) == 1  ~ "Community-based organization (CBO)",
      as.integer(grantee_entity_type_for_report) == 2  ~ "County or City Government",
      as.integer(grantee_entity_type_for_report) == 3  ~ "Family Resource Center",
      as.integer(grantee_entity_type_for_report) == 4  ~ "Hospital and Hospital System",
      as.integer(grantee_entity_type_for_report) == 5  ~ "Institution of Higher Education",
      as.integer(grantee_entity_type_for_report) == 6  ~ "Local Education Agency",
      as.integer(grantee_entity_type_for_report) == 7  ~ "Provider Clinic",
      as.integer(grantee_entity_type_for_report) == 8  ~ "Statewide and Local Agency",
      as.integer(grantee_entity_type_for_report) == 9  ~ "Tribal Entity",
      as.integer(grantee_entity_type_for_report) == 10 ~ "Other",
      TRUE ~ NA_character_
    )
  )

# table 4 BETWEEN rounds table of percents ----------------------------------

round_levels <- paste("Round", 1:5)

table4_between <- grantees |>
  filter(!is.na(funding_round), funding_round != "",
         !is.na(entity_type_label), entity_type_label != "") |>
  mutate(
    funding_round = str_trim(as.character(funding_round)),
    funding_round = case_when(
      funding_round %in% as.character(1:5) ~ paste("Round", funding_round),
      str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", str_extract(funding_round, "\\d")),
      TRUE ~ funding_round
    ),
    funding_round = factor(funding_round, levels = round_levels),
    
    entity_type_label = factor(str_trim(as.character(entity_type_label)), levels = entity_levels)
  ) |>
  count(entity_type_label, funding_round, name = "n") |>
  complete(entity_type_label, funding_round, fill = list(n = 0)) |>
  group_by(entity_type_label) |>
  mutate(
    entity_n = sum(n),
    row_pct  = if_else(entity_n > 0, n / entity_n, 0),
    cell     = sprintf("%d (%.1f%%)", n, 100 * row_pct)
  ) |>
  ungroup() |>
  select(entity_type_label, funding_round, cell, entity_n) |>
  pivot_wider(names_from = funding_round, values_from = cell) |>
  mutate(
    grand_n = sum(entity_n, na.rm = TRUE),
    `All Rounds` = sprintf("%d (%.1f%%)", entity_n, 100 * entity_n / grand_n)
  ) |>
  select(-entity_n, -grand_n) |>
  arrange(entity_type_label)

table4_between

# table 4 WITHIN rounds table of percents -----------------------------------

round_levels <- paste("Round", 1:5)

table4_within <- grantees %>%
  filter(!is.na(funding_round), funding_round != "",
         !is.na(entity_type_label), entity_type_label != "") %>%
  mutate(
    funding_round = str_trim(as.character(funding_round)),
    funding_round = case_when(
      funding_round %in% as.character(1:5) ~ paste("Round", funding_round),
      str_detect(funding_round, "^R\\s*\\d$") ~ paste("Round", str_extract(funding_round, "\\d")),
      TRUE ~ funding_round
    ),
    funding_round = factor(funding_round, levels = round_levels),
    
    entity_type_label = factor(str_trim(as.character(entity_type_label)), levels = entity_levels)
  ) %>%
  count(entity_type_label, funding_round, name = "n") %>%
  complete(entity_type_label, funding_round, fill = list(n = 0)) %>%
  group_by(funding_round) %>%
  mutate(
    round_n = sum(n),
    col_pct = if_else(round_n > 0, n / round_n, 0),
    cell    = sprintf("%d (%.1f%%)", n, 100 * col_pct)
  ) %>%
  ungroup() %>%
  select(entity_type_label, funding_round, cell) %>%
  pivot_wider(names_from = funding_round, values_from = cell) %>%
  left_join(
    grantees %>%
      filter(!is.na(funding_round), funding_round != "",
             !is.na(entity_type_label), entity_type_label != "") %>%
      count(entity_type_label, name = "entity_n") %>%
      mutate(all_rounds = sprintf("%d (%.1f%%)", entity_n, 100 * entity_n / sum(entity_n))) %>%
      select(entity_type_label, all_rounds),
    by = "entity_type_label"
  ) %>%
  rename(`All Rounds` = all_rounds) %>%
  arrange(entity_type_label)

table4_within

