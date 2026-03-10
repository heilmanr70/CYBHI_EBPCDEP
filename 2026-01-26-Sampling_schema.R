library(tidyverse)
library(readxl)
library(haven)
library(janitor)

# ----------------------------
# Methods (notes)
# ----------------------------
# Initial stratum-specific sample sizes were determined based on stratum size.
# These were proportionally scaled to meet a fixed total sample size of 100
# while maintaining representation across strata.
#
# QA: Verified that the number of grantees sampled in each stratum matched the
# planned allocation, that strata in the plan were represented, and that the
# sample broadly mirrors the universe across region and grant characteristics.

# ----------------------------
# Load data
# ----------------------------
master_grantee_file <- haven::read_spss(
  "/Users/cristin/CWS Dropbox/Cristin Young/CYBHI Project/Data/MASTER Grantee Files/Quant Team File/MasterGranteeSpreadsheet_forBHESQuantTeam_1-20-26_NEW!.sav"
) %>%
  zap_missing() %>%
  mutate(across(where(is.labelled), as.character)) %>%
  mutate(across(where(is.character), ~ na_if(str_trim(.x), ""))) %>%
  rename_with(~ gsub("^@", "", .x)) %>%
  clean_names() %>%
  rename_with(~ gsub("^x", "", .x)) %>%
  select(where(~ !all(is.na(.))))

grantees <- master_grantee_file %>%
  rename(
    provider_name = `1a_grantee_provider_name`,
    provider_id   = `2grantee_provider_id`,
    region        = `8c_regions`,
    ebp_type      = `9c_intervention_categories_test`,
    grant_track   = `7a_grant_track_for_report`,
    ebp_vs_cdep   = `9b_intervention_type_for_report`,
    num_clients   = `16a_numberof_clients_e_insight`,
    grant_id      = `3grant_id`,
    funding_round = `4funding_round`,
    entity_type   = `12a_grantee_entity_type_for_report`
  ) %>% 
  filter(ebp_type != 999)

# Define the stratification variables we're using
strata_vars <- c("grant_track", "ebp_type")

# Load lookup table (n -> k)
density_lookup <- read_excel("/Users/cristin/Downloads/Sampling density.xlsx") %>%
  clean_names() %>%
  select(n, k) %>%
  mutate(
    n = as.integer(n),
    k = as.integer(k)
  )

stopifnot(all(c("n", "k") %in% names(density_lookup)))

# ----------------------------
# Stratified sampling function
# ----------------------------
make_stratified_sample <- function(grantees, strata_vars, density_lookup,
                                   seed = 289, K_target = 100) {
  
  stopifnot(is.data.frame(grantees))
  stopifnot(is.character(strata_vars), length(strata_vars) >= 1)
  stopifnot(all(strata_vars %in% names(grantees)))
  stopifnot(all(c("n", "k") %in% names(density_lookup)))
  stopifnot(is.numeric(K_target), K_target > 0)
  
  fill_na_as <- function(x) tidyr::replace_na(x, "Missing/Unknown")
  
  k_for_n <- function(n, density_lookup, pct = 0.20, min_k = 6) {
    max_n <- max(density_lookup$n, na.rm = TRUE)
    
    dplyr::if_else(
      n <= max_n,
      density_lookup$k[match(n, density_lookup$n)],
      pmax(min_k, round(pct * n))
    )
  }
  
  # 1) Raw plan (k_raw)
  plan_raw <- grantees %>%
    mutate(across(all_of(strata_vars), fill_na_as)) %>%
    group_by(across(all_of(strata_vars))) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      k_raw = k_for_n(n, density_lookup),
      k_raw = pmin(k_raw, n)
    )
  
  # 2) Cap to ~K_target via scaling + rounding adjustment
  plan <- plan_raw %>% 
    mutate(
      scale = K_target / sum(k_raw),
      k = round(k_raw * scale),
      k = if_else(n > 0 & k < 1, 1L, as.integer(k)),
      k = pmin(k, n)
    )
  
  # Rounding drift correction
  delta <- K_target - sum(plan$k)
  
  if (delta != 0) {
    plan <- plan %>%
      mutate(adj_priority = k_raw - k) %>%
      arrange(desc(adj_priority))
    
    idx <- seq_len(min(abs(delta), nrow(plan)))
    plan$k[idx] <- plan$k[idx] + sign(delta)
    
    plan <- plan %>%
      mutate(
        k = if_else(n > 0 & k < 1, 1L, as.integer(k)),
        k = pmin(k, n)
      ) %>%
      select(-adj_priority, -scale)
  } else {
    plan <- plan %>% select(-scale)
  }
  
  # IMPORTANT: if number of strata > K_target and you enforce k>=1,
  # then sum(k) cannot be forced down to K_target.
  if (sum(plan$k) != K_target) {
    warning(
      "Sum of k (", sum(plan$k), ") does not equal K_target (", K_target, "). ",
      "This often happens when the number of strata exceeds K_target and k>=1 is enforced."
    )
  }
  
  # 3) Sample within strata using capped k
  set.seed(seed)
  
  samp <- grantees %>%
    mutate(across(all_of(strata_vars), fill_na_as)) %>%
    left_join(plan %>% select(all_of(strata_vars), k), by = strata_vars) %>%
    mutate(rand = runif(n())) %>%
    group_by(across(all_of(strata_vars))) %>%
    arrange(rand, .by_group = TRUE) %>%
    filter(row_number() <= first(k)) %>%
    ungroup() %>%
    select(-rand)
  
  list(plan = plan, sample = samp, K_target = K_target, seed = seed)
}

# Run sampling
out <- make_stratified_sample(
  grantees = grantees,
  strata_vars = strata_vars,
  density_lookup = density_lookup,
  seed = 289,
  K_target = 100
)

out$plan
out$sample
write_csv(test, "/Users/cristin/Downloads/test.csv")

# ---------------/Users/cristin/Downloads/grantee_list.csv# ----------------------------
# QA/QC helper
# ----------------------------
check_sampling <- function(out, grantees, strata_vars, id_col = "grant_id") {
  
  stopifnot(is.list(out), all(c("plan", "sample") %in% names(out)))
  stopifnot(is.data.frame(out$plan), is.data.frame(out$sample), is.data.frame(grantees))
  stopifnot(all(strata_vars %in% names(out$plan)))
  stopifnot(all(strata_vars %in% names(out$sample)))
  stopifnot(id_col %in% names(out$sample))
  
  plan <- out$plan
  samp <- out$sample
  
  # 0) basic integrity
  missing_strata <- dplyr::setdiff(
    plan %>% select(all_of(strata_vars)) %>% distinct(),
    samp %>% select(all_of(strata_vars)) %>% distinct()
  )
  
  dup_ids <- samp %>%
    count(.data[[id_col]], name = "n") %>%
    filter(n > 1)
  
  # 1) k vs sampled per stratum
  by_stratum <- samp %>%
    count(across(all_of(strata_vars)), name = "sampled") %>%
    right_join(plan, by = strata_vars) %>%
    mutate(
      sampled = replace_na(sampled, 0L),
      ok = sampled == k
    ) %>%
    arrange(ok, desc(abs(sampled - k)))
  
  mismatched_strata <- by_stratum %>% filter(!ok) # should be zero
  
  # 2) totals
  totals <- tibble(
    target_total = out$K_target %||% NA_integer_,
    sampled_total = nrow(samp),
    plan_total = sum(plan$k),
    n_strata = nrow(plan),
    min_k = min(plan$k),
    max_k = max(plan$k),
    median_k = median(plan$k)
  )
  
  # 3) sample vs universe distributions for the stratification variables
  compare_dist <- function(var) {
    bind_rows(
      grantees %>% 
        mutate(type = "Universe"),
      samp    %>% 
        mutate(type = "Sample")) %>%
      count(type, .data[[var]], name = "n") %>%
      group_by(type) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
  }
  
  # Build comparisons only for vars that actually exist
  comp_vars <- strata_vars[strata_vars %in% names(grantees)]
  comparisons <- setNames(lapply(comp_vars, compare_dist), comp_vars)
  
  # human-readable summary
  message("=== Sampling QA Summary ===")
  message("Sample rows: ", nrow(samp))
  message("Plan total k: ", sum(plan$k))
  message("Target total: ", totals$target_total)
  message("Strata: ", nrow(plan),
          " | min_k=", min(plan$k),
          " | max_k=", max(plan$k),
          " | median_k=", median(plan$k))
  message("Mismatched strata (sampled != k): ", nrow(mismatched_strata))
  message("Duplicate ", id_col, "s in sample: ", nrow(dup_ids))
  message("Strata in plan missing entirely in sample: ", nrow(missing_strata))
  
  list(
    totals = totals,
    by_stratum = by_stratum,
    mismatched_strata = mismatched_strata,
    duplicate_ids = dup_ids,
    missing_strata = missing_strata,
    comparisons = comparisons
  )
}

# Run QA/QC
qa <- check_sampling(out, grantees, strata_vars, id_col = "grant_id")

qa$totals
qa$mismatched_strata
qa$duplicate_ids
qa$comparisons$ebp_type



count_strata <- function(vars) {
  grantees %>%
    mutate(across(all_of(vars), ~ replace_na(.x, "Missing/Unknown"))) %>%
    distinct(across(all_of(vars))) %>%
    nrow()
}

count_strata(c("region", "grant_track"))
count_strata(c("region", "grant_track", "ebp_vs_cdep"))
count_strata(c("region", "grant_track", "ebp_vs_cdep", "ebp_type"))

# region characteristics
region_dist <- bind_rows(
  grantees %>% mutate(type = "Universe"),
  out$sample %>% mutate(type = "Sample")
) %>%
  count(type, region) %>%
  group_by(type) %>%
  mutate(
    pct = n / sum(n),
    pct = round(100 * pct, 1)
  ) %>%
  ungroup()

region_dist <- region_dist

region_compare <- region_dist %>%
  select(type, region, pct) %>%
  pivot_wider(names_from = type, values_from = pct) %>%
  mutate(diff_pp = Sample - Universe)

region_compare

region_compare %>%
  pivot_longer(c(Universe, Sample), names_to = "type", values_to = "pct") %>%
  ggplot(aes(x = region, y = pct, fill = type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Regional Distribution: Sample vs Universe",
    y = "Percent of grantees",
    x = "Region"
  )

# funding round
funding_compare <- bind_rows(
  grantees %>% mutate(type = "Universe"),
  out$sample %>% mutate(type = "Sample")
) %>%
  count(type, funding_round) %>%
  group_by(type) %>%
  mutate(
    pct = n / sum(n),
    pct = round(100 * pct, 1)
  ) %>%
  ungroup() %>% 
  select(type, funding_round, n, pct) %>%
  pivot_wider(names_from = type, values_from = c(n, pct)) %>%
  mutate(diff_pp = Sample - Universe)

funding_compare

funding_compare %>%
  pivot_longer(c(Universe, Sample), names_to = "type", values_to = "pct") %>%
  ggplot(aes(x = funding_round, y = pct, fill = type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Funding Round Distribution: Sample vs Universe",
    y = "Percent of grantees",
    x = "Funding Round"
  )

# region
region_compare <- bind_rows(
  grantees %>% mutate(type = "Universe"),
  out$sample %>% mutate(type = "Sample")
) %>%
  count(type, region) %>%
  group_by(type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  select(type, region, n, pct) %>%
  pivot_wider(names_from = type, values_from = c(n, pct)) %>%
  mutate(diff_pp = pct_Sample - pct_Universe)

region_compare

# entity type
entity_dist <- bind_rows(
  grantees %>% mutate(type = "Universe"),
  out$sample %>% mutate(type = "Sample")
) %>%
  count(type, entity_type) %>%
  group_by(type) %>%
  mutate(
    pct = n / sum(n),
    pct = round(100 * pct, 1)
  ) %>%
  ungroup()

entity_compare <- entity_dist %>%
  select(type, entity_type, pct) %>%
  pivot_wider(names_from = type, values_from = pct) %>%
  mutate(diff_pp = Sample - Universe)

entity_compare

entity_compare %>%
  pivot_longer(c(Universe, Sample), names_to = "type", values_to = "pct") %>%
  ggplot(aes(x = entity_type, y = pct, fill = type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Entity Type Distribution: Sample vs Universe",
    y = "Percent of grantees",
    x = "Entity Type"
  )
