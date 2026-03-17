library(haven)

# load in spss data
master_grantee_file <- haven::read_spss("/Users/cristin/CWS Dropbox/Cristin Young/CYBHI Project/Data/MASTER Grantee Files/Quant Team File/MasterGranteeSpreadsheet_forBHESQuantTeam_1-20-26_NEW!.sav") %>% 
  zap_missing() %>%
  mutate(across(where(is.labelled), as.character)) %>%
  mutate(across(where(is.character), ~ na_if(str_trim(.x), ""))) %>% 
  rename_with(~ gsub("^@", "", .x)) %>% 
  janitor::clean_names() %>% 
  rename_with(~ gsub("^x", "", .x)) %>% 
  select(where(~ !all(is.na(.))))

#join to all data
master_grantee_file <- master_grantee_file %>%
  mutate(
    funding_round_num = recode(
      `4funding_round`,
      "Round 1" = "1",
      "Round 2" = "2",
      "Round 3" = "3",
      "Round 4" = "4",
      "Round 5" = "5"
    )
  )

all_data_spss <- left_join(all_data, master_grantee_file, by = c("provider" = "1a_grantee_provider_name", 
                                                                 "funding_round" = "funding_round_num")) %>% 
  select(where(~ !all(is.na(.)))) %>% # checking to see if any columns are all NA 
  rename(region = `8c_regions`,
         ebp_type = `9c_intervention_categories_test`,
         grant_track = `7a_grant_track_for_report`,
         ebp_vs_cdep = `9b_intervention_type_for_report`) %>% 
  mutate(successful_discharge = case_when(
    reason_for_discharge %in% c(
    "Mutually agreed cessation of treatment (Successful Completion)",
    "Clinically referred out") ~ 1,
    TRUE ~ 0))

all_data_spss %>% 
  tabyl(urban_rural) %>% 
  adorn_pct_formatting(digits = 2)


urban_rural_by_grantee <- all_data_spss %>%
  filter(!is.na(urban_rural)) %>%
  count(provider_system_id, provider, urban_rural) %>%
  group_by(provider_system_id, provider) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()


urban_rural_label <- all_data_spss %>%
  filter(!is.na(urban_rural)) %>%
  count(provider_system_id, provider, urban_rural) %>%
  group_by(provider_system_id, provider) %>%
  summarise(
    urban_pct = n[urban_rural == "Urban"] / sum(n),
    rural_pct = n[urban_rural == "Rural"] / sum(n),
    .groups = "drop"
  ) %>%
  mutate(
    urban_rural_grantee = case_when(
      urban_pct >= 0.75 ~ "Mostly Urban",
      rural_pct >= 0.75 ~ "Mostly Rural",
      TRUE ~ "Mixed"
    )
  )

test_label <- test %>%
  left_join(urban_rural_label, by = c("provider_name" = "provider"))
write_csv(test_label, "/Users/cristin/Downloads/test_label.csv")

# see what's not matching when joining
comparison <- all_data %>%
  left_join(
    master_grantee_file,
    by = c("provider" = "1a_grantee_provider_name")
  ) %>%
  filter(
    intervention_name != `9a_ebpcde` | is.na(`9a_ebpcde`)
  ) %>%
  distinct(
    provider,
    intervention_name,
    `9a_ebpcde`
  ) %>%
  arrange(provider, intervention_name)

comparison

master_sets <- master_grantee_file %>%
  transmute(
    provider = `1a_grantee_provider_name`,
    intervention_master = `9a_ebpcde`
  ) %>%
  distinct() %>%
  group_by(provider) %>%
  summarise(master_list = list(sort(unique(intervention_master))), .groups = "drop")

all_sets <- all_data %>%
  transmute(
    provider = provider,
    intervention_all = intervention_name
  ) %>%
  distinct() %>%
  group_by(provider) %>%
  summarise(all_list = list(sort(unique(intervention_all))), .groups = "drop")

#all_not_master: interventions appearing in all_data for that provider that don’t exist in the master list for that provider
#master_not_all: interventions in master that never show up in all_data for that provider

diff_report <- full_join(all_sets, master_sets, by = "provider") %>%
  mutate(
    all_list    = coalesce(all_list, list(character(0))),
    master_list = coalesce(master_list, list(character(0))),
    in_all_not_master = purrr::map2(all_list, master_list, setdiff),
    in_master_not_all = purrr::map2(master_list, all_list, setdiff)
  ) %>%
  filter(lengths(in_all_not_master) > 0 | lengths(in_master_not_all) > 0) %>%
  transmute(
    provider,
    all_not_master = purrr::map_chr(in_all_not_master, ~ paste(.x, collapse = " | ")),
    master_not_all = purrr::map_chr(in_master_not_all, ~ paste(.x, collapse = " | "))
  ) %>%
  arrange(provider)

diff_report

#finding diffs a different way:
master_intervention <- master_grantee_file %>% 
  distinct(provider = `1a_grantee_provider_name`, intervention_name = `9a_ebpcde`)

all_data_intervention <- all_data %>% 
  distinct(provider, intervention_name)

in_all_not_master <- all_data_intervention %>%
  anti_join(master_intervention, by = c("provider", "intervention_name")) %>%
  arrange(provider, intervention_name)

in_all_not_master
write_csv(in_all_not_master, "/Users/cristin/Downloads/in_all_not_master.csv")
write_csv(all_data_intervention, "/Users/cristin/Downloads/all_data_intervention.csv")

in_master_not_all <- master_intervention %>%
  anti_join(all_data_intervention, by = c("provider", "intervention_name")) %>%
  arrange(provider, intervention_name)

in_master_not_all
write_csv(in_master_not_all, "/Users/cristin/Downloads/in_master_not_all.csv")
write_csv(master_intervention, "/Users/cristin/Downloads/master_intervention.csv")

# which grantees are driving it? 
in_all_not_master %>%
  count(provider, sort = TRUE)

in_master_not_all %>%
  count(provider, sort = TRUE)

anti_join(
  distinct(all_data_intervention, intervention_name),
  distinct(master_intervention, intervention_name),
  by = "intervention_name"
) %>% arrange(intervention_name)


#other code -------
results <- all_data_spss %>%
  dplyr::group_by(region, grant_track, ebp_vs_cdep, ebp_type) %>%
  dplyr::summarise(
    n = dplyr::n(),
    n_success = sum(successful_discharge == 1, na.rm = TRUE),
    pct_success = 100 * n_success / n,
    .groups = "drop"
  )


col_h_long <- all_data_spss %>%
  filter(!is.na(successful_discharge)) %>%
  group_by(
    region,
    grant_track,
    ebp_vs_cdep,
    ebp_type,
    successful_discharge
  ) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(
    region,
    grant_track,
    ebp_vs_cdep,
    ebp_type
  ) %>%
  mutate(
    n_total = sum(n),
    pct = round(100 * n / sum(n), 1)
  )

col_h_wide <- col_h_long %>%
  mutate(
    discharge_label = if_else(
      successful_discharge == 1,
      "Successful",
      "Unsuccessful"
    )
  ) %>%
  select(
    region,
    grant_track,
    ebp_vs_cdep,
    ebp_type,
    discharge_label,
    n,
    pct,
    n_total
  ) %>%
  pivot_wider(
    names_from = discharge_label,
    values_from = c(n, pct)
  )

col_h_final <- col_h_wide %>%
  mutate(
    column_h = paste0(
      n_Successful, " (", pct_Successful, "%) / ",
      n_Unsuccessful, " (", pct_Unsuccessful, "%)"
    )
  )
