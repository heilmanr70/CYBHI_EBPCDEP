# Native American/Indian numbers for Rikke
# 12.16.2025
# Cristin


grantees <- read_csv("/Users/cristin/Downloads/grantee_list.csv")
head(grantees)

ihs_summary <- discharge_data_filtered %>% 
  filter(provider_system_id %in% grantees$provider_system_id)

ihs_summary %>% 
  group_by(provider, intervention_name) %>% 
  count() %>% 
  clipr::write_clip()

ihs_summary %>% 
  group_by(provider, intervention_name) %>% 
  summarise(
    n_clients = n_distinct(client_system_id),
    .groups = "drop"
  ) %>% 
  clipr::write_clip()

ihs_summary %>% 
  tabyl(provider, positive_outcomes) %>% 
  clipr::write_clip()

ihs_summary %>% 
  count(is.na(pre_score))

ihs_summary %>% 
  count(is.na(post_score))

ihs_summary %>% 
  count(!is.na(pre_score) & !is.na(post_score))
