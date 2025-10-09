library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(stringr)

# ------------------------------------------------------------
# compare_datasets_by_vars()
# - old_all_data/new_all_data: your two datasets
# - id_col: optional (kept for reference, not required)
# - group_col: the grantee column
# - vars: character vector of variable names to compare (e.g., c("housing","age_group"))
# - return = c("stacked", "list"): "stacked" -> one big tibble; "list" -> named list per var
# ------------------------------------------------------------
old_demo <- read_xlsx("/Users/cristin/Dropbox/Data/eInsight Exports/Admin Report Exports/2025-09-17-client_demographics.xlsx", na = c("-", "", "NA", "N/A")) %>% 
  janitor::clean_names()
old_demo <- old_demo %>%
  filter(!if_any(c(provider_id, client_id), ~ str_detect(as.character(.x), "DEMO"))) %>% 
  arrange(., provider_id)
new_demo <- read_xlsx("/Users/cristin/Dropbox/Data/eInsight Exports/Admin Report Exports/2025-10-09-client_demographics.xlsx", na = c("-", "", "NA", "N/A")) %>% 
  janitor::clean_names()
new_demo <- new_demo %>%
  filter(!if_any(c(provider_id, client_id), ~ str_detect(as.character(.x), "DEMO"))) %>% 
  arrange(., provider_id)

# which IDs have been repeated and how many times? 
duplicates_new <- new_demo %>% 
  group_by(client_id) %>% 
  filter(n() > 1) %>%
  tally() 
# which IDs have been repeated and how many times? 
duplicates <- old_demo %>% 
  group_by(client_id) %>% 
  filter(n() > 1) %>%
  tally() 

#for now, drop the 45 clients (164 rows)
old_demo <- old_demo %>% 
  filter(!(client_id %in% duplicates$client_id))

# filter to affected clients
old_restricted <- old_demo %>%
  semi_join(affected_clients, by = c("client_id" = "system_client_id"))

new_restricted <- new_demo %>%
  semi_join(affected_clients, by = c("client_id" = "system_client_id"))


## COMPARING DATAFRAMES ##
compare_datasets_by_vars <- function(old_df, new_df,
                                     group_col = "provider_id",
                                     vars,
                                     id_col = "client_id",
                                     return = c("stacked", "list")) {
  return <- match.arg(return)
  
  # Prepare combined long frame with dataset flag
  combined <- bind_rows(
    old_df %>% mutate(dataset = "old"),
    new_df %>% mutate(dataset = "new")
  )
  
  # Helper that produces a tidy comparison for one variable
  cmp_one <- function(var_nm) {
    var_sym <- sym(var_nm)
    grp_sym <- sym(group_col)
    
    # Summarise counts by grantee x dataset x level
    summ <- combined %>%
      mutate(
        .level = !!var_sym,
        .level = if_else(is.na(.level), "(Missing)", as.character(.level))
      ) %>%
      group_by(!!grp_sym, dataset, .level, .drop = FALSE) %>%
      summarise(n = n(), .groups = "drop_last") %>%
      # percents are within (grantee x dataset)
      mutate(pct = 100 * n / sum(n)) %>%
      ungroup()
    
    # Pivot to old/new side-by-side, fill absent combos with 0
    out <- summ %>%
      pivot_wider(
        names_from = dataset,
        values_from = c(n, pct),
        values_fill = 0
      ) %>%
      mutate(
        diff_n   = n_new  - n_old,
        diff_pct = pct_new - pct_old
      ) %>%
      arrange(!!grp_sym, desc(abs(diff_pct)), .level) %>%
      select(!!grp_sym, .level,
             n_old, n_new, diff_n,
             pct_old, pct_new, diff_pct)
    
    out %>%
      mutate(variable = var_nm, .before = 1) %>%
      rename(grantee = !!grp_sym, level = .level)
  }
  
  results <- map(vars, cmp_one)
  names(results) <- vars
  
  if (return == "list") {
    results
  } else {
    bind_rows(results)
  }
}

## Variables to check----------------
vars_to_check <- c(
  "client_type", "pregnant", "gender_identity", "sexual_orientation",
  "insurance_status", "primary_language", "preferred_language",
  "urban_rural", "housing_status"
)

# Replace these with your actual data frames:
# old_affected = old (affected) dataset; new_fixed = new (corrected) dataset
audit_tbl <- compare_datasets_by_vars(
  old_df   = old_restricted,
  new_df   = new_restricted,
  group_col = "provider_id",
  vars      = vars_to_check,
  return    = "stacked"
)

# Peek biggest shifts per variable/grantee
audit_tbl %>%
  arrange(variable, grantee, desc(abs(diff_pct))) %>%
  dplyr::slice_head(n = 200)

pregnancy_changes <- audit_tbl %>%
  filter(variable == "pregnant") %>%
  arrange(desc(abs(diff_pct)))

pregnancy_changes

changed_any <- audit_tbl %>% filter(diff_n != 0 | abs(diff_pct) > 0.0001)
changed_any

audit_tbl %>%
  filter(variable == "pregnant") %>%
  ggplot(aes(x = grantee, y = diff_pct, fill = level)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Change in % by Housing Status (new - old)",
       y = "Difference in percentage points", x = "Grantee") +
  theme_minimal()


vars_changed <- audit_tbl %>%
  group_by(variable, grantee) %>%
  summarise(
    changed = any(diff_n != 0 | abs(diff_pct) > 1e-9), 
    .groups = "drop"
  ) %>%
  filter(changed) %>%
  select(-changed)

vars_changed

vars_changed_grantee <- vars_changed %>%
  group_by(grantee) %>%
  summarise(vars = paste(variable, collapse = ", "), .groups = "drop")

#Counts of variables that changed per grantee:
vars_changed_size %>%
  count(grantee, name = "n_variables_changed") %>%
  arrange(desc(n_variables_changed))

vars_changed %>%
  count(grantee, name = "n_vars_changed")

vars_changed %>%
  count(variable, name = "n_grantees")

# audit_tbl columns assumed:
# variable | grantee | level | n_old | n_new | diff_n | pct_old | pct_new | diff_pct

epsilon <- 1e-9  # tolerance for tiny % diffs

vars_changed_size <- audit_tbl %>%
  mutate(
    abs_diff_pct = abs(diff_pct),
    abs_diff_n   = abs(diff_n),
    changed      = diff_n != 0 | abs(coalesce(diff_pct, 0)) > epsilon
  ) %>%
  group_by(variable, grantee) %>%
  summarise(
    # How many levels changed at all?
    n_levels_changed = sum(changed, na.rm = TRUE),
    
    # Biggest % change (by absolute percentage points) and which level it was
    max_abs_diff_pct = suppressWarnings(max(abs_diff_pct, na.rm = TRUE)),
    level_max_pct    = level[which.max(abs_diff_pct)],
    diff_pct_at_max  = diff_pct[which.max(abs_diff_pct)],
    
    # Biggest count change and which level it was
    max_abs_diff_n = suppressWarnings(max(abs_diff_n, na.rm = TRUE)),
    level_max_n    = level[which.max(abs_diff_n)],
    diff_n_at_max  = diff_n[which.max(abs_diff_n)],
    
    # Did anything change at all?
    any_change = any(changed),
    .groups = "drop"
  ) %>%
  filter(any_change) %>%
  select(-any_change) %>%
  arrange(desc(max_abs_diff_pct), desc(max_abs_diff_n))

vars_changed_size

#Top movers by % (across all variables & grantees):
vars_changed_size %>% arrange(desc(max_abs_diff_pct)) %>% slice_head(n = 50)

vars_changed_size <- vars_changed_size %>% filter(max_abs_diff_pct > 0)

#Heatmap-ready wide table of max % change (variables as columns):
wide_max_pct <- vars_changed_size %>%
  select(grantee, variable, max_abs_diff_pct) %>%
  pivot_wider(names_from = variable, values_from = max_abs_diff_pct, values_fill = 0)

# Pivot longer for plotting
heatmap_df <- wide_max_pct %>%
  pivot_longer(-grantee, names_to = "variable", values_to = "max_abs_diff_pct")

heatmap_df <- heatmap_df %>% filter(max_abs_diff_pct > 0)

# Order grantees/variables if you like (optional)
heatmap_df <- heatmap_df %>%
  mutate(
    grantee  = factor(grantee, levels = unique(grantee)),
    variable = factor(variable, levels = unique(variable))
  )

# Plot heatmap
ggplot(heatmap_df, aes(x = variable, y = grantee, fill = max_abs_diff_pct)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    name = "Max % Change\n(pts)",
    low = "#F7FBFF", high = "#08306B"
  ) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Max % Change by Variable and Grantee (Old vs New, affected clients only)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# number of grantees per plot
n_per_page <- 30

# get ordered grantees (so plots are consistent)
grantee_levels <- levels(heatmap_df$grantee)
pages <- split(grantee_levels, ceiling(seq_along(grantee_levels) / n_per_page))

# plotting function
make_heat <- function(df_sub, page_idx) {
  ggplot(df_sub, aes(x = variable, y = grantee, fill = max_abs_diff_pct)) +
    geom_tile(color = "white") +
    scale_fill_gradient(
      name = "Max % Change\n(pts)",
      low = "#F7FBFF", high = "#08306B",
      limits = c(0, 100) 
    ) +
    labs(
      title = paste("Max % Change by Variable and Grantee (Old vs New, affected clients only) — Page", page_idx),
      x = NULL, y = NULL,
      caption = "Max % change (pts) = largest category shift between old and new datasets (percentage points)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# generate and display/save plots
plots <- walk2(pages, seq_along(pages), function(grs, i) {
  dfp <- heatmap_df %>%
    filter(grantee %in% grs) %>%
    mutate(grantee = forcats::fct_drop(forcats::fct_relevel(grantee, grs)))
  
  p <- make_heat(dfp, i)
  
  # show in RStudio Plots pane
  print(p)
  
  # save to file
  ggsave(sprintf("heatmap_page_%02d.png", i), p, width = 12, height = 8, dpi = 300)
})

