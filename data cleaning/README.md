# Data Cleaning — CYBHI EBP/CDEP Project

This folder contains all data cleaning scripts for the CYBHI Evidence-Based Practice / Community Development Education Program (EBP/CDEP) project. Each script reads raw exports from the eInsight clinical management platform, scores assessments, pairs initial and follow-up records, and writes clean CSV files for downstream analysis and reporting.

See `CHANGES.md` for the October 2026 review (bug fixes, new checks, and decisions still open).

------------------------------------------------------------------------

## Common Patterns

All scripts share the same general pipeline:

1.  **Configure** — Input folder, output folder, and the reporting window (`start_date`, default 2024-01-01; `end_date`, default today) are set once in the Setup chunk of each script. Set `end_date` to a fixed date to freeze the data window for the final report.
2.  **Read raw data** — CSV or Excel exports from eInsight, often with two-row hierarchical headers that are forward-filled and collapsed into clean column names via `zoo::na.locf()` and `janitor::make_clean_names()`. DEMO/test accounts are removed right after reading (every script).
3.  **Score** — Convert text responses to numeric (case- and whitespace-insensitive), apply reverse scoring where needed, compute total and subscale scores, flag or exclude records with too many missing items.
4.  **Pair assessments** — Group by `client_system_id`, assign the first record as *initial* and the last as *follow-up* (minimum 2 assessments required).
5.  **Compute outcomes** — Calculate score change and a `positive_outcome` indicator: `1` (improved), `0` (no change), `−1` (worsened). Direction of improvement varies by measure (see table below).
6.  **Write outputs** — Clean CSVs written to the shared Dropbox folder, filtered to the reporting window.

**Shared helpers.** Helper functions are copied verbatim into each script that uses them (keep them identical if you edit one):

- `read_assessment_csv()` — two-row header reader. "0" is a valid rating and is *not* treated as missing.
- `remove_demo_rows()` — removes rows whose `provider_id` or `client_system_id` contains "DEMO" and rows with no `client_system_id`; keeps and counts rows with a missing `provider_id` (assessment and report scripts; satisfaction has its own version that keeps rows without a client ID).
- `parse_einsight_date()` — accepts 2024-03-15 or 03/15/2024 (with or without a time) and warns with a count of values it cannot parse.
- `check_items()` — stops if the number of item columns differs from what positional scoring expects.
- `clean_item_scores()` — converts item scores to numeric and sets non-numeric or out-of-range values to `NA`, with a count.
- `map_responses()` / `audit_unmapped()` — maps text responses ignoring case and spacing, and lists any response that does not match.
- `item_rest_check()` — after reverse scoring, flags any item that correlates negatively with the items the instrument never reverse-scores (a sign its direction is wrong).

Every paired output includes `days_since_initial`, and each Quick Check reports how many pairs have a follow-up within 3 days of the initial.

------------------------------------------------------------------------

## Files

### `basis24_data_cleaning.qmd`

Cleans **BASIS-24** (Behavior and Symptom Identification Scale) data.

- **Input:** `Basis-24.csv`
- **Scoring:** 24 items (0–4); items 4–9 reverse-scored, but the eInsight export already reverses them, so the script does not (`basis24_reversed_in_export`); weighted total (range 0–4, higher = greater impairment); up to 1 missing item allowed.
- **Subscales:** Item mappings for Depression/Functioning, Relationships, Self-Harm, Emotional Lability, Psychosis, and Substance Abuse are documented in the script; subscale scores are **not** currently computed.
- **Output:** `basis24_paired_clean.csv`

------------------------------------------------------------------------

### `cpss5_data_cleaning.qmd`

Cleans **CPSS-5** (Child PTSD Symptom Scale, 5th Edition) in both interviewer-administered and self-report versions.

- **Inputs:** `CPSS-5-Interviewer.csv`, `CPSS-5-self.csv`
- **Scoring:** Sum of symptom items Q1–Q20 (the first 20 score columns; any later score columns such as the impairment items are excluded and listed) (range 0–80, higher = greater PTSD severity); total set to NA when ≥ 3 items are missing (`cpss_max_missing`); clinical cutoff ≥ 31.
- **Severity bands:** Minimal / Mild / Moderate / Severe / Very Severe; cutoff flags `initial_above_cutoff`, `follow_up_above_cutoff`.
- **Outputs:** `cpss_i_paired_clean.csv`, `cpss_s_paired_clean.csv`

------------------------------------------------------------------------

### `gad7_data_cleaning.qmd`

Cleans **GAD-7** (Generalized Anxiety Disorder-7) data.

- **Input:** `GAD-7.csv`
- **Scoring:** 7 items with text responses mapped to 0–3; total range 0–21 (higher = greater anxiety); ≥ 2 missing items → score set to NA. Item 8 (functional impairment) excluded from total.
- **Severity bands:** Minimal / Mild / Moderate / Severe.
- **Output:** `gad7_paired_clean.csv`

------------------------------------------------------------------------

### `phq9_data_cleaning.qmd`

Cleans **PHQ-9** (Patient Health Questionnaire-9) depression screening data.

- **Input:** `PHQ-9.csv`
- **Scoring:** 9 items with text responses mapped to 0–3; total range 0–27 (higher = greater depression); ≥ 2 missing items → score set to NA. Item 10 (functional impairment) excluded from total.
- **Severity bands:** Minimal / Mild / Moderate / Moderately Severe / Severe.
- **Output:** `phq9_paired_clean.csv`

------------------------------------------------------------------------

### `pss_data_cleaning.qmd`

Cleans **PSS** (Parental Stress Scale) data.

- **Input:** `PSS.csv`
- **Scoring:** 18 items with Likert responses mapped to 1–5; items 1, 2, 5, 6, 7, 8, 17, 18 reverse-scored (formula: `6 − score`); total range 18–90 (higher = greater parenting stress); ≥ 2 missing items → score set to NA. Contradictory forms (disagreeing on average with both the positive and the stress items) are flagged and, by default, dropped before pairing (`pss_drop_contradictory`).
- **Output:** `pss_paired_clean.csv`

------------------------------------------------------------------------

### `noms_data_cleaning.qmd`

Cleans **NOMS** (National Outcome Measures) data in child and self-report versions.

- **Inputs:** `NOMS-child.csv`, `NOMS-self_report.csv`
- **Scoring:** 21 items (Q1, 2a–2g, 3a–3g, 4a–4f) each scored 1–5, assumed coded so higher = better (confirm for the Q3 distress items; the item-rest check flags any that run the other way); output is **long format at the item level** (one row per client × item). Domain sums (Q2 functioning, Q3 well-being, Q4 social connectedness) are not computed here. Higher follow-up score = positive outcome (opposite direction from most other measures).
- **Pairing rule:** Follow-up must be more than 3 days after the initial assessment (`noms_min_gap_days`).
- **Outputs:** `noms_child_pairs_clean.csv`, `noms_self_pairs_clean.csv`

------------------------------------------------------------------------

### `sdq_data_cleaning.qmd`

Cleans **SDQ** (Strengths and Difficulties Questionnaire) data across four age/respondent versions.

- **Inputs:** `SDQ P2-4.csv`, `SDQ P4-10.csv`, `SDQ P11-17.csv`, `SDQ S11-17.csv`; plus `sdq-*-scoring.csv` lookup tables.
- **Scoring:** 25 items rated 0–2; prosocial items (1, 4, 9, 17, 20) excluded from total; items 7, 11, 14, 21, 25 reverse-scored, but the eInsight export already reverses them, so the script does not (`sdq_reverse_items_in_export`); total range 0–40 (higher = greater difficulties); ≥ 3 missing items → score set to NA. Externalizing subscale also computed (range 0–20), with bands `initial_ext_band` / `follow_up_ext_band`.
- **Cut-off bands:** Age- and version-specific (see script for complete tables).
- **Outputs:** `sdq_p24_pairs_clean.csv`, `sdq_p410_pairs_clean.csv`, `sdq_p1117_pairs_clean.csv`, `sdq_s1117_pairs_clean.csv`

------------------------------------------------------------------------

### `yoq_data_cleaning.qmd`

Cleans **YOQ** (Youth Outcome Questionnaire) data in caregiver and self-report versions, including Reliable Change Index (RCI) computation.

- **Inputs:** `YOQ-caregiver.csv`, `YOQ-self_report.csv`
- **Scoring:** 30 items rated 0–4; total range 0–120 (higher = greater distress); ≥ 3 missing items → score set to NA.
- **RCI:** `initial_score − follow_up_score`; thresholds: `reliably_better` (RCI ≥ 10), `reliably_worse` (RCI ≤ −10). Column order is kept stable (`rci` is column 7) because downstream code references it by position.
- **Outputs:** `yoq_caregiver_paired_clean.csv`, `yoq_self_paired_clean.csv`

------------------------------------------------------------------------

### `satisfaction_data_cleaning.qmd`

Cleans satisfaction survey data from four respondent groups and combines them into a single analysis-ready file.

- **Inputs:** `Satisfaction_survey-caregiver.csv`, `Satisfaction_survey-child_13-17.csv`, `Satisfaction_survey-adult_18-59.csv`, `Satisfaction_survey-adult_60_plus.csv`
- **Cleaning steps:** Removes DEMO accounts (rows with a missing `provider_id` are kept and counted); corrects malformed dates with erroneous "00" or "29" year prefixes; filters the combined file to the reporting window and reports how many rows were dropped for each reason.
- **Outputs:** Individual files per group (`satisfaction_caregiver_clean.csv`, etc.; demo rows removed, no date filter) and a combined primary output (`combined_satisfaction_clean.csv`) with `source`, `year`, and `quarter_label` columns.

------------------------------------------------------------------------

### `report_data_cleaning.Rmd`

Master cleaning script for program-level demographic and discharge/outcome data.

- **Inputs:** `<export_date>-discharge_report.xlsx` (one row per pre/post assessment pair) and `<export_date>-client_demographics.xlsx` (one row per client). Set `export_date` in the Setup chunk (currently `2026-10-08`).
- **Cleaning steps:**
  - Converts all ID columns to text (no scientific notation) so they compare and join consistently.
  - Removes DEMO accounts and rows with no `client_system_id`; keeps and counts rows with a missing `provider_id`.
  - Identifies and exports duplicate client records for manual review; conservatively drops all duplicates pending a resolution strategy.
  - Recodes race and ethnicity from comma-separated text to numeric codes using a codebook (including multiracial flags), splitting value by value; lists any value with no codebook entry.
  - Converts pre and post completion dates from Excel serial numbers or text dates; fixes 2-digit year errors.
  - Parses measure name strings (e.g., "Follow Up 1 PHQ-9", "Follow-Up 1 PHQ-9") into standardized time-point ("Initial" / "Follow Up N"), `post_follow_up_number`, and assessment-name columns.
  - Applies manual corrections for providers with missing funding round assignments.
  - Classifies clients with more than one complete pre–post pair (concurrent, sequential, multiple follow-ups).
  - Left-joins discharge data onto demographics (checked many-to-one) and reports unmatched rows.
- **Outputs:** `demographic_data_clean.csv`, `discharge_data_clean.csv`, `demographic_discharge_clean.csv`, `demographic_duplicate_list.csv`

------------------------------------------------------------------------

## Output Summary

| Assessment | Output File(s) | Score Range | Better = |
|----|----|----|----|
| BASIS-24 | `basis24_paired_clean.csv` | 0–4 | Lower |
| CPSS-5 | `cpss_i_paired_clean.csv`, `cpss_s_paired_clean.csv` | 0–80 | Lower |
| GAD-7 | `gad7_paired_clean.csv` | 0–21 | Lower |
| PHQ-9 | `phq9_paired_clean.csv` | 0–27 | Lower |
| PSS | `pss_paired_clean.csv` | 18–90 | Lower |
| NOMS | `noms_child_pairs_clean.csv`, `noms_self_pairs_clean.csv` | 1–5 per item | **Higher** |
| SDQ | `sdq_p24/p410/p1117/s1117_pairs_clean.csv` | 0–40 | Lower |
| YOQ | `yoq_caregiver_paired_clean.csv`, `yoq_self_paired_clean.csv` | 0–120 + RCI | Lower |
| Satisfaction | `combined_satisfaction_clean.csv` | — | — |
| Demographics & Outcomes | `demographic_discharge_clean.csv` | — | — |

Columns added in the October 2026 review are appended after the existing columns, so earlier column positions are unchanged.

------------------------------------------------------------------------

## Open Issues

- **Minimum follow-up gap:** NOMS enforces a \>3-day gap between initial and follow-up; other measures do not. All paired outputs now include `days_since_initial`, so a uniform rule can be applied downstream once agreed.
- **Missing data thresholds:** Thresholds vary by measure (0–3 allowable missing items) and totals are not prorated. Confirm these align with psychometric guidance for each tool. CPSS-5 now uses ≥ 3 missing → NA (previously no rule); confirm.
- **Pairing when a score is invalid:** If the first or last assessment has too many missing items, the pair has an NA outcome rather than falling back to the next valid assessment. Each Quick Check reports `n_missing_outcome`.
- **Duplicate client records:** `report_data_cleaning.Rmd` currently drops all duplicates conservatively. A resolution strategy for manual review is needed. Their discharge rows remain in the joined file with blank demographics.
- **Multiple interventions:** Some clients have concurrent or sequential interventions, or several follow-ups for one initial. A clear rule for the final outcome calculation is pending (`post_follow_up_number` supports a "last follow-up" rule).
- **Provider 573 funding round:** Reassigned to Round 2 on every row, not only "No Funding Round Selected" rows. Confirm.
- **Missing `provider_id`:** Rows with a missing provider ID are now kept (satisfaction and report scripts) instead of being dropped silently. Confirm.
- **YOQ RCI column reference:** The downstream analysis identifies the RCI by column position (column 7 = `rci`). Switch it to the column name.

------------------------------------------------------------------------

## Archive

The `archive/` subfolder contains earlier versions of cleaning scripts and exploratory R files. These are retained for reference but are not part of the active pipeline.
