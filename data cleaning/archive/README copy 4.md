# Data Cleaning — CYBHI EBP/CDEP Project

This folder contains all data cleaning scripts for the CYBHI Evidence-Based Practice / Community Development Education Program (EBP/CDEP) project. Each script reads raw exports from the eInsight clinical management platform, scores assessments, pairs initial and follow-up records, and writes clean CSV files for downstream analysis and reporting.

See `CHANGES.md` for the October 2026 reviews (the October 8 bug fixes and new checks; the October 29 changes that apply the final-report decisions and the published scoring guides) and the November 2 rebuild of the satisfaction script.

------------------------------------------------------------------------

## Common Patterns

All scripts share the same general pipeline:

1.  **Configure** — Input folder, output folder, and the reporting window are set once in the Setup chunk of each script. The window is fixed for the final report (D1): `start_date` 2024-01-01 and `end_date` 2027-03-31. On interim exports `end_date` filters nothing.
2.  **Read raw data** — CSV or Excel exports from eInsight, often with two-row hierarchical headers that are forward-filled and collapsed into clean column names via `zoo::na.locf()` and `janitor::make_clean_names()`. DEMO/test accounts are removed right after reading (every script).
3.  **Score** — Convert text responses to numeric (case- and whitespace-insensitive), apply reverse scoring where needed, and compute total and subscale scores. **Missing items (D10):** each measure follows its published rule where one exists; otherwise up to 10% of items may be blank and the total is prorated. Filled-in totals are rounded half up to whole numbers. See the table under Missing-Item Policy.
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
- `score_total()`, `default_max_missing()`, `round_half_up()` — apply the missing-item policy: a total is `NA` when more than `max_missing` items are blank, and otherwise blanks are filled in by prorating (`"prorate"`), by the rounded mean of the answered items (`"item_mean"`), or counted as 0 (`"zero"`, PSC-35). A message reports how many totals were filled in or set to `NA` (CATS, CPSS-5, GAD-7, PHQ-9, P-OQ, PSC-35, PSS, SDQ, YOQ).
- `scale_item_cols()` — finds the item columns by their answers: the `_response` columns where most answers are on the instrument's scale, so Yes/No and free-text questions on the same form drop out. Stops and lists every response column if the count is wrong (CATS, PSC-35).

Every paired output includes `days_since_initial`, and each Quick Check reports how many pairs have a follow-up within 3 days of the initial.

### Missing-Item Policy (D10)

| Measure | Source | Rule |
|----|----|----|
| BASIS-24 | BASIS-24 Instruction Guide (2021) | Up to 1 of 24 blank; weighted score divided by (1 − weight of the blank item). Unchanged. |
| CPSS-5 | 10% default (no published rule) | Up to 2 of 20 blank, prorated. |
| GAD-7 | 10% default (no published rule) | No blank items (10% of 7 rounds down to 0): complete forms only. |
| PHQ-9 | APA adapted PHQ-9 | Up to 2 of 9 blank, prorated; not scored at 3 or more. |
| PSS | 10% default (no rule in the scoring guide) | Up to 1 of 18 blank, prorated. |
| SDQ | SDQ scoring sheets (2015, 2016) | Each 5-item subscale prorated when at least 3 items are answered; total and externalizing are `NA` if any subscale in them is `NA`. |
| Y-OQ 30.2 | Y-OQ-30.2 Administration and Scoring Manual | Each blank item takes the rounded mean of the answered items; `NA` when more than 10% (4 or more of 30) are blank. |
| NOMS | — | Output is item-level, so no total is scored here. |
| PSC-35 | Scoring the PSC (MGH) | 1–3 blank items count as 0 (not prorated); invalid at 4 or more. Same rule for the 31-item (ages 3–5) score. |
| CATS-2 (7–17) and CATS (3–6) | 10% default (no published rule) | Up to 2 of 20 items (7–17) or 1 of 16 (3–6) blank, prorated. A split item (9, 10, 15) is blank only when all its sub-items are. |
| P-OQ | P-OQ Administration and Scoring Manual | 1–2 blank items take the rounded mean of the answered items; `NA` at 3 or more. |

------------------------------------------------------------------------

## Files

### `basis24_data_cleaning.qmd`

Cleans **BASIS-24** (Behavior and Symptom Identification Scale) data.

- **Input:** `Basis-24.csv`
- **Scoring:** 24 items (0–4); items 4–9 reverse-scored, but the eInsight export already reverses them, so the script does not (`basis24_reversed_in_export`); weighted total (range 0–4, higher = greater impairment); up to 1 missing item allowed, which matches the published rule.
- **Subscales:** Item mappings for Depression/Functioning, Relationships, Self-Harm, Emotional Lability, Psychosis, and Substance Abuse are documented in the script; subscale scores are **not** currently computed.
- **Output:** `basis24_paired_clean.csv`

------------------------------------------------------------------------

### `cpss5_data_cleaning.qmd`

Cleans **CPSS-5** (Child PTSD Symptom Scale, 5th Edition) in both interviewer-administered and self-report versions.

- **Inputs:** `CPSS-5-Interviewer.csv`, `CPSS-5-self.csv`
- **Scoring:** Sum of symptom items Q1–Q20 (the first 20 score columns; any later score columns such as the impairment items are excluded and listed) (range 0–80, higher = greater PTSD severity); up to 2 blank items prorated, `NA` at 3 or more; clinical cutoff ≥ 31 (published for the self-report version).
- **Severity bands:** Minimal / Mild / Moderate / Severe / Very Severe, with the published ranges for each version (interviewer 0–4 / 5–10 / 11–22 / 23–42 / 43–80; self-report 0–10 / 11–20 / 21–40 / 41–60 / 61–80); cutoff flags `initial_above_cutoff`, `follow_up_above_cutoff`.
- **Outputs:** `cpss_i_paired_clean.csv`, `cpss_s_paired_clean.csv`

------------------------------------------------------------------------

### `gad7_data_cleaning.qmd`

Cleans **GAD-7** (Generalized Anxiety Disorder-7) data.

- **Input:** `GAD-7.csv`
- **Scoring:** 7 items with text responses mapped to 0–3; total range 0–21 (higher = greater anxiety); complete forms only under the 10% default. Item 8 (functional impairment) excluded from total.
- **Severity bands:** Minimal / Mild / Moderate / Severe.
- **Output:** `gad7_paired_clean.csv`

------------------------------------------------------------------------

### `phq9_data_cleaning.qmd`

Cleans **PHQ-9** (Patient Health Questionnaire-9) depression screening data.

- **Input:** `PHQ-9.csv`
- **Scoring:** 9 items with text responses mapped to 0–3; total range 0–27 (higher = greater depression); 1–2 blank items prorated, `NA` at 3 or more (APA rule). Item 10 (functional impairment) excluded from total.
- **Severity bands:** Minimal / Mild / Moderate / Moderately Severe / Severe.
- **Output:** `phq9_paired_clean.csv`

------------------------------------------------------------------------

### `pss_data_cleaning.qmd`

Cleans **PSS** (Parental Stress Scale) data.

- **Input:** `PSS.csv`
- **Scoring:** 18 items with Likert responses mapped to 1–5; items 1, 2, 5, 6, 7, 8, 17, 18 reverse-scored (formula: `6 − score`); total range 18–90 (higher = greater parenting stress); 1 blank item prorated, `NA` at 2 or more. Contradictory forms (disagreeing on average with both the positive and the stress items) are flagged and, by default, dropped before pairing (`pss_drop_contradictory`).
- **Output:** `pss_paired_clean.csv`

------------------------------------------------------------------------

### `noms_data_cleaning.qmd`

Cleans **NOMS** (National Outcome Measures) data in child and self-report versions.

- **Inputs:** `NOMS-child.csv`, `NOMS-self_report.csv`
- **Scoring:** 21 items (Q1, 2a–2g, 3a–3g, 4a–4f) each scored 1–5, coded so higher = better (the CYBHI NOMS Quick Guide codes Q3 as 5 = None of the time; the item-rest check verifies the export); output is **long format at the item level** (one row per client × item). The guide's four subscales (Mental Health 1–5, Handling Daily Life 7–35, General Well-Being 7–35, Social Connectedness 6–30) are sums of their items and are not computed here. Higher follow-up score = positive outcome (opposite direction from most other measures).
- **Pairing rule:** Follow-up must be more than 3 days after the initial assessment (`noms_min_gap_days`).
- **Outputs:** `noms_child_pairs_clean.csv`, `noms_self_pairs_clean.csv`

------------------------------------------------------------------------

### `sdq_data_cleaning.qmd`

Cleans **SDQ** (Strengths and Difficulties Questionnaire) data across four age/respondent versions.

- **Inputs:** `SDQ P2-4.csv`, `SDQ P4-10.csv`, `SDQ P11-17.csv`, `SDQ S11-17.csv`; plus `sdq-*-scoring.csv` lookup tables.
- **Scoring:** 25 items rated 0–2; prosocial items (1, 4, 9, 17, 20) excluded from total; items 7, 11, 14, 21, 25 reverse-scored, but the eInsight export already reverses them, so the script does not (`sdq_reverse_items_in_export`); four difficulty subscales, each prorated when at least 3 of 5 items are answered; total = sum of the subscales, range 0–40 (higher = greater difficulties). Externalizing = conduct + hyperactivity (range 0–20), with bands `initial_ext_band` / `follow_up_ext_band`.
- **Cut-off bands:** Age- and version-specific (see script for complete tables).
- **Outputs:** `sdq_p24_pairs_clean.csv`, `sdq_p410_pairs_clean.csv`, `sdq_p1117_pairs_clean.csv`, `sdq_s1117_pairs_clean.csv`

------------------------------------------------------------------------

### `yoq_data_cleaning.qmd`

Cleans **YOQ** (Youth Outcome Questionnaire) data in caregiver and self-report versions, including Reliable Change Index (RCI) computation.

- **Inputs:** `YOQ-caregiver.csv`, `YOQ-self_report.csv`
- **Scoring (Y-OQ-30.2 manual):** 30 items rated 0–4, summed with no reverse scoring; total range 0–120 (higher = greater distress); each blank item takes the rounded mean of the answered items, and the total is `NA` at 4 or more blank.
- **RCI:** `initial_score − follow_up_score`; thresholds from the manual: `reliably_better` (RCI ≥ 10), `reliably_worse` (RCI ≤ −10). Column order is kept stable (`rci` is column 7) because downstream code references it by position.
- **Outputs:** `yoq_caregiver_paired_clean.csv`, `yoq_self_paired_clean.csv`

------------------------------------------------------------------------

### `psc35_data_cleaning.qmd`

Cleans **PSC-35** (Pediatric Symptom Checklist, parent form) data (SQ 3.13).

- **Input:** `PSC-35.csv` (set `psc_file` in Setup)
- **Scoring:** 35 items, Never = 0 / Sometimes = 1 / Often = 2; total range 0–70 (higher = more impairment); 1–3 blank items count as 0, invalid at 4 or more. Cutoff ≥ 28 (ages 6–17). Ages 3–5 ignore items 5, 6, 17, 18 (31 items) with cutoff ≥ 24; the export has no age, so both scores are kept (`*_3_5` columns) and the version is chosen downstream by age at intervention start.
- **Output:** `psc35_paired_clean.csv`

------------------------------------------------------------------------

### `cats_data_cleaning.qmd`

Cleans **CATS** (Child and Adolescent Trauma Screen) data in three versions (SQ 3.15).

- **Inputs:** `CATS-self.csv`, `CATS-caregiver_7-17.csv`, `CATS-caregiver_3-6.csv` (set `cats_files` in Setup)
- **Scoring:** symptom items only (trauma events and impairment are Yes/No and drop out). CATS-2, 7–17 (self and caregiver): 25 rows rated 0–3, items 9, 10, 15 counted at their highest sub-item, giving 20 items and a DSM-5 PTSD sum of 0–60; bands Normal < 15, Moderate 15–20, Elevated 21–24, Probable PTSD ≥ 25; positive screen ≥ 21. CATS, 3–6 (caregiver): 16 items rated 0–3, total 0–48; clinical ≥ 12. Up to 10% blank, prorated. Higher = more distress.
- **Outputs:** `cats_self_7_17_paired_clean.csv`, `cats_caregiver_7_17_paired_clean.csv`, `cats_caregiver_3_6_paired_clean.csv`

------------------------------------------------------------------------

### `poq_data_cleaning.qmd`

Cleans **P-OQ** (Pre-School Outcome Questionnaire, caregiver form) data, including the Reliable Change Index (SQ 3.13, with the Y-OQ).

- **Input:** `P-OQ.csv` (set `poq_file` in Setup); single-row header like the Y-OQ export.
- **Scoring:** 35 items; problem items 1–4, positive items 1, 2, 12, 21, 25, 26, 35 scored 2 / 1 / −1 / −2; total range 14–126 (higher = more distress). The export is assumed to hold the form's printed scores (`poq_export_has_form_scores`); the read stops if the positive items show otherwise. 1–2 blank items take the rounded mean of the answered items; `NA` at 3 or more.
- **RCI:** `initial_score − follow_up_score`; `reliably_better` (RCI ≥ 7), `reliably_worse` (RCI ≤ −7), as in the manual. Same columns as the YOQ outputs.
- **Output:** `poq_paired_clean.csv`

------------------------------------------------------------------------

### `satisfaction_data_cleaning.qmd`

Cleans satisfaction survey data from four respondent groups and combines them into a single analysis-ready file.

- **Inputs:** `Satisfaction_survey-caregiver.csv`, `Satisfaction_survey-child_13-17.csv`, `Satisfaction_survey-adult_18-59.csv`, `Satisfaction_survey-adult_60_plus.csv`
- **Cleaning steps:** Removes DEMO accounts (rows with a missing `provider_id` are kept and counted); corrects malformed dates with erroneous "00" or "29" year prefixes; filters the combined file to the reporting window and reports how many rows were dropped for each reason. The reporting window matches the other scripts: 2024-01-01 through 2027-03-31 (D1). Rows with no `client_system_id` are kept, since a survey response without a client ID is still valid.
- **Outputs:** Individual files per group (`satisfaction_caregiver_clean.csv`, etc.; demo rows removed, no date filter) and a combined primary output (`combined_satisfaction_clean.csv`) with `source`, `year`, and `quarter_label` columns.

------------------------------------------------------------------------

### `report_data_cleaning.Rmd`

Master cleaning script for program-level demographic and discharge/outcome data.

- **Inputs:** `<export_date>-discharge_report.xlsx` (one row per pre/post assessment pair) and `<export_date>-client_demographics.xlsx` (one row per client). Set `export_date` in the Setup chunk (currently `2026-10-08`).
- **Cleaning steps:**
  - Converts all ID columns to text (no scientific notation) so they compare and join consistently.
  - Removes DEMO accounts and rows with no `client_system_id`; keeps and counts rows with a missing `provider_id`.
  - Identifies and exports duplicate client records for manual review, then resolves each duplicated client to one row: fields that agree are kept and conflicting fields are set to `NA` (D5).
  - Recodes race and ethnicity from comma-separated text to numeric codes using a codebook (including multiracial flags), splitting value by value. Nonresponse is kept as separate codes (D11): 97 Don't Know, 98 Prefer Not To Answer, 99 Not Applicable. Lists any value with no code.
  - Applies the data window to intervention start (rows with no start are kept) and cuts follow-ups after `end_date` back to the last follow-up before it (D1). Prints how many clients the window removes.
  - Converts pre and post completion dates from Excel serial numbers or text dates; fixes 2-digit year errors.
  - Parses measure name strings (e.g., "Follow Up 1 PHQ-9", "Follow-Up 1 PHQ-9") into standardized time-point ("Initial" / "Follow Up N"), `post_follow_up_number`, and assessment-name columns.
  - Applies manual corrections for providers with missing funding round assignments and lists providers still without a round.
  - Classifies clients with more than one complete pre–post pair (concurrent, sequential, multiple follow-ups).
  - Left-joins discharge data onto demographics (checked many-to-one) and reports unmatched rows.
  - Reports "people with demographic data" (de-duplicated demographics) and "people enrolled" (an `intervention_name` in the discharge file) (D0).
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
| PSC-35 | `psc35_paired_clean.csv` | 0–70 (31-item: 0–62) | Lower |
| CATS | `cats_self_7_17/caregiver_7_17/caregiver_3_6_paired_clean.csv` | 0–60 (3–6: 0–48) | Lower |
| P-OQ | `poq_paired_clean.csv` | 14–126 + RCI | Lower |
| Satisfaction | `combined_satisfaction_clean.csv` | — | — |
| Demographics & Outcomes | `demographic_discharge_clean.csv` | — | — |

Columns added in the October 2026 review are appended after the existing columns, so earlier column positions are unchanged.

------------------------------------------------------------------------

## Open Issues

- **Minimum follow-up gap:** NOMS enforces a \>3-day gap between initial and follow-up; other measures do not. All paired outputs now include `days_since_initial`, so a uniform rule can be applied downstream once agreed.
- **GAD-7 missing items:** the 10% default allows no blank items on a 7-item scale. If too many forms are lost, adopt the PHQ-9 rule (prorate up to 2 blank).
- **CPSS-5 interviewer cutoff:** the ≥ 31 cutoff is published for the self-report version; confirm whether the interviewer flags should use it.
- **Pairing when a score is invalid:** If the first or last assessment has too many missing items, the pair has an NA outcome rather than falling back to the next valid assessment. Each Quick Check reports `n_missing_outcome`.
- **Duplicate client records with conflicting IDs:** clients whose duplicate demographic rows disagree on an ID column get `NA` for that ID, so their discharge rows do not join. The report script counts them.
- **Demographics and the data window:** the demographics file has no intervention date, so on the final export "people with demographic data" includes anyone added after `end_date`.
- **Multiple interventions:** Some clients have concurrent or sequential interventions, or several follow-ups for one initial. A clear rule for the final outcome calculation is pending (`post_follow_up_number` supports a "last follow-up" rule).
- **Missing `provider_id`:** Rows with a missing provider ID are now kept (satisfaction and report scripts) instead of being dropped silently. Confirm.
- **PSC-35, CATS, P-OQ export layouts:** built from the paper forms and the other eInsight exports before any real export was seen. The first real run confirms the file names and the item columns (each read stops if the count is wrong).
- **PSC-35 age version:** choose the 35-item or 31-item score by age at intervention start.
- **YOQ RCI column reference:** The downstream analysis identifies the RCI by column position (column 7 = `rci`). Switch it to the column name.

------------------------------------------------------------------------

## Archive

The `archive/` subfolder contains earlier versions of cleaning scripts and exploratory R files. These are retained for reference but are not part of the active pipeline.
