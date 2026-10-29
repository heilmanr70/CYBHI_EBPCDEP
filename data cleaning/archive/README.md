# Data Cleaning — CYBHI EBP/CDEP Project

This folder contains all data cleaning scripts for the CYBHI Evidence-Based Practice / Community Development Education Program (EBP/CDEP) project. Each script reads raw exports from the eInsight clinical management platform, scores assessments, pairs initial and follow-up records, and writes clean CSV files for downstream analysis and reporting.

------------------------------------------------------------------------

## Common Patterns

All scripts share the same general pipeline:

1.  **Read raw data** — CSV or Excel exports from eInsight, often with two-row hierarchical headers that are forward-filled and collapsed into clean column names via `zoo::na.locf()` and `janitor::make_clean_names()`.
2.  **Score** — Convert text responses to numeric, apply reverse scoring where needed, compute total and subscale scores, flag or exclude records with too many missing items.
3.  **Pair assessments** — Group by `client_system_id`, assign the first record as *initial* and the last as *follow-up* (minimum 2 assessments required).
4.  **Compute outcomes** — Calculate score change and an `outcome` indicator: `1` (improved), `0` (no change), `−1` (worsened). Direction of improvement varies by measure (see table below).
5.  **Write outputs** — Clean CSVs written to the shared Dropbox folder. Data are filtered to 2024-01-01 onwards.

------------------------------------------------------------------------

## Files

### `basis24_data_cleaning.qmd`

Cleans **BASIS-24** (Behavior and Symptom Identification Scale) data.

- **Input:** `Basis-24.csv`
- **Scoring:** 24 items (0–4); items 4–9 reverse-scored; weighted total (range 0–4, higher = greater impairment); up to 1 missing item allowed.
- **Subscales:** Depression/Functioning, Relationships, Self-Harm, Emotional Lability, Psychosis, Substance Abuse.
- **Output:** `basis24_paired_clean.csv`

------------------------------------------------------------------------

### `cpss5_data_cleaning.qmd`

Cleans **CPSS-5** (Child PTSD Symptom Scale, 5th Edition) in both interviewer-administered and self-report versions.

- **Inputs:** `CPSS-5-Interviewer.csv`, `CPSS-5-self.csv`
- **Scoring:** Sum of items Q1–Q20 (range 0–80, higher = greater PTSD severity); clinical cutoff ≥ 31.
- **Severity bands:** Minimal / Mild / Moderate / Severe / Very Severe.
- **Outputs:** `cpss_i_paired_clean.csv`, `cpss_s_paired_clean.csv`

------------------------------------------------------------------------

### `gad7_data_cleaning.qmd`

Cleans **GAD-7** (Generalized Anxiety Disorder-7) data.

- **Input:** `GAD-7.csv`
- **Scoring:** 7 items with text responses mapped to 0–3; total range 0–21 (higher = greater anxiety); ≥ 2 missing items → score set to NA. Item 8 (functional impairment) excluded from total.
- **Severity bands:** Minimal / Mild / Moderate / Severe.
- **Additional metric:** `days_since_initial` (days between initial and follow-up assessments).
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

Cleans **PSS** (Parenting Stress Scale) data.

- **Input:** `PSS.csv`
- **Scoring:** 18 items with Likert responses mapped to 1–5; items 1, 2, 5, 6, 7, 8, 17, 18 reverse-scored (formula: `6 − score`); total range 18–90 (higher = greater parenting stress); ≥ 2 missing items → score set to NA.
- **Output:** `pss_paired_clean.csv`

------------------------------------------------------------------------

### `noms_data_cleaning.qmd`

Cleans **NOMS** (National Outcome Measures) data in child and self-report versions.

- **Inputs:** `NOMS-child.csv`, `NOMS-self_report.csv`
- **Scoring:** Four question groups (Q1–Q4) scored on 1–5 scales; output is **long format** (one row per client × question). Higher follow-up score = positive outcome (opposite direction from most other measures).
- **Pairing rule:** Follow-up must be at least 3 days after the initial assessment.
- **Outputs:** `noms_child_pairs_clean.csv`, `noms_self_pairs_clean.csv`

------------------------------------------------------------------------

### `sdq_data_cleaning.qmd`

Cleans **SDQ** (Strengths and Difficulties Questionnaire) data across four age/respondent versions.

- **Inputs:** `SDQ P2-4.csv`, `SDQ P4-10.csv`, `SDQ P11-17.csv`, `SDQ S11-17.csv`; plus `sdq-*-scoring.csv` lookup tables.
- **Scoring:** 25 items rated 0–2; prosocial items (1, 4, 9, 17, 20) excluded from total; items 7, 11, 14, 21, 25 reverse-scored; total range 0–40 (higher = greater difficulties); ≥ 3 missing items → score set to NA. Externalizing subscale also computed (range 0–20).
- **Cut-off bands:** Age- and version-specific (see script for complete tables).
- **Outputs:** `sdq_p24_pairs_clean.csv`, `sdq_p410_pairs_clean.csv`, `sdq_p1117_pairs_clean.csv`, `sdq_s1117_pairs_clean.csv`

------------------------------------------------------------------------

### `yoq_data_cleaning.qmd`

Cleans **YOQ** (Youth Outcome Questionnaire) data in caregiver and self-report versions, including Reliable Change Index (RCI) computation.

- **Inputs:** `YOQ-caregiver.csv`, `YOQ-self_report.csv`
- **Scoring:** 30 items rated 0–4; total range 0–120 (higher = greater distress); ≥ 3 missing items → score set to NA.
- **RCI:** `initial_score − follow_up_score`; thresholds: `reliably_better` (RCI ≥ 10), `reliably_worse` (RCI ≤ −10).
- **Outputs:** `yoq_caregiver_paired_clean.csv`, `yoq_self_paired_clean.csv`

------------------------------------------------------------------------

### `satisfaction_data_cleaning.qmd`

Cleans satisfaction survey data from four respondent groups and combines them into a single analysis-ready file.

- **Inputs:** `Satisfaction_survey-caregiver.csv`, `Satisfaction_survey-child_13-17.csv`, `Satisfaction_survey-adult_18-59.csv`, `Satisfaction_survey-adult_60_plus.csv`
- **Cleaning steps:** Removes DEMO accounts; corrects malformed dates with erroneous "00" or "29" year prefixes; filters to 2024-01-01 onwards.
- **Outputs:** Individual files per group (`satisfaction_caregiver_clean.csv`, etc.) and a combined primary output (`combined_satisfaction_clean.csv`) with `source`, `year`, and `quarter_label` columns.

------------------------------------------------------------------------

### `report_data_cleaning.Rmd`

Master cleaning script for program-level demographic and discharge/outcome data.

- **Inputs:** `2026-04-21-discharge_report.xlsx` (one row per pre/post assessment pair), `2026-04-21-client_demographics.xlsx` (one row per client).
- **Cleaning steps:**
  - Removes DEMO accounts from both files.
  - Identifies and exports duplicate client records for manual review; conservatively drops all duplicates pending a resolution strategy.
  - Recodes race and ethnicity from free text to numeric codes using a codebook (including multiracial flags).
  - Converts Excel date serial numbers; fixes 2-digit year errors.
  - Parses measure name strings (e.g., "Follow Up 1 PHQ-9") into structured time-point and assessment-name columns.
  - Applies manual corrections for providers with missing funding round assignments.
  - Left-joins discharge data onto demographics.
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
| NOMS | `noms_child_pairs_clean.csv`, `noms_self_pairs_clean.csv` | 1–5 per question | **Higher** |
| SDQ | `sdq_p24/p410/p1117/s1117_pairs_clean.csv` | 0–40 | Lower |
| YOQ | `yoq_caregiver_paired_clean.csv`, `yoq_self_paired_clean.csv` | 0–120 + RCI | Lower |
| Satisfaction | `combined_satisfaction_clean.csv` | — | — |
| Demographics & Outcomes | `demographic_discharge_clean.csv` | — | — |

------------------------------------------------------------------------

## Open Issues

- **Minimum follow-up gap:** NOMS enforces a \>3-day gap between initial and follow-up; other measures do not. Confirm whether a minimum gap should apply uniformly.
- **Missing data thresholds:** Thresholds vary by measure (0–3 allowable missing items). Confirm these align with psychometric guidance for each tool.
- **Duplicate client records:** `report_data_cleaning.Rmd` currently drops all duplicates conservatively. A resolution strategy for manual review is needed.
- **Multiple interventions:** Some clients have concurrent or sequential interventions. A clear rule for the final outcome calculation is pending.
- **YOQ RCI column reference:** Currently identified by column position rather than name; may break if the export structure changes.

------------------------------------------------------------------------

## Archive

The `archive/` subfolder contains earlier versions of cleaning scripts and exploratory R files. These are retained for reference but are not part of the active pipeline.
