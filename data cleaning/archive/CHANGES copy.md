# Change Log — Final-Report Decisions and Scoring Guides (October 29, 2026)

These changes apply the decisions recorded in the Action Items document (D0, D1, D2, D4, D5, D10, D11) and the published scoring guides now in the project files. All scripts were tested on synthetic mock files built like the eInsight exports; no real data were opened. Each script ran end to end. BASIS-24 and NOMS outputs were identical to the October 8 versions. For the other measures, rows and columns were unchanged, and the new totals matched an independent hand scoring of the raw mock files.

------------------------------------------------------------------------

## 1. Changes that can alter your numbers

| Script | Change | Why |
|----|----|----|
| All 8 assessment scripts | `end_date` is fixed at 2027-03-31. | D1. On interim exports this filters nothing. |
| CPSS-5 | 1–2 blank items are now prorated (sum × 20 ÷ answered, rounded); `NA` at 3 or more, as before. | D2 / D10: 10% default. |
| CPSS-5 interviewer | Severity bands now use the interviewer ranges (0–4 / 5–10 / 11–22 / 23–42 / 43–80). | The self-report ranges were applied to both versions; the CTSA summary publishes separate ranges. |
| GAD-7 | Complete forms only (any blank item → `NA`). Previously 1 blank was allowed and summed as 0. | D10: no published rule, and 10% of 7 items rounds down to 0. |
| PHQ-9 | 1–2 blank items prorated (sum × 9 ÷ answered, rounded); `NA` at 3 or more. Previously `NA` at 2. | D10: APA adapted PHQ-9 rule. |
| PSS | 1 blank item prorated (sum × 18 ÷ 17, rounded); `NA` at 2 or more, as before. | D10: 10% default. |
| SDQ (all four) | Each 5-item subscale is prorated when at least 3 items are answered, otherwise `NA`. Total = the four difficulty subscales; externalizing = conduct + hyperactivity; each is `NA` if any of its subscales is. | D10: published SDQ rule. Previously 3+ blank items anywhere among 20 made the total `NA`, with no prorating. |
| YOQ | Blank items take the rounded mean of the answered items; `NA` at 4 or more blank of 30. Previously `NA` at 3, with no fill-in. | D10: Y-OQ-30.2 manual. |
| Report | Rows whose intervention started before `start_date` or after `end_date` are removed; rows with no start are kept. | D1 (window on intervention start) and D0 (no start date needed to count as enrolled). |
| Report | A pair whose follow-up is after `end_date` is dropped when the same initial has a follow-up inside the window; otherwise the row is kept with its follow-up fields cleared. | D1: last follow-up before the cutoff; no pair if there is none, but the client stays enrolled. |
| Report | Duplicate demographic records are resolved to one row per client: fields that agree are kept, conflicting fields become `NA`. Previously every row for a duplicated client was dropped. | D5. Resolution runs on the cleaned race and ethnicity text, and the codes are derived afterwards, so the cleaned and coded columns stay consistent. |
| Report | Race and ethnicity nonresponse is kept: "Don't Know" = 97, "Prefer Not To Answer" = 98, "Not Applicable" = 99 in `code_num` / `ethn_num`, with standard labels in `cleaned_race` / `cleaned_ethn`. Nonresponse is still dropped when a race or ethnicity was also selected. `recoded_race` / `recoded_ethn` stay `NA` for nonresponse. | D11: follow the interim report. Re-run SQ 1.01, SQ 4.02, and the demographic tables (S3). |

## 2. New output and checks

- **Shared helpers** `score_total()`, `default_max_missing()`, and `round_half_up()` are byte-identical in the CPSS-5, GAD-7, PHQ-9, PSS, SDQ, and YOQ scripts. `score_total()` reports how many totals were filled in or set to `NA`, for the methods note (V1). GAD-7 and PHQ-9 no longer use `rowwise()`.
- **Report, Section 3.2:** clients with an intervention before `start_date` or after `end_date` and clients removed entirely (the D1 count), rows changed by the follow-up cutoff, and the list of follow-up fields cleared. `intervention_start` is parsed for this filter only and left as exported until Q5 is answered; unreadable values are reported in a warning.
- **Report, Section 2.4:** conflicting fields among duplicated clients, with counts, and the number of clients whose duplicate rows disagree on an ID column. Those clients' discharge rows cannot join; Section 5 counts them.
- **Report, Section 3.5:** providers still without a funding round after the fix (to send to Rikke, D4/Q4).
- **Report, Section 6:** "People with demographic data (de-duplicated)" and "People enrolled (intervention_name recorded)" replace the "served" and "outcomes" counts (D0).
- **Report, Sections 2.5–2.6:** the values-without-a-code tables now also list combinations of different nonresponse options, which get no code.

## 3. Documentation and cleanup

- **BASIS-24:** the weights and the missing-item rule already match the 2021 Instruction Guide; documented, no change.
- **NOMS:** the CYBHI NOMS Quick Guide confirms the Q3 coding (5 = None of the time), so higher = better for every item. Domain names and ranges now follow the guide. No change to the output.
- **YOQ:** the 30.2 manual has no reverse-scored items, which answers the YOQ half of Q1.
- **PSS:** removed the debugging lines that printed one client's raw rows (`client_system_id == 13175`) into the rendered HTML, and labeled the items-answered chunk.
- **Report:** Section 2 is reordered to clean the race and ethnicity text (2.3), resolve duplicates (2.4), then recode race (2.5) and ethnicity (2.6). Section 3 gains 3.2 Apply the Data Window, so the later subsections shift by one. "No Funding Round Selected" is defined once. Open Issues updated.

## 4. Still open

- **Satisfaction script:** not part of this update. Set `end_date <- as.Date("2027-03-31")` in its Setup chunk (D1).
- **GAD-7:** if complete-forms-only loses many forms, adopt the PHQ-9 rule (change the second argument of `score_total()` to `2`).
- **CPSS-5 interviewer cutoff:** ≥ 31 is published for the self-report version only.
- **Nonresponse code numbers:** 97–99 were chosen to sit outside both codebooks; confirm they suit the downstream scripts.
- **Demographics and the window:** the demographics file has no intervention date, so the final export's "people with demographic data" will include anyone added after `end_date`.
- **Paths:** the PSS script uses `/Users/cristin/Dropbox-CWS/...`; the others use `/Users/cristin/CWS Dropbox/...`.
- D3, D6–D9, D12–D15 and Q1–Q6 are unchanged; see the Action Items document.

------------------------------------------------------------------------

# Change Log — Data Cleaning Review (October 8, 2026)

All scripts were tested on synthetic mock files built to mimic the eInsight export layouts. No real data were opened. Each script was run end to end and rendered with Quarto / rmarkdown. Where logic was not meant to change, the new outputs matched the old ones cell for cell on the same mock inputs.

------------------------------------------------------------------------

## 1. Changes that can alter your numbers

Re-run these and compare paired N and outcome percentages against your last run.

| Script | Change | Why |
|----|----|----|
| All 8 assessment scripts | DEMO/test accounts and rows with no `client_system_id` are removed right after reading. | Only the satisfaction and report scripts removed DEMO accounts, so test clients were in every paired assessment output. Rows with no client ID could also pair as a single blank client. |
| CPSS-5 | Total now sums only the first 20 score columns (Q1–Q20). | The old code summed every `_score` column, including the 7 functional-impairment items, contrary to the documented scoring. |
| CPSS-5 | Total is `NA` when ≥ 3 of 20 items are missing (`cpss_max_missing <- 2`). | Previously there was no rule, so blanks counted as 0 and a fully blank form scored 0 ("Minimal"). **Decision needed** — see Section 5. |
| CPSS-5 | "0" is no longer read as missing. | The default missing-value list included "0", which inflated missing-item counts. |
| PSS | Contradictory forms (mean answer ≤ Disagree on both the positive and the stress items) are dropped before pairing by default (`pss_drop_contradictory <- TRUE`). | Found on the first real run: 31% of forms at one provider versus 0.6% elsewhere. The same provider's items 17 and 18 also run backwards; not corrected in code pending the provider's confirmation. |
| PSS | Responses matched ignoring case. | The map used "Strongly disagree"/"Strongly agree"; an export using "Strongly Disagree" scored every form as missing (all outcomes NA in testing). |
| BASIS-24 | Items 4–9 are no longer reversed by the script (`basis24_reversed_in_export <- 4:9`). | Found on the first real run: the export already reverses them, so they were being reversed twice. Earlier BASIS-24 totals counted them backwards (about 19% of the total weight). |
| SDQ (all four) | Items 7, 11, 14, 21, 25 are no longer reversed by the script (`sdq_reverse_items_in_export <- TRUE`). | Found on the first real run: eInsight exports their text already reversed, so they were being reversed twice. Earlier SDQ totals and externalizing scores counted them backwards. |
| GAD-7, PHQ-9, SDQ | Responses matched ignoring case. | Same exact-match risk, e.g., "Not At All". (Trailing spaces were already trimmed by `read_csv`.) |
| Report | Race/ethnicity split and cleaned value by value. | "White, Don't Know" was left as "White, " and coded multiracial (88); "Don't Know, White" became " White" with no code; a curly apostrophe (Don’t) was not removed. |
| Report | Time points matched case-insensitively and allow "Follow-Up"/"Followup"; standardized to "Initial" / "Follow Up N". | "Follow-Up 1" previously gave `NA` and stayed inside the assessment name. |
| Report | ID columns converted to text without scientific notation. | `as.character(100000)` gives "1e+05", which fails to join. |
| Satisfaction, Report | Rows with a missing `provider_id` are kept and counted. | The DEMO filter dropped them silently. **Decision needed.** |
| All item measures | Item values that are non-numeric or out of range are set to `NA` (with a count). | A stray code (e.g., 9) would otherwise be added into the total. |

## 2. Fixes for crashes and silent failures

- **Satisfaction:** the Quick Checks chunk called `filter()` without a data frame and stopped the render. Fixed.
- **Report:** `post_completion_date` did not get the text-date fallback or 2-digit-year fix that `pre_completion_date` gets, so any text date in the post column stopped the render. Both now use one helper (`parse_excel_date()`).
- **Dates (all assessment scripts):** CPSS expected 03/15/2024 while the others expected 2024-03-15. With US-format dates, the old GAD-7 script returned 0 rows without any error and the old SDQ script stopped. SDQ also filtered dates as text before converting them. All scripts now use `parse_einsight_date()`, which accepts either format and reports unparseable values.
- **Report:** only 2 of the 4 join keys were converted to text; a numeric/text mismatch would have stopped the join. All four are converted on read, and the join is checked many-to-one.

## 3. New checks (no effect on outputs)

- Item-count checks before positional scoring (BASIS-24 24, GAD-7 7, PHQ-9 9, PSS 18, YOQ 30, SDQ 20 + 10, CPSS ≥ 20, NOMS 21).
- Lists of text responses that don't match the scoring map.
- Reverse-scoring direction check (item-rest correlations) for BASIS-24, PSS, SDQ, YOQ, and NOMS. Where the instrument has reverse-scored items, each item is compared with the never-reversed items only, because a block of reversed items that agree with each other can otherwise look fine. On real data every item should be positive. A negative item means its direction is wrong, for example if eInsight already reverses BASIS-24 items 4–9. (On random mock data many items show small negatives; that is expected.)
- Quick Checks report pairs with a follow-up within 3 days of the initial.
- Satisfaction: counts of DEMO rows removed, rows kept with missing provider ID, year prefixes corrected, and rows dropped by reason (no valid date / before start / after end). The old "Missing dates" line always printed 0 because it ran after the filter.
- Satisfaction and report: list of provider IDs containing "demo" or "test" in any case that the "DEMO" rule did not remove.
- Report: race/ethnicity values with no codebook entry; measure strings with no recognizable time point; funding rounds before and after the manual fix; unmatched discharge rows (and whether they are dropped duplicates); columns duplicated by the join (`.x`/`.y`).
- Report: Section 4.4 classifies every client with more than one complete pair as concurrent / sequential / multiple follow-ups. This resolves the open issue about the counts not adding up. The concurrent table had one row per client × date, the groups overlap, and one initial paired with several follow-ups fit neither group.

## 4. Output columns added

All new columns are appended after the existing ones, so earlier column positions are unchanged (YOQ `rci` is still column 7).

| Output | New columns |
|----|----|
| BASIS-24, PSS, YOQ (both) | `days_since_initial` |
| CPSS-5 (both) | `initial_above_cutoff`, `follow_up_above_cutoff`, `days_since_initial` |
| SDQ (all four) | `initial_ext_band`, `follow_up_ext_band` |
| PSS | `initial_contradictory`, `follow_up_contradictory` (both `FALSE` when contradictory forms are dropped) |
| NOMS (both) | `room_to_improve` (FALSE when the initial score is already 5) |
| `discharge_data_clean.csv` / `demographic_discharge_clean.csv` | `post_follow_up_number` |

GAD-7 and PHQ-9 outputs have the same columns as before.

## 5. Decisions still open

1. **CPSS-5 missing-item rule.** Now ≥ 3 missing → `NA`. Change `cpss_max_missing` in the Setup chunk if you prefer another rule.
2. **Rows with a missing `provider_id`.** Now kept and counted. If they should be excluded, add an explicit filter.
3. **Provider 573 funding round.** Reassigned to Round 2 on every row, unlike the other four providers. The new before/after table shows what this changes.
4. **Pairing when the first or last score is invalid.** Unchanged: the pair gets an `NA` outcome rather than falling back to the next valid assessment.
5. **Race/ethnicity nonresponse.** "Don't Know / Prefer Not To Answer / Not Applicable" are stripped, while the interim report treats valid nonresponses as categories. Confirm downstream scripts use the raw columns for those.
6. **SDQ externalizing bands** are not part of the standard published SDQ banding; confirm the source.

## 6. Housekeeping

- Paths and the reporting window (`start_date`, `end_date`) are set once in each Setup chunk. Set `end_date` to a fixed date to freeze the final-report window. The report script also has an `export_date` setting.
- Shared helpers are byte-identical in every script that uses them.
- Repeated blocks were folded into small functions where versions were identical (NOMS child/self, SDQ versions, CPSS/YOQ summaries).
- YAML: `date: today`; `embed-resources` replaces deprecated `self-contained`; removed `toc-float`, which is not a Quarto option. In the .Rmd, `self-contained` → `self_contained` (the hyphenated form was silently ignored).
- Removed unused `library(gt)` and `library(openxlsx)` from the report script, plus unused objects (`row_id`, NOMS `total_score`/`any_missing_q1x`, the commented-out 2025-12-31 filters).
- Documentation fixes: BASIS-24 subscales are documented but not computed; NOMS output is item-level; NOMS gap is "more than 3 days"; PSS is the Parental Stress Scale; the stale CPSS note about Excel inputs was removed.
