# Quarterly report — DHCS-branded, no copy/paste

## Files

- **`2026-Q2-quarterly-report.qmd`** — replaces your old results file. Computes
  everything your original file did, and renders straight to a finished,
  DHCS-branded `.docx`. Nothing to paste into the template anymore — the
  rendered file *is* the report.
- **`dhcs-reference-doc.docx`** — the style template Quarto reads to brand the
  output (fonts, colors, logo, headings, table look). Must stay in the same
  folder as the `.qmd`. You won't open or edit this directly.
- **`dhcs-style-preview-SAMPLE-DATA.docx`** — a quick preview of the styling
  (headings, callout box, branded table, stat highlight) built from placeholder
  numbers, not your real data. Open this first to see what the formatting
  looks like before running the full thing against real data.

## Setup (one time)

```r
install.packages(c("flextable", "officer", "scales"))
```
(readr, tidyverse, haven, janitor — same as before.)

## Each quarter

Edit only the `params:` block at the top of the `.qmd`:

```yaml
params:
  quarter_label: "2026 Q2"
  period_start: "2024-06-01"
  period_end: "2026-06-30"
  date_downloaded: "2026-07-17"
  suppression_threshold: 11
  discharge_data_path: "..."
  demo_data_path: "..."
  satisfaction_data_path: "..."
```

Then **Render**. Everything else in the file is generic and shouldn't need
touching quarter to quarter.

## What changed vs. your original file

- **Output format**: HTML analysis notebook → branded `.docx`, using
  `format: docx: reference-doc: dhcs-reference-doc.docx`. This is the
  standard Quarto/Pandoc mechanism for making rendered Word docs match a
  corporate template — Quarto reads the fonts/colors/heading styles/logo
  from that reference file.
- **Tables**: raw `tabyl()` printouts → `flextable`-based tables run through
  one `dhcs_table()` helper (DHCS Blue header, white bold text, banded rows,
  Segoe UI). Change the look once, in one place, and it updates everywhere.
- **Small-cell suppression**: your `TO DO` note ("anything <11 needs to be
  suppressed... suppress the total too if only one item is suppressed") is
  now actual code — `suppress_labeled_counts()` for single-variable tables,
  `dhcs_crosstab()` for the two-way tables (outcomes/discharge/client type by
  round).
- **Numbers you used to copy/paste** (the `pct_4_5`-style inline stats) are
  now used throughout the prose the same way your original already did it in
  one spot — just extended everywhere a number needed to land in a sentence.
- **File paths** moved into `params` instead of being hardcoded three times
  in the body of the file.

Every filter/cleaning step (the `filter()`, `tabyl()`, join, and grouping
logic) is preserved exactly as it was in your original file — I only changed
*how results are displayed*, not how they're computed, so the underlying
numbers shouldn't drift.

## Important — please verify before trusting this for publication

I don't have R, tidyverse, or Quarto available in the environment I built
this in, so **I could not actually run this against your data**. Everything
here is based on careful reading of your original file plus a real,
programmatically-verified test of the Word-styling pipeline itself (I
rendered sample content through the actual reference-doc using pandoc — the
same engine Quarto uses — and confirmed every style applies correctly). But
the R/data logic itself is unverified. Before you rely on this quarter to
quarter:

1. **Render it once and skim every table** against your last real report to
   confirm the numbers match.
2. **Specifically spot-check the suppression logic** — pick a few cells you
   know are small (<11) from a past report and confirm they show as
   `<11` and that totals suppress correctly when only one cell in a row is
   small. This handles client privacy, so it's worth a careful first look
   rather than trusting it blind.
3. The `dhcs_crosstab()` helper recomputes counts and percentages itself
   (rather than reusing `tabyl()`'s `adorn_ns()`/`adorn_percentages()` chain)
   so suppression could happen before any percentage math touched a small
   cell — double check a couple of its outputs against the equivalent table
   in your original file to confirm the percentages still match.

If anything's off, it's almost certainly in one of those two spots — the
styling/rendering pipeline itself I was able to test directly and is solid.
