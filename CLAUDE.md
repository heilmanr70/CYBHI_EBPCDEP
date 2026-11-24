# CYBHI EBP/CDEP evaluation scripts

## Data
- Real data contain PHI and are never in this repo or this environment. Never look for them or try Dropbox paths.
- Test every change against mock data that matches the real column structures.
- Never commit renders or outputs made from real data.

## R style
- All library() calls in one block near the top; track every dependency.
- No setwd(); paths must work in a fresh session and chunk by chunk.
- Assume an empty environment; never rely on workspace objects.
- Prefer tidyverse; avoid obscure or unmaintained packages.
- No hard-coded factor levels or magic strings; code must still run if the data change.
- Keep code short; vectorize instead of loops; factor repeated code into functions.
- Fix root causes, not band-aids.
- Reuse existing helpers and make minimal changes to working code.
- Log every change in CHANGES.md.