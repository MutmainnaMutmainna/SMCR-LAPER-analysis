# SMCR-LAPER analysis code

Analysis code for the Data Descriptor "Preservice physics teachers' multiple-choice response dataset for physics education research" (Scientific Data).

- Dataset: Mendeley Data, https://doi.org/10.17632/j6x6n422sh (most recent version). The analyses use the corrected workbook identified by the SHA-256 given below. Versions 4 and 5 contain errors in MBT20 and must not be used. Use version 6 (https://doi.org/10.17632/j6x6n422sh.6).
- Code archive: Zenodo, https://doi.org/10.5281/zenodo.23957946, version 1.1.0
- Software: R 4.6.0

## What is in this archive

| Folder | Content |
|---|---|
| `R/` | Analysis scripts, numbered in the order they are run |
| `mappings/` | Placeholder. Mappings of Supplementary Tables S1 to S3 as CSV are to be added. The CFA model syntax is currently inside `R/06_cfa.R` |
| `validation/` | Aiken's V per item (Table 5). Ratings of individual validators are not distributed |
| `outputs/` | Results written by the scripts when they are run (labels, split IDs, out-of-fold predictions, item parameters, agreement tables) |
| `notebooks/` | HTML reports of the notebooks that produced the CFA and reliability results and the IRT and Random Forest results (no answer keys, no respondent-level data) |
| `keys/` | Not distributed. See "Answer keys" below |

## Data

The scripts read the published workbook (corrected version, Mendeley Data version 6 or later), sheet `Raw_data`. Set its file name in `DATA_FILE` in `R/00_config.R`. The data are not copied into this archive, so the archived code and the published data cannot diverge.

Before running, check the file against the values below.

- Rows: 1,081 and columns: 285 in the sheet `Raw_data`
- SHA-256: 96ae9cae4696dba4226de223deb19a6d0546f6644f1e923138cba523a4fd7cb6
- MD5: dcce0a04fa4f4e33533a09a015b69412

If the checksum of your file differs, you are probably not using the corrected workbook. `R/00_start.R` prints the MD5 of the file it reads.

## Answer keys

The answer keys of the original instruments are not redistributed because of the PhysPort distribution restrictions. The published dataset contains only the options selected by respondents. To rerun the scoring step, obtain the keys from PhysPort and save them as `keys/answer_keys.xlsx` with one sheet per instrument (see `keys/README.md`).

## Requirements

R 4.6.0 and the packages recorded in `session_info.txt` (or `renv.lock`). Main packages are readxl, psych, lavaan, mirt, mclust, caret, randomForest, smotefamily, and pROC.

## How to run

1. Place the Mendeley workbook (version 6) in `data/` and `keys/answer_keys.xlsx` in `keys/`, and set `DATA_FILE` at the top of `R/00_config.R`.
2. Restart R, set the working directory to the archive folder with `setwd()`, and run `source("R/00_start.R")` to check that the data load correctly.
3. Run `source("run_all.R")`, or run the scripts one by one in numerical order.
4. `R/06_cfa.R` is run separately. It needs a local CFA workbook (`data/CFA_workbook.xlsx`, one sheet per instrument, the respondents analysed in Table 6) that is not distributed, because the rule that selects the CFA respondents from the published workbook is not encoded in the script. The script prints the sample size of each instrument.

## Map from manuscript to script

| Manuscript item | Script | Status |
|---|---|---|
| FCI2 analysis input (657 to 605 respondents, scoring, response time ratio) | `R/00_prepare_fci2.R` | included, run on the corrected workbook |
| Table 5 (Aiken's V, item-level values) | `validation/aiken_v_by_item.csv` | included (values only, script to be added) |
| Table 7 (reliability, MBT) | `R/02_reliability_mbt.R` | included, run on the corrected workbook |
| Table 7 (reliability, other instruments) | `R/06_cfa.R` | included, run on the author's local CFA workbook (HTML report in `notebooks/`) |
| Table 6 and Supplementary Tables S1 to S3 (CFA fit, loadings) | `R/06_cfa.R` | included, run on the author's local CFA workbook (HTML report in `notebooks/`) |
| Supplementary Tables S4 to S9 (FCI2 IRT assumptions and model comparison) | `R/05_irt_vs_rf.R` | included, run on the corrected workbook (HTML report in `notebooks/`) |
| Tables 8 and 9 and Fig. 4 (IRT, RF, and response time labels) | `R/05_irt_vs_rf.R` | included, run on the corrected workbook (HTML report in `notebooks/`) |

## Settings recorded for the illustrative use case

- Random seed 42, stratified 70/30 split (training 425, test 180). The split IDs are saved in `outputs/` by `R/05_irt_vs_rf.R` because random number generation can differ between R versions.
- RF tuning by random search over 20 values of mtry in [2, 4 times the square root of p] with p = 62, five-fold cross-validation, ntree = 300, AUC as the criterion.
- SMOTE with K = 5, applied only to training data within each fold.
- Response time ratio relative to 1,800 seconds, threshold 0.25 (450 seconds), ratios above 1.0 truncated to 1.0.

## Citation and licence

See `CITATION.cff`. Code is released under the MIT licence (`LICENSE`). The dataset is licensed separately under CC BY 4.0 at Mendeley Data.
