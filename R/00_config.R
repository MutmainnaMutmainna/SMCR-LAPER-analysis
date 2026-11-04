# Paths and constants shared by all scripts.
# Edit only this file to point the scripts at your local copies.

DATA_FILE <- "data/SMCR-LAPER dataset for ML_5thVersion.xlsx"   # corrected workbook (Mendeley Data, version 5 or later), see README for its SHA-256
DATA_SHEET <- "Raw_data"
KEY_FILE <- "keys/answer_keys.xlsx"           # one sheet per instrument (obtain keys from PhysPort)
RATINGS_FILE <- "validation/validator_ratings.csv"
OUT_DIR <- "outputs"

EXPECTED_ROWS <- 1081
SEED <- 42

# Columns used for the FCI2 illustrative analysis (check against the Raw_data sheet of the corrected workbook)
ID_COL <- "ID"                                # respondent identifier column
FCI2_ITEMS <- paste0("FCI2-", 1:30)
TIME_FCI2 <- "Time_FCI2"
TIME_LIMIT_SEC <- 1800                        # recommended FCI duration (30 minutes), used to normalise time
# Respondents excluded from the illustrative analysis as probable timer anomalies.
# 7_ID70 has a recorded FCI2 time of 51 minutes 39 seconds, far above the 40 minute limit of the test form.
# Set to character(0) to include all respondents with a recorded time (606 respondents).
EXCLUDE_IDS <- c("7_ID70")
RT_THRESHOLD <- 0.25                          # ratio below which a response is labelled rapid (450 seconds)

# Item columns of the MBT subset (16 of 26 original items, see Supplementary Table S2)
MBT_ITEMS <- paste0("MBT", c(1:8, 10, 15, 16, 19:22, 26))

dir.create(OUT_DIR, showWarnings = FALSE)
