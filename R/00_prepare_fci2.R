# Prepare the FCI2 input for the illustrative IRT and Random Forest analysis.
# Reads the published workbook (corrected workbook, Mendeley Data version 5 or later) and the user-supplied answer keys,
# selects the respondents used in the manuscript, scores the items in memory, and computes the
# response time ratio and the response time label.
#
# Two kinds of output are written.
#  - work/fci2_input.rds contains scored (0/1) responses. It is NOT shared (listed in .gitignore),
#    because scored responses together with the published options would reveal the answer key.
#  - outputs/fci2_rt_labels.csv contains only identifiers, the response time ratio, the response time
#    label, and the proportion correct. These do not reveal the key and can be shared.

source("R/00_config.R")
source("R/helpers.R")

d <- read_smcr()
keys <- read_keys()
keys_fci <- keys[keys$instrument == "FCI", ]
if (nrow(keys_fci) != 30) stop("Expected 30 FCI keys but found ", nrow(keys_fci), ". Check the FCI sheet.")

# The FCI key sheet may name its columns differently from the data (for example FCI1 to FCI30).
# Map each key to the FCI2 column with the same item number (the trailing digits of the name).
key_no <- as.integer(sub(".*?([0-9]+)$", "\\1", keys_fci$item))
if (anyNA(key_no) || !setequal(key_no, 1:30)) stop("Could not read item numbers 1 to 30 from the FCI key sheet.")
keys_fci$item <- paste0("FCI2-", key_no)
cat("Key sheet names mapped to data columns (first 3):",
    paste(head(keys$item[keys$instrument == "FCI"], 3), "->", head(keys_fci$item, 3), collapse = "  "), "\n")

needed <- c(ID_COL, FCI2_ITEMS, TIME_FCI2)
missing_cols <- setdiff(needed, names(d))
if (length(missing_cols) > 0) {
  stop("Columns not found in ", DATA_SHEET, ": ", paste(missing_cols, collapse = ", "),
       ". Check ID_COL, FCI2_ITEMS, and TIME_FCI2 in R/00_config.R.")
}

# 1. Respondents with at least one recorded FCI2 response
opts <- as.data.frame(lapply(d[FCI2_ITEMS], function(x) toupper(trimws(as.character(x)))),
                      stringsAsFactors = FALSE)
has_resp <- rowSums(!is.na(opts) & opts != "") > 0
cat("Respondents with FCI2 responses:", sum(has_resp), "(manuscript 657)\n")

# 2. Recorded completion time (text such as "13 minutes 14 seconds"), converted to seconds
raw_time <- d[[TIME_FCI2]]
time_sec <- parse_duration_sec(raw_time)
unparsed <- !is.na(raw_time) & is.na(time_sec)
if (any(unparsed)) {
  warning(sum(unparsed), " time entries could not be read, for example: ",
          paste(head(unique(raw_time[unparsed]), 5), collapse = " | "))
}
cat("Completion time summary (seconds, respondents with FCI2 responses):\n")
print(summary(time_sec[has_resp]))

has_time <- has_resp & !is.na(time_sec)
cat("Respondents with FCI2 responses and a recorded time:", sum(has_time), "\n")
if (sum(has_time) == 0) stop("No readable completion times. Check TIME_FCI2 and the time format.")

# Respondents listed in EXCLUDE_IDS (probable timer anomalies, see 00_config.R) are not analysed.
excluded <- has_time & d[[ID_COL]] %in% EXCLUDE_IDS
cat("Excluded as probable timer anomalies:", sum(excluded), "\n")
if (any(excluded)) print(data.frame(id = d[[ID_COL]][excluded], seconds = time_sec[excluded]))

use <- has_time & !excluded
cat("Respondents included in the analysis:", sum(use), "(manuscript 605)\n")

# 3. Score in memory with the user-supplied key
scored <- score_items(d[use, ], FCI2_ITEMS, keys_fci)
if (anyNA(scored)) warning("Missing item responses found among the selected respondents (manuscript reports none).")

# 4. Response time ratio relative to the recommended duration, truncated at 1.0
rt_raw <- time_sec[use] / TIME_LIMIT_SEC
cat("Ratios above 1.0 before truncation:", sum(rt_raw > 1), "(manuscript 109)\n")
rt_ratio <- pmin(rt_raw, 1)
rt_label <- ifelse(rt_ratio < RT_THRESHOLD, "invalid", "valid")
cat("Label counts (rapid = invalid):\n"); print(table(rt_label))

prop_correct <- rowMeans(scored)

# 5. Save
dir.create("work", showWarnings = FALSE)
saveRDS(list(id = d[[ID_COL]][use], options = opts[use, ], scored = scored,
             rt_ratio = rt_ratio, rt_label = rt_label, prop_correct = prop_correct,
             time_sec = time_sec[use]),
        file = "work/fci2_input.rds")

shareable <- data.frame(id = d[[ID_COL]][use], rt_ratio = rt_ratio,
                        rt_label = rt_label, prop_correct = prop_correct)
write.csv(shareable, file.path(OUT_DIR, "fci2_rt_labels.csv"), row.names = FALSE)
cat("Saved work/fci2_input.rds (not shared) and", file.path(OUT_DIR, "fci2_rt_labels.csv"), "\n")
