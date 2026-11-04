# Helper functions.

# Read the published workbook and check it against the expected number of rows.
read_smcr <- function(file = DATA_FILE, sheet = DATA_SHEET, expected_rows = EXPECTED_ROWS) {
  d <- as.data.frame(readxl::read_excel(file, sheet = sheet))
  if (nrow(d) != expected_rows) {
    warning("Expected ", expected_rows, " rows but found ", nrow(d),
            ". Check that the file is the corrected workbook (see README for its SHA-256).")
  }
  d
}

# Convert completion times recorded as text, for example "13 minutes 14 seconds" or "1 minute 51 seconds",
# to seconds. Hours are read when present. Missing values and text without a time unit (for example N/A) give NA.
parse_duration_sec <- function(x) {
  x <- tolower(as.character(x))
  num_before <- function(unit) {
    has <- grepl(paste0("[0-9]+\\s*", unit), x)            # FALSE for NA
    out <- rep(0, length(x))
    out[has] <- as.numeric(sub(paste0("^.*?([0-9]+)\\s*", unit, ".*$"), "\\1", x[has], perl = TRUE))
    out
  }
  tot <- num_before("hour") * 3600 + num_before("minute") * 60 + num_before("second")
  tot[is.na(x) | !grepl("hour|minute|second", x)] <- NA_real_
  tot
}

# Read the user-supplied answer key workbook (not distributed, see keys/README.md).
# Format: one sheet per instrument, one row, column names = item names, cell values = key letters.
# The function returns a long table with the columns instrument, item, and key.
read_keys <- function(file = KEY_FILE) {
  if (!file.exists(file)) {
    stop("Answer key file not found: ", file,
         ". Obtain the keys from PhysPort and save them as described in keys/README.md.")
  }
  sheets <- readxl::excel_sheets(file)
  out <- lapply(sheets, function(s) {
    k <- as.data.frame(readxl::read_excel(file, sheet = s, col_types = "text"))
    if (nrow(k) < 1) stop("Sheet ", s, " has no key row.")
    data.frame(instrument = s, item = names(k),
               key = toupper(trimws(as.character(unlist(k[1, ])))),
               stringsAsFactors = FALSE, row.names = NULL)
  })
  do.call(rbind, out)
}

# Score selected options as 1 (correct) or 0 (incorrect) using a key table.
# key_table has the columns instrument, item, key. Items are matched by column name.
score_items <- function(resp, items, key_table) {
  keys <- key_table$key[match(items, key_table$item)]
  if (anyNA(keys)) stop("No key found for: ", paste(items[is.na(keys)], collapse = ", "))
  out <- sapply(seq_along(items), function(j) {
    x <- toupper(trimws(as.character(resp[[items[j]]])))
    ifelse(is.na(resp[[items[j]]]) | x == "", NA_integer_, as.integer(x == toupper(keys[j])))
  })
  colnames(out) <- items
  as.data.frame(out)
}

# Classical KR-20 (population variance of the total score). For dichotomous items
# computed on the same records it equals Cronbach's alpha.
kr20 <- function(x) {
  x <- as.matrix(x)
  x <- x[complete.cases(x), , drop = FALSE]
  k <- ncol(x); n <- nrow(x)
  p <- colMeans(x); q <- 1 - p
  var_tot <- var(rowSums(x)) * (n - 1) / n
  c(N = n, k = k, KR20 = (k / (k - 1)) * (1 - sum(p * q) / var_tot))
}
