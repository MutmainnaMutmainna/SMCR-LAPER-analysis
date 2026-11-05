# Confirmatory factor analysis (WLSMV, Table 6) and reliability (Table 7) of the SMCR-LAPER instruments.
#
# INPUT. A local CFA workbook (data/CFA_workbook.xlsx) with one sheet per instrument (MBT, FCI1, FCI2, FMCE, EMCS,
# RRMCS, FLIP COIN, CLASS) and the answer keys (keys/answer_keys.xlsx, sheets MBT, FCI1, FCI2, FMCE, EMCS, RRMCS, FLIP-CoIN
# in the same column order as the response sheets). Neither file is distributed. The CFA workbook holds the respondents analysed
# in Table 6, which are subsets of the published workbook. The rule that selects them is not encoded here.
# The script prints the sample size of each instrument.
#
# STATUS. Adapted from the author's R notebook that produced Table 6. Not yet run inside this archive.

library(readxl)
library(dplyr)
library(lavaan)
library(CTT)
library(psych)
library(openxlsx)

select <- dplyr::select
filter <- dplyr::filter

# Local input files (not distributed, see README.md).
CFA_FILE <- "data/CFA_workbook.xlsx"     # one sheet per instrument: MBT, FCI1, FCI2, FMCE, EMCS, RRMCS, FLIP COIN, CLASS
KEY_FILE <- "keys/answer_keys.xlsx"      # answer keys, one sheet per instrument (obtained from PhysPort, not distributed)
OUT_DIR  <- "outputs"                     # results are written here
dir.create(OUT_DIR, showWarnings = FALSE)
stopifnot(file.exists(CFA_FILE), file.exists(KEY_FILE))

# ---- I. Responses ----
# Options A to E (FMCE A to J) for the concept inventories, ratings 1 to 4 for the CLASS.
MBT_raw      <- read_xlsx(CFA_FILE, sheet = "MBT")
FCI1_raw     <- read_xlsx(CFA_FILE, sheet = "FCI1")
FCI2_raw     <- read_xlsx(CFA_FILE, sheet = "FCI2")
FMCE_raw     <- read_xlsx(CFA_FILE, sheet = "FMCE")
EMCS_raw     <- read_xlsx(CFA_FILE, sheet = "EMCS")
RRMCS_raw    <- read_xlsx(CFA_FILE, sheet = "RRMCS")
FLIPCOIN_raw <- read_xlsx(CFA_FILE, sheet = "FLIP COIN")
CLASS_raw    <- read_xlsx(CFA_FILE, sheet = "CLASS")

# ---- II. Answer keys (one sheet per instrument, columns in the same order as the response sheets) ----
MBTkey      <- read_xlsx(KEY_FILE, sheet = "MBT")
FCI1key     <- read_xlsx(KEY_FILE, sheet = "FCI1")
FCI2key     <- read_xlsx(KEY_FILE, sheet = "FCI2")
FMCEkey     <- read_xlsx(KEY_FILE, sheet = "FMCE")
EMCSkey     <- read_xlsx(KEY_FILE, sheet = "EMCS")
RRMCSkey    <- read_xlsx(KEY_FILE, sheet = "RRMCS")
FLIPCOINkey <- read_xlsx(KEY_FILE, sheet = "FLIP-COIN")

# ---- III. Scoring (1 = correct, 0 = incorrect) ----
MBT_scored      <- score(MBT_raw,      MBTkey,      output.scored = TRUE)
FCI1_scored     <- score(FCI1_raw,     FCI1key,     output.scored = TRUE)
FCI2_scored     <- score(FCI2_raw,     FCI2key,     output.scored = TRUE)
FMCE_scored     <- score(FMCE_raw,     FMCEkey,     output.scored = TRUE)
EMCS_scored     <- score(EMCS_raw,     EMCSkey,     output.scored = TRUE)
RRMCS_scored    <- score(RRMCS_raw,    RRMCSkey,    output.scored = TRUE)
FLIPCOIN_scored <- score(FLIPCOIN_raw, FLIPCOINkey, output.scored = TRUE)

score_MBT      <- as.data.frame(MBT_scored$scored)
score_FCI1     <- as.data.frame(FCI1_scored$scored)
score_FCI2     <- as.data.frame(FCI2_scored$scored)
score_FMCE     <- as.data.frame(FMCE_scored$scored)
score_EMCS     <- as.data.frame(EMCS_scored$scored)
score_RRMCS    <- as.data.frame(RRMCS_scored$scored)
score_FLIPCOIN <- as.data.frame(FLIPCOIN_scored$scored)

# CLASS: the ratings from 1 to 4 are used as they are (no scoring)
score_CLASS <- CLASS_raw %>%
  select(where(is.numeric)) %>%
  select(-any_of("ID"))

# Sample size of each CFA sample (N column of Table 6)
sample_n <- sapply(list(MBT = score_MBT, FMCE = score_FMCE, FCI1 = score_FCI1,
                        FCI2 = score_FCI2, EMCS = score_EMCS, RRMCS = score_RRMCS,
                        FLIPCOIN = score_FLIPCOIN, CLASS = score_CLASS), nrow)
print(sample_n)
# Expected: MBT 724, FMCE 648, FCI1 767, FCI2 483, EMCS 447, RRMCS 426, FLIPCOIN 382, CLASS 662


# ---- IV. CFA model specifications (lavaan syntax) ----
# Item columns are V1, V2, ... in the order of the response sheets (CLASS items are B1 to B42).

# MBT
model_MBT <- '
  MBTF1 =~ V1+V4
  MBTF2 =~ V2+V3+V8+V14
  MBTF3 =~ V7+V5+V12
  MBTF4 =~ V13+V9
  MBTF5 =~ V11+V15+V10
  MBTF6 =~ V6+V16
'

# FCI (the same five-factor structure for the two administrations)
model_FCI1 <- '
  FCI1 =~ V6+V7+V8+V10+V20+V23+V24
  FCI2 =~ V9+V12+V14+V19+V21+V22+V27
  FCI3 =~ V4+V15+V16+V28
  FCI4 =~ V5+V11+V13+V18+V30
  FCI5 =~ V17+V25+V26
'

model_FCI2 <- '
  FCI1 =~ V6+V7+V8+V10+V20+V23+V24
  FCI2 =~ V9+V12+V14+V19+V21+V22+V27
  FCI3 =~ V4+V15+V16+V28
  FCI4 =~ V5+V11+V13+V18+V30
  FCI5 =~ V17+V25+V26
'

# FMCE
model_FMCE <- '
  FMCE1 =~ V1+V2+V3+V4+V7
  FMCE2 =~ V8+V9+V10+V11+V12+V13+V27+V28+V29
  FMCE3 =~ V14+V16+V17+V18+V19+V20+V21
  FMCE4 =~ V22+V23+V24+V25+V26
  FMCE5 =~ V30+V31+V32+V34+V36+V38
  FMCE6 =~ V40+V41+V42+V43
  FMCE7 =~ V44+V45+V46+V47
'

# EMCS
model_EMCS <- '
  EMCS1 =~ V1+V22+V17+V15+V9+V23+V5+V2+V20+V10+V24+V4+V8+V6+V12
  EMCS2 =~ V18+V25+V16+V3+V14+V13+V21+V19+V7+V11
'

# RRMCS
model_RRMCS <- '
  RRMCS1 =~ V3+V4+V20+V24
  RRMCS2 =~ V1+V2+V3+V21
  RRMCS3 =~ V5+V13+V22+V25
  RRMCS4 =~ V11+V12+V24+V29
  RRMCS5 =~ V9+V10+V23+V26+V27+V28+V30
  RRMCS6 =~ V6+V7+V8+V16
  RRMCS7 =~ V14+V15+V17
  RRMCS8 =~ V18+V19
'

# FliP-CoIn (V41 and V42 hold the same item, item 26, and are not used)
model_FLIPCOIN <- '
  FLIP1 =~ V1
  FLIP2 =~ V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14
  FLIP3 =~ V2
  FLIP4 =~ V25+V26+V27+V28
  FLIP5 =~ V29
  FLIP6 =~ V15+V16+V17+V18+V19+V20+V21
  FLIP7 =~ V22+V23
  FLIP8 =~ V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40
'

# CLASS (ratings from 1 to 4)
model_CLASS <- '
  CLASS1 =~ B28+B30+B35+B37
  CLASS2 =~ B3+B11+B14+B25+B28+B30
  CLASS3 =~ B11+B23+B24+B32+B36+B39+B42
  CLASS4 =~ B1+B5+B6+B13+B21+B32
  CLASS5 =~ B1+B5+B6+B8+B21+B22+B40
  CLASS6 =~ B13+B15+B16+B25+B26+B34+B40+B42
  CLASS7 =~ B15+B16+B34+B40
  CLASS8 =~ B5+B21+B22+B25+B34+B40
'


# ---- V. CFA function (WLSMV, ordinal indicators) ----
run_cfa <- function(data, model, instrument, n_factors) {

  data_ord <- data %>% mutate(across(everything(), ~ ordered(.)))

  fit <- tryCatch({
    cfa(model, data = data_ord, ordered = TRUE, estimator = "WLSMV",
        check.gradient = FALSE, control = list(iter.max = 10000))
  }, error = function(e) {
    cat(sprintf("[%s] WLSMV failed: %s\nTrying ULSMV...\n", instrument, conditionMessage(e)))
    tryCatch({
      cfa(model, data = data_ord, ordered = TRUE, estimator = "ULSMV",
          check.gradient = FALSE, control = list(iter.max = 10000))
    }, error = function(e2) {
      cat(sprintf("[%s] ULSMV failed as well.\n", instrument))
      NULL
    })
  })

  if (is.null(fit)) {
    return(list(fit = NULL, table = data.frame(
      Instrument = instrument, Factors = n_factors,
      chi_sq = NA, df = NA, CFI = NA, TLI = NA, RMSEA = NA, SRMR = NA)))
  }

  idx <- fitMeasures(fit, c("chisq", "df", "cfi", "tli", "rmsea", "srmr"))

  cat(sprintf("\n=== %s ===\n", instrument))
  cat(sprintf("  chi_sq : %.3f\n", idx["chisq"]))
  cat(sprintf("  df     : %.0f\n", idx["df"]))
  cat(sprintf("  CFI    : %.3f\n", idx["cfi"]))
  cat(sprintf("  TLI    : %.3f\n", idx["tli"]))
  cat(sprintf("  RMSEA  : %.3f\n", idx["rmsea"]))
  cat(sprintf("  SRMR   : %.3f\n", idx["srmr"]))

  list(fit = fit,
       table = data.frame(Instrument = instrument, Factors = n_factors,
                          chi_sq = round(idx["chisq"], 3), df = as.integer(idx["df"]),
                          CFI = round(idx["cfi"], 3), TLI = round(idx["tli"], 3),
                          RMSEA = round(idx["rmsea"], 3), SRMR = round(idx["srmr"], 3),
                          row.names = NULL))
}


# ---- VI. Run the CFA for all instruments ----
res <- list(
  MBT      = run_cfa(score_MBT,      model_MBT,      "MBT",      6),
  FMCE     = run_cfa(score_FMCE,     model_FMCE,     "FMCE",     7),
  FCI1     = run_cfa(score_FCI1,     model_FCI1,     "FCI1",     5),
  FCI2     = run_cfa(score_FCI2,     model_FCI2,     "FCI2",     5),
  EMCS     = run_cfa(score_EMCS,     model_EMCS,     "EMCS",     2),
  RRMCS    = run_cfa(score_RRMCS,    model_RRMCS,    "RRMCS",    8),
  FLIPCOIN = run_cfa(score_FLIPCOIN, model_FLIPCOIN, "FliP-CoIn", 8),
  CLASS    = run_cfa(score_CLASS,    model_CLASS,    "CLASS",    8)
)

# ---- VII. Fit indices (Table 6) ----
fit_table <- bind_rows(lapply(res, function(x) x$table))
fit_table$N <- as.integer(sample_n[c("MBT", "FMCE", "FCI1", "FCI2", "EMCS", "RRMCS", "FLIPCOIN", "CLASS")])
print(fit_table)

# Conventional cut-offs: CFI and TLI >= 0.95, RMSEA <= 0.06, SRMR <= 0.08
fit_table_check <- fit_table %>%
  mutate(CFI_ok = CFI >= 0.95, TLI_ok = TLI >= 0.95, RMSEA_ok = RMSEA <= 0.06, SRMR_ok = SRMR <= 0.08)
print(fit_table_check[, c("Instrument", "CFI", "CFI_ok", "TLI", "TLI_ok", "RMSEA", "RMSEA_ok", "SRMR", "SRMR_ok")])

# ---- VIII. Standardised factor loadings ----
loadings <- bind_rows(lapply(names(res), function(nm) {
  f <- res[[nm]]$fit
  if (is.null(f)) return(NULL)
  parameterEstimates(f, standardized = TRUE) %>%
    filter(op == "=~") %>%
    transmute(Instrument = nm, Factor = lhs, Item = rhs,
              Estimate = round(est, 3), Std_loading = round(std.all, 3),
              SE = round(se, 3), z = round(z, 3), p = round(pvalue, 3))
}))

# ---- IX. Export ----
wb <- createWorkbook()
addWorksheet(wb, "Fit_indices");     writeData(wb, "Fit_indices", fit_table)
addWorksheet(wb, "Factor_loadings"); writeData(wb, "Factor_loadings", loadings)
saveWorkbook(wb, file.path(OUT_DIR, "CFA_results.xlsx"), overwrite = TRUE)
write.csv(fit_table, file.path(OUT_DIR, "cfa_fit_indices.csv"), row.names = FALSE)
write.csv(loadings, file.path(OUT_DIR, "cfa_loadings.csv"), row.names = FALSE)
cat("\nSaved the fit indices and loadings in", OUT_DIR, "\n")


# ---- X. Reliability (Cronbach's alpha and McDonald's omega) ----
# Alpha is computed without reversing any item (check.keys = FALSE).
# FliP-CoIn: item 26 (V41 and V42, recorded twice in the CFA workbook) is excluded.
# Omega total (one factor, tetrachoric or polychoric correlations) is computed for all instruments,
# but Table 7 reports omega for the MBT only.
compute_reliability <- function(data, instrument) {

  data_num <- data %>% select(where(is.numeric))

  alpha_result <- tryCatch(
    suppressWarnings(psych::alpha(data_num, check.keys = FALSE)),
    error = function(e) { cat(sprintf("[%s] Alpha failed: %s\n", instrument, conditionMessage(e))); NULL })
  raw_alpha <- if (!is.null(alpha_result)) round(alpha_result$total$raw_alpha, 4) else NA

  omega_result <- tryCatch(
    suppressWarnings(psych::omega(data_num, nfactors = 1, poly = TRUE, plot = FALSE)),
    error = function(e) { cat(sprintf("[%s] Omega failed: %s\n", instrument, conditionMessage(e))); NULL })
  omega_total <- if (!is.null(omega_result)) round(omega_result$omega.tot, 4) else NA

  cat(sprintf("\n=== RELIABILITY: %s (N = %d, %d columns) ===\n", instrument, nrow(data_num), ncol(data_num)))
  cat(sprintf("  Cronbach's alpha : %s\n", ifelse(is.na(raw_alpha), "-", raw_alpha)))
  cat(sprintf("  McDonald's omega : %s\n", ifelse(is.na(omega_total), "-", omega_total)))

  data.frame(Instrument = instrument, N = nrow(data_num), Columns = ncol(data_num),
             Alpha = raw_alpha, Omega_total = omega_total)
}

reliability_table <- bind_rows(
  compute_reliability(score_MBT,   "MBT (CFA sample)"),
  compute_reliability(score_FMCE,  "FMCE"),
  compute_reliability(score_FCI1,  "FCI1"),
  compute_reliability(score_FCI2,  "FCI2"),
  compute_reliability(score_EMCS,  "EMCS"),
  compute_reliability(score_RRMCS, "RRMCS"),
  compute_reliability(score_FLIPCOIN[, paste0("V", 1:40)], "FliP-CoIn (item 26 excluded)"),
  compute_reliability(score_CLASS, "CLASS")
)
print(reliability_table)
cat("\nThe MBT values in Table 7 (alpha 0.58, omega 0.72, N = 906) come from R/02_reliability_mbt.R,\n")
cat("which uses all complete respondents of the published workbook, not this CFA sample.\n")

wb_rel <- createWorkbook()
addWorksheet(wb_rel, "Reliability")
writeData(wb_rel, "Reliability", reliability_table)
saveWorkbook(wb_rel, file.path(OUT_DIR, "Reliability_results.xlsx"), overwrite = TRUE)
write.csv(reliability_table, file.path(OUT_DIR, "cfa_sample_reliability.csv"), row.names = FALSE)
cat("\nSaved the reliability results in", OUT_DIR, "\n")

writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info_cfa.txt"))
