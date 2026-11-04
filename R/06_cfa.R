# Confirmatory factor analysis (WLSMV, Table 6) and reliability (Table 7), adapted from the author's R notebook.
#
# INPUT. This script needs a local CFA workbook with one sheet per instrument (MBT, FCI1, FCI2, FMCE, EMCS,
# RRMCS, FLIP COIN, CLASS). Item columns are V1, V2, ... (CLASS: B1, B2, ...). The sheets hold the selected
# options (A to E, FMCE A to J) or the CLASS ratings (1 to 4) of the respondents analysed in Table 6.
# The CFA samples are subsets of the published workbook. The selection of respondents is NOT encoded here,
# so the CFA workbook is not part of the archive. The script prints the N of each sample.
# Answer keys are read from keys/answer_keys.xlsx (not distributed). Key sheets: MBT, FMCE, FCI, EMCS, RRMCS, Flip_CoIn.
# Key letters are matched to the columns by POSITION (first key = V1, and so on). The number of keys must equal the
# number of item columns of the sheet, otherwise the script stops.
#
# STATUS. Draft adapted from the notebook that produced Table 6. Not yet run inside this archive.

source("R/00_config.R")
source("R/helpers.R")
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(lavaan); library(CTT); library(psych)
})
select <- dplyr::select
filter <- dplyr::filter

CFA_FILE <- "data/CFA_workbook.xlsx"      # local file, see the note above
stopifnot(file.exists(CFA_FILE))

keys <- read_keys()
get_key <- function(instrument, n) {
  k <- keys$key[keys$instrument == instrument]
  if (length(k) != n) stop("Key sheet ", instrument, " has ", length(k), " keys but the CFA sheet has ", n, " columns.")
  k
}
raw <- function(sheet) as.data.frame(readxl::read_xlsx(CFA_FILE, sheet = sheet))
score_sheet <- function(sheet, instrument) {
  r <- raw(sheet)
  as.data.frame(CTT::score(r, get_key(instrument, ncol(r)), output.scored = TRUE)$scored)
}

score_MBT      <- score_sheet("MBT",       "MBT")
score_FCI1     <- score_sheet("FCI1",      "FCI")
score_FCI2     <- score_sheet("FCI2",      "FCI")
score_FMCE     <- score_sheet("FMCE",      "FMCE")
score_EMCS     <- score_sheet("EMCS",      "EMCS")
score_RRMCS    <- score_sheet("RRMCS",     "RRMCS")
score_FLIPCOIN <- score_sheet("FLIP COIN", "Flip_CoIn")
score_CLASS    <- raw("CLASS") %>% select(where(is.numeric)) %>% select(-any_of("ID"))

samples <- sapply(list(MBT = score_MBT, FMCE = score_FMCE, FCI1 = score_FCI1, FCI2 = score_FCI2, EMCS = score_EMCS,
                       RRMCS = score_RRMCS, FliP_CoIn = score_FLIPCOIN, CLASS = score_CLASS), nrow)
cat("Respondents per CFA sample (N column for Table 6):\n"); print(samples)
# Expected: MBT 724, FMCE 648, FCI1 767, FCI2 483, EMCS 447, RRMCS 426, FliP-CoIn 384, CLASS 662

# ============================================
# IV. DEFINISI MODEL CFA
# ============================================

# --- MBT (sesuaikan dengan hasil EFA / teori) ---
model_MBT <- '
  MBTF1 =~ V1+V4
  MBTF2 =~ V2+V3+V8+V14
  MBTF3 =~ V7+V5+V12
  MBTF4 =~ V13+V9
  MBTF5 =~ V11+V15+V10
  MBTF6 =~ V6+V16
'

# --- FCI (contoh sesuai gambar 3) ---
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

# --- FMCE ---
model_FMCE <- '
  FMCE1 =~ V1+V2+V3+V4+V7
  FMCE2 =~ V8+V9+V10+V11+V12+V13+V27+V28+V29
  FMCE3 =~ V14+V16+V17+V18+V19+V20+V21
  FMCE4 =~ V22+V23+V24+V25+V26
  FMCE5 =~ V30+V31+V32+V34+V36+V38
  FMCE6 =~ V40+V41+V42+V43
  FMCE7 =~ V44+V45+V46+V47
'

# --- EMCS ---
model_EMCS <- '
  EMCS1 =~ V1+V22+V17+V15+V9+V23+V5+V2+V20+V10+V24+V4+V8+V6+V12
  EMCS2 =~ V18+V25+V16+V3+V14+V13+V21+V19+V7+V11
'

# --- RRMCS ---
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

# --- FLIP COIN ---
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

# --- CLASS (skala 1-4) ---
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
jalankan_cfa <- function(data, model,
                          nama_instrumen,
                          n_faktor,
                          is_likert = FALSE) {

  # Konversi ke ordered
  data_ord <- data %>%
    mutate(across(everything(), ~ ordered(.)))

  # Estimator WLSMV (tepat untuk data ordinal/biner)
  fit <- tryCatch({
    cfa(model,
        data           = data_ord,
        ordered        = TRUE,
        estimator      = "WLSMV",
        check.gradient = FALSE,
        control        = list(iter.max = 10000))
  }, error = function(e) {
    cat(sprintf("[%s] WLSMV gagal: %s\nMencoba ULSMV...\n",
                nama_instrumen, conditionMessage(e)))
    tryCatch({
      cfa(model,
          data           = data_ord,
          ordered        = TRUE,
          estimator      = "ULSMV",
          check.gradient = FALSE,
          control        = list(iter.max = 10000))
    }, error = function(e2) {
      cat(sprintf("[%s] ULSMV juga gagal.\n", nama_instrumen))
      return(NULL)
    })
  })

  if (is.null(fit)) {
    return(data.frame(
      Instrumen=nama_instrumen, Jumlah_Faktor=n_faktor,
      chi_sq=NA, df=NA, CFI=NA, TLI=NA, RMSEA=NA, SRMR=NA
    ))
  }

  # Ambil fit indeks
  idx <- fitMeasures(fit,
                     c("chisq","df","cfi","tli","rmsea","srmr"))

  cat(sprintf("\n=== %s ===\n", nama_instrumen))
  cat(sprintf("  chi_sq : %.3f\n",  idx["chisq"]))
  cat(sprintf("  df     : %.0f\n",  idx["df"]))
  cat(sprintf("  CFI    : %.3f\n",  idx["cfi"]))
  cat(sprintf("  TLI    : %.3f\n",  idx["tli"]))
  cat(sprintf("  RMSEA  : %.3f\n",  idx["rmsea"]))
  cat(sprintf("  SRMR   : %.3f\n",  idx["srmr"]))

  list(
    fit = fit,
    tabel = data.frame(
      Instrumen     = nama_instrumen,
      Jumlah_Faktor = n_faktor,
      chi_sq        = round(idx["chisq"], 3),
      df            = as.integer(idx["df"]),
      CFI           = round(idx["cfi"],   3),
      TLI           = round(idx["tli"],   3),
      RMSEA         = round(idx["rmsea"], 3),
      SRMR          = round(idx["srmr"],  3),
      row.names     = NULL
    )
  )
}

# ---- Run CFA ----
res <- list(
  MBT      = jalankan_cfa(score_MBT,      model_MBT,      "MBT",      6),
  FMCE     = jalankan_cfa(score_FMCE,     model_FMCE,     "FMCE",     7),
  FCI1     = jalankan_cfa(score_FCI1,     model_FCI1,     "FCI1",     5),
  FCI2     = jalankan_cfa(score_FCI2,     model_FCI2,     "FCI2",     5),
  EMCS     = jalankan_cfa(score_EMCS,     model_EMCS,     "EMCS",     2),
  RRMCS    = jalankan_cfa(score_RRMCS,    model_RRMCS,    "RRMCS",    8),
  FLIPCOIN = jalankan_cfa(score_FLIPCOIN, model_FLIPCOIN, "FLIPCOIN", 8),
  CLASS    = jalankan_cfa(score_CLASS,    model_CLASS,    "CLASS",    8, is_likert = TRUE))
tabel_fit <- bind_rows(lapply(res, function(x) if (is.null(x$tabel)) x else x$tabel))
tabel_fit$N <- as.integer(samples[c("MBT", "FMCE", "FCI1", "FCI2", "EMCS", "RRMCS", "FliP_CoIn", "CLASS")])
cat("\nFit indices (Table 6):\n"); print(tabel_fit)
write.csv(tabel_fit, file.path(OUT_DIR, "cfa_fit_indices.csv"), row.names = FALSE)

loadings <- bind_rows(lapply(names(res), function(nm) {
  f <- res[[nm]]$fit
  if (is.null(f)) return(NULL)
  parameterEstimates(f, standardized = TRUE) %>% filter(op == "=~") %>%
    transmute(instrument = nm, factor = lhs, item = rhs, std_loading = round(std.all, 3))
}))
write.csv(loadings, file.path(OUT_DIR, "cfa_loadings.csv"), row.names = FALSE)

# ---- Reliability on the CFA samples (alpha with check.keys = TRUE, omega total with poly = TRUE) ----
hitung_reliabilitas <- function(data, nama) {
  x <- data %>% select(where(is.numeric))
  a <- tryCatch(suppressWarnings(psych::alpha(x, check.keys = TRUE)), error = function(e) NULL)
  o <- tryCatch(suppressWarnings(psych::omega(x, nfactors = 1, poly = TRUE, plot = FALSE)), error = function(e) NULL)
  data.frame(instrument = nama, N = nrow(x), n_columns = ncol(x),
             alpha = if (is.null(a)) NA else round(a$total$raw_alpha, 3),
             omega_total = if (is.null(o)) NA else round(o$omega.tot, 3))
}
rel <- bind_rows(
  hitung_reliabilitas(score_MBT, "MBT"), hitung_reliabilitas(score_FMCE, "FMCE"),
  hitung_reliabilitas(score_FCI1, "FCI1"), hitung_reliabilitas(score_FCI2, "FCI2"),
  hitung_reliabilitas(score_EMCS, "EMCS"), hitung_reliabilitas(score_RRMCS, "RRMCS"),
  hitung_reliabilitas(score_FLIPCOIN, "FliP-CoIn"), hitung_reliabilitas(score_CLASS, "CLASS"))
cat("\nReliability on the CFA samples:\n"); print(rel)
write.csv(rel, file.path(OUT_DIR, "cfa_sample_reliability.csv"), row.names = FALSE)
# The MBT reliability in Table 7 (N = 906) comes from R/02_reliability_mbt.R, not from this CFA subset.

writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info_cfa.txt"))
