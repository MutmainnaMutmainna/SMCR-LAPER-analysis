# Run the included scripts in order. Run from the root of the archive.
# Requires data/ (Mendeley workbook, version 6) and keys/answer_keys.xlsx (obtained from PhysPort).
# 06_cfa.R additionally requires data/CFA_workbook.xlsx, a local workbook with the respondent
# subsets used for the CFA. The rule used to select these respondents is not encoded in this archive.

scripts <- c(
  "R/00_prepare_fci2.R",
  "R/02_reliability_mbt.R",
  "R/05_irt_vs_rf.R",
  "R/06_cfa.R"
)

for (s in scripts) {
  cat("\n==== Running", s, "====\n")
  source(s, echo = FALSE)
}

writeLines(capture.output(sessionInfo()), "session_info.txt")
