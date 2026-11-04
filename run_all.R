# Run all included scripts in order. Run from the root of the archive.
# Scripts marked "to be added" in README.md are listed here once they exist.

scripts <- c(
  "R/00_prepare_fci2.R",
  "R/02_reliability_mbt.R"
  # "R/01_aiken_v.R",
  # "R/02_reliability_other.R",
  # "R/03_cfa.R",
  # "R/04_irt_assumptions.R",
  # "R/05_irt_vs_rf.R"
)

for (s in scripts) {
  cat("\n==== Running", s, "====\n")
  source(s, echo = FALSE)
}

writeLines(capture.output(sessionInfo()), "session_info.txt")
