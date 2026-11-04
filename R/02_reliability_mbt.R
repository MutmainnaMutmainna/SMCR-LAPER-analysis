# Reliability of the MBT (16 items, N = 906): alpha, KR-20, omega total (Table 7), and the
# six-factor Schmid-Leiman sensitivity check described in Technical Validation.

source("R/00_config.R")
source("R/helpers.R")
suppressPackageStartupMessages(library(psych))

d <- read_smcr()
keys <- read_keys()
mbt <- score_items(d, MBT_ITEMS, keys[keys$instrument == "MBT", ])
mbt <- mbt[complete.cases(mbt), ]
stopifnot(ncol(mbt) == 16)
cat("N used:", nrow(mbt), "(expected 906)\n")

# 1. Alpha and KR-20 on the same records
a <- psych::alpha(mbt, check.keys = FALSE, use = "complete.obs")
cat("alpha =", round(a$total$raw_alpha, 4), "\n")
print(kr20(mbt))

# 2. Omega total from a one-factor model on the tetrachoric correlation matrix (minres)
tet <- psych::tetrachoric(mbt)$rho
f1 <- psych::fa(tet, nfactors = 1, fm = "minres", n.obs = nrow(mbt))
lam <- as.numeric(f1$loadings)
omega_t <- sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2))
cat("omega total, one factor =", round(omega_t, 4), "\n")  # manuscript value 0.7157, reported as 0.72

# 3. Sensitivity check, Schmid-Leiman with six group factors (manuscript omega_t 0.80, omega_h 0.45)
sl <- psych::omega(mbt, nfactors = 6, fm = "minres", poly = TRUE, plot = FALSE)
cat("six-factor omega total =", round(sl$omega.tot, 4),
    " omega hierarchical =", round(sl$omega_h, 4), "\n")
print(round(sl$schmid$sl[, c("h2", "u2")], 2))  # check for communalities of 1.00 (improper solution)

# 4. Eigenvalue ratio used in the text (first to second eigenvalue of the correlation matrix)
ev <- eigen(tet)$values
cat("eigenvalue ratio =", round(ev[1] / ev[2], 2), "\n")  # manuscript value 1.75

writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info_mbt_reliability.txt"))
