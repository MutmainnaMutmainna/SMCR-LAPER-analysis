# Illustrative IRT and Random Forest analysis of FCI2 response time validity (Tables 8 and 9, Fig. 4, Supplementary Tables S4 to S9).
# INPUT. work/fci2_input.rds, created by R/00_prepare_fci2.R from the published workbook and the user-supplied answer keys.
# No answer key is read or written here. Run from the root of the code archive.
# STATUS. Adapted from the author's R notebook and run by the author on the local data (see the HTML report in notebooks/).
source("R/00_config.R")
suppressPackageStartupMessages({
  library(mirt); library(mclust); library(caret); library(randomForest)
  library(smotefamily); library(pROC); library(ggplot2); library(dplyr); library(openxlsx)
})
THRESHOLD_FAST <- RT_THRESHOLD
THRESHOLD_NORM <- 0.33    # used only for the descriptive "ambiguous" zone
FINAL_MODEL    <- "BIC"   # IRT model chosen by lowest BIC
GUESS_P        <- 1 / 5   # P(correct) under random guessing, 5 options (A-E)
POST_CUT       <- 0.50    # IRT flag: invalid if posterior P(guessing class) > 0.50
Q3_CUT         <- 0.20    # Yen (1993)
LZ_CUT         <- -1.645  # IRT flag: person misfit if lz < -1.645 (alpha = .05, one-sided)
OUT_TAG        <- "_bestBIC"

set.seed(SEED)
cat("mirt version:", as.character(packageVersion("mirt")), "\n")

# ---------- chunk ----------
stopifnot(file.exists("work/fci2_input.rds"))
inp <- readRDS("work/fci2_input.rds")
stopifnot(!is.null(inp$time_sec))
item_cols <- FCI2_ITEMS
n_item    <- length(item_cols)
N         <- length(inp$id)
time_vec  <- inp$time_sec
resp_raw  <- as.data.frame(inp$options)
stopifnot(ncol(resp_raw) == n_item)
names(resp_raw) <- item_cols     # as.data.frame() may have turned "FCI2-1" into "FCI2.1"
resp_01   <- as.data.frame(inp$scored)
names(resp_01) <- item_cols
ids       <- inp$id
cat("Respondents:", N, "(expected 605) | items:", n_item, "\n")

# ---------- chunk ----------
# 2A scoring is done in R/00_prepare_fci2.R
# 2B. Selected option, A-E -> 1-5
option_map <- c(A = 1, B = 2, C = 3, D = 4, E = 5)
resp_enc <- as.data.frame(lapply(resp_raw, function(x) unname(option_map[x])))
names(resp_enc) <- paste0(item_cols, "_enc")

# 2C. Response-time ratio, clipped at 1.0 (overtime = full time)
rt_ratio <- pmin(time_vec / TIME_LIMIT_SEC, 1)

# Descriptive zones (one definition used everywhere: [lower, upper) )
time_zone <- cut(rt_ratio, breaks = c(-Inf, THRESHOLD_FAST, THRESHOLD_NORM, Inf),
                 labels = c("fast", "ambiguous", "normal"), right = FALSE)

# 2D. Proportion correct
prop_correct <- rowMeans(resp_01, na.rm = TRUE)

# 2E. Distractor entropy (Shannon, log2) over incorrectly answered items
entropy_distractor <- sapply(seq_len(N), function(i) {
  wrong <- as.character(resp_raw[i, resp_01[i, ] == 0 & !is.na(resp_01[i, ])])
  wrong <- wrong[!is.na(wrong)]
  if (length(wrong) == 0) return(0)
  p <- table(wrong) / length(wrong)
  -sum(p * log2(p))
})

cat("Overtime respondents clipped to 1.0:", sum(time_vec > TIME_LIMIT_SEC, na.rm = TRUE), "\n")

# ---------- chunk ----------
print(table(time_zone, useNA = "ifany"))

# ---------- chunk ----------
cat("Proportion correct: mean =", round(mean(prop_correct), 3), "\n")

# ---------- chunk ----------
cat("Distractor entropy: mean =", round(mean(entropy_distractor), 3),
    "| max =", round(max(entropy_distractor), 3), "\n")

# ---------- chunk ----------
fit_safe <- function(expr, label) {
  tryCatch(expr, error = function(e) {
    cat("  WARNING", label, "failed:", conditionMessage(e), "\n"); NULL })
}

mod_1pl <- fit_safe(mirt(resp_01, 1, itemtype = "Rasch", verbose = FALSE), "1PL")
mod_2pl <- fit_safe(mirt(resp_01, 1, itemtype = "2PL",   verbose = FALSE), "2PL")

syn_3pl <- paste0("F1 = 1-", n_item, "\n",
                  "PRIOR = (1-", n_item, ", g, expbeta, 5, 17)")
mod_3pl <- fit_safe(mirt(resp_01, mirt.model(syn_3pl), itemtype = "3PL",
                         verbose = FALSE, technical = list(NCYCLES = 2000)), "3PL")

syn_4pl <- paste0(syn_3pl, "\n",
                  "PRIOR = (1-", n_item, ", u, expbeta, 19, 1)")
mod_4pl <- fit_safe(mirt(resp_01, mirt.model(syn_4pl), itemtype = "4PL",
                         verbose = FALSE, technical = list(NCYCLES = 2000)), "4PL")

mod_list <- Filter(Negate(is.null),
                   list("1PL" = mod_1pl, "2PL" = mod_2pl, "3PL" = mod_3pl, "4PL" = mod_4pl))

irt_compare <- do.call(rbind, lapply(names(mod_list), function(m) {
  x <- mod_list[[m]]
  data.frame(Model = m,
             npar  = extract.mirt(x, "nest"),
             logLik = round(extract.mirt(x, "logLik"), 2),
             AIC   = round(extract.mirt(x, "AIC"), 2),
             BIC   = round(extract.mirt(x, "BIC"), 2),
             converged = extract.mirt(x, "converged"))
}))
irt_compare$dAIC <- round(irt_compare$AIC - min(irt_compare$AIC), 2)
irt_compare$dBIC <- round(irt_compare$BIC - min(irt_compare$BIC), 2)
print(irt_compare, row.names = FALSE)

# ---------- chunk ----------
# LRT only for models WITHOUT priors (1PL vs 2PL). With priors (3PL/4PL)
# the likelihood-ratio test is not valid, so AIC/BIC are used instead.
if (!is.null(mod_1pl) && !is.null(mod_2pl)) {
  lr  <- 2 * (extract.mirt(mod_2pl, "logLik") - extract.mirt(mod_1pl, "logLik"))
  ldf <- extract.mirt(mod_2pl, "nest") - extract.mirt(mod_1pl, "nest")
  cat(sprintf("\nLRT 1PL vs 2PL: chi2 = %.2f, df = %d, p = %.4g\n",
              lr, ldf, pchisq(lr, ldf, lower.tail = FALSE)))
}

# ---------- chunk ----------
best_bic <- irt_compare$Model[which.min(irt_compare$BIC)]
cat("\nBest by AIC:", irt_compare$Model[which.min(irt_compare$AIC)],
    "| Best by BIC:", best_bic, "\n")

# ---------- chunk ----------
# Is the 3PL conducive? (fitted, converged, plausible guessing parameters)
conducive_3pl <- FALSE
if (!is.null(mod_3pl)) {
  g_est <- coef(mod_3pl, IRTpars = TRUE, simplify = TRUE)$items[, "g"]
  conducive_3pl <- isTRUE(extract.mirt(mod_3pl, "converged")) && all(g_est < 0.50)
  cat(sprintf("\n3PL check: converged = %s | c (g): mean = %.3f, min = %.3f, max = %.3f | items with c > 0.35 = %d\n",
              extract.mirt(mod_3pl, "converged"), mean(g_est), min(g_est), max(g_est), sum(g_est > 0.35)))
  cat(sprintf("3PL vs best-BIC model: dAIC = %.2f, dBIC = %.2f\n",
              irt_compare$dAIC[irt_compare$Model == "3PL"], irt_compare$dBIC[irt_compare$Model == "3PL"]))
}

# ---------- chunk ----------
final_name <- if (FINAL_MODEL == "3PL" && conducive_3pl) "3PL" else
              if (FINAL_MODEL %in% c("2PL", "3PL") && FINAL_MODEL != "3PL") FINAL_MODEL else best_bic
if (FINAL_MODEL == "3PL" && !conducive_3pl)
  cat("WARNING 3PL not conducive (not fitted, not converged, or c >= 0.50) -> using", final_name, "\n")
mod_final <- mod_list[[final_name]]
cat("FINAL MODEL used in Steps 4-5b:", final_name, "\n")

# ---------- chunk ----------
item_par <- as.data.frame(coef(mod_final, IRTpars = TRUE, simplify = TRUE)$items)
item_par <- data.frame(Item = rownames(item_par), round(item_par, 3), row.names = NULL)
head(item_par)

# ---------- chunk ----------
q3_run <- function(mod) {
  Q3 <- as.matrix(mirt::residuals(mod, type = "Q3", verbose = FALSE))
  diag(Q3) <- NA
  ut <- which(upper.tri(Q3), arr.ind = TRUE)
  pr <- data.frame(Item_1 = colnames(Q3)[ut[, 1]], Item_2 = colnames(Q3)[ut[, 2]], Q3 = Q3[ut])
  m  <- mean(pr$Q3)
  pr$Q3_star <- pr$Q3 - m                   # Christensen et al. (2017)
  pr$Abs_Q3  <- abs(pr$Q3)
  pr$Flag    <- ifelse(pr$Abs_Q3 > Q3_CUT, "Yes", "No")
  pr <- pr[order(-pr$Abs_Q3), ]
  sm <- c(nrow(pr), sprintf("%.3f", m), sprintf("%.3f", -1 / (n_item - 1)),
          sprintf("%.3f", sd(pr$Q3)), sprintf("%.3f", min(pr$Q3)), sprintf("%.3f", max(pr$Q3)),
          sprintf("%.3f", max(pr$Abs_Q3)), paste(pr$Item_1[1], "-", pr$Item_2[1]),
          sum(pr$Abs_Q3 > Q3_CUT), sum(pr$Abs_Q3 > 0.30), sum(pr$Q3_star > Q3_CUT))
  pr[c("Q3", "Q3_star", "Abs_Q3")] <- round(pr[c("Q3", "Q3_star", "Abs_Q3")], 3)
  list(pairs = pr, summary = sm)
}

q3_models <- unique(c(final_name, best_bic))
q3_res    <- lapply(setNames(q3_models, q3_models), function(m) q3_run(mod_list[[m]]))
q3_pairs  <- q3_res[[final_name]]$pairs

q3_summary <- data.frame(
  Statistic = c("Item pairs", "Mean Q3", "Expected mean -1/(k-1)", "SD Q3", "Min Q3",
                "Max Q3", "Max |Q3|", "Pair with max |Q3|",
                paste0("Pairs |Q3| > ", Q3_CUT), "Pairs |Q3| > 0.30", paste0("Pairs Q3* > ", Q3_CUT)),
  sapply(q3_res, `[[`, "summary"), check.names = FALSE)
print(q3_summary, row.names = FALSE)

# ---------- chunk ----------
head(q3_pairs, 10)

# ---------- chunk ----------
pf <- personfit(mod_final, method = "MAP")
lz_vec     <- pf$Zh
infit_vec  <- pf$infit
outfit_vec <- pf$outfit

fs <- fscores(mod_final, method = "MAP", full.scores.SE = TRUE)

# ---------- chunk ----------
theta_vec <- as.vector(fs[, 1])
theta_se  <- as.vector(fs[, 2])

cat("Person-fit computed from:", final_name, "\n")

# ---------- chunk ----------
cat("Missing values -> lz:", sum(is.na(lz_vec)), "| infit:", sum(is.na(infit_vec)),
    "| outfit:", sum(is.na(outfit_vec)), "\n")

# ---------- chunk ----------
cat("lz: mean =", round(mean(lz_vec, na.rm = TRUE), 3),
    "| lz < -2:", sum(lz_vec < -2, na.rm = TRUE), "\n")

# ---------- chunk ----------
cat("infit > 1.5:", sum(infit_vec > 1.5, na.rm = TRUE),
    "| outfit > 1.5:", sum(outfit_vec > 1.5, na.rm = TRUE), "\n")

# ---------- chunk ----------
cat("theta: mean =", round(mean(theta_vec), 3), "| SD =", round(sd(theta_vec), 3), "\n")

# ---------- chunk ----------
theta_grid <- seq(-6, 6, length.out = 121)
w_grid     <- dnorm(theta_grid); w_grid <- w_grid / sum(w_grid)

# P(correct | theta) for every item on the grid, from the fitted final model
P_grid <- sapply(seq_len(n_item), function(j)
  probtrace(extract.item(mod_final, j), matrix(theta_grid))[, 2])
P_grid <- pmin(pmax(P_grid, 1e-6), 1 - 1e-6)                      # G x J

X  <- as.matrix(resp_01)
X1 <- ifelse(is.na(X), 0, X)          # correct indicators (missing ignored)
X0 <- ifelse(is.na(X), 0, 1 - X)      # incorrect indicators

log_sum_exp <- function(v) { m <- max(v); m + log(sum(exp(v - m))) }
ll_grid  <- X1 %*% t(log(P_grid)) + X0 %*% t(log(1 - P_grid))     # N x G
ll_model <- apply(sweep(ll_grid, 2, log(w_grid), "+"), 1, log_sum_exp)
ll_guess <- rowSums(X1) * log(GUESS_P) + rowSums(X0) * log(1 - GUESS_P)

# EM for the proportion of the guessing class (item parameters fixed)
pi_g <- 0.10
for (it in 1:1000) {
  a <- log(pi_g) + ll_guess; b <- log(1 - pi_g) + ll_model; m <- pmax(a, b)
  p_guess <- exp(a - m) / (exp(a - m) + exp(b - m))
  if (abs(mean(p_guess) - pi_g) < 1e-8) break
  pi_g <- mean(p_guess)
}
label_irt <- factor(ifelse(p_guess > POST_CUT, "invalid", "valid"), levels = c("valid", "invalid"))

cat(sprintf("Model: %s | EM iterations: %d | estimated guessing-class proportion = %.3f\n",
            final_name, it, pi_g))

# ---------- chunk ----------
cat(sprintf("IRT flag (posterior > %.2f): %d respondents (%.1f%%)\n",
            POST_CUT, sum(label_irt == "invalid"), mean(label_irt == "invalid") * 100))

# ---------- chunk ----------
# -- Relation between response time and guessing indication --
ct_rt    <- cor.test(rt_ratio, p_guess, method = "spearman", exact = FALSE)
ct_theta <- cor.test(theta_vec, p_guess, method = "spearman", exact = FALSE)
cat(sprintf("\nSpearman rho (RT ratio, P(guessing)) = %.3f, p = %.3g\n", ct_rt$estimate, ct_rt$p.value))

# ---------- chunk ----------
cat(sprintf("Spearman rho (theta,    P(guessing)) = %.3f, p = %.3g\n", ct_theta$estimate, ct_theta$p.value))

# ---------- chunk ----------
guess_by_zone <- data.frame(zone = time_zone, p_guess, flag = label_irt == "invalid") |>
  group_by(zone) |>
  summarise(n = n(), mean_P_guess = round(mean(p_guess), 3),
            median_P_guess = round(median(p_guess), 3),
            n_flagged = sum(flag), pct_flagged = round(mean(flag) * 100, 1),
            .groups = "drop") |>
  as.data.frame()
kw <- kruskal.test(p_guess ~ time_zone)
print(guess_by_zone, row.names = FALSE)

# ---------- chunk ----------
cat(sprintf("Kruskal-Wallis across RT zones: chi2 = %.2f, df = %d, p = %.3g\n",
            kw$statistic, kw$parameter, kw$p.value))

# ---------- chunk ----------
guess_rt_stats <- data.frame(
  Statistic = c("Final IRT model", "Guessing-class proportion (EM)",
                "Flagged by IRT (posterior > cut)", "Spearman rho RT ratio vs P(guess)", "p",
                "Spearman rho theta vs P(guess)", "p", "Kruskal-Wallis chi2 (RT zones)", "p"),
  Value = c(final_name, sprintf("%.3f", pi_g), sum(label_irt == "invalid"),
            sprintf("%.3f", ct_rt$estimate), sprintf("%.3g", ct_rt$p.value),
            sprintf("%.3f", ct_theta$estimate), sprintf("%.3g", ct_theta$p.value),
            sprintf("%.2f", kw$statistic), sprintf("%.3g", kw$p.value)))

# ---------- chunk ----------
label_target <- factor(ifelse(rt_ratio < THRESHOLD_FAST, "invalid", "valid"),
                       levels = c("valid", "invalid"))
print(table(label_target))

# ---------- chunk ----------
cat("\nZone x label (ambiguous zone is valid by definition):\n")

# ---------- chunk ----------
print(table(zone = time_zone, label = label_target))

# ---------- chunk ----------
cat("\nDirection check (invalid vs valid):\n")

# ---------- chunk ----------
chk <- data.frame(
  variable = c("RT ratio", "lz", "outfit", "prop_correct"),
  invalid  = c(mean(rt_ratio[label_target == "invalid"]),
               mean(lz_vec[label_target == "invalid"], na.rm = TRUE),
               mean(outfit_vec[label_target == "invalid"], na.rm = TRUE),
               mean(prop_correct[label_target == "invalid"])),
  valid    = c(mean(rt_ratio[label_target == "valid"]),
               mean(lz_vec[label_target == "valid"], na.rm = TRUE),
               mean(outfit_vec[label_target == "valid"], na.rm = TRUE),
               mean(prop_correct[label_target == "valid"])))
chk[, 2:3] <- round(chk[, 2:3], 3)
print(chk, row.names = FALSE)

# ---------- chunk ----------
fast    <- factor(ifelse(label_target == "invalid", "fast", "not fast"), levels = c("fast", "not fast"))
guessed <- factor(ifelse(label_irt == "invalid", "guessing", "not guessing"),
                  levels = c("guessing", "not guessing"))
tab_fg <- table(RT = fast, IRT = guessed)
print(tab_fg)

# ---------- chunk ----------
pct <- function(x) sprintf("%.1f%%", 100 * x)
p_guess_given_fast    <- tab_fg["fast", "guessing"] / sum(tab_fg["fast", ])
p_guess_given_notfast <- tab_fg["not fast", "guessing"] / sum(tab_fg["not fast", ])
p_fast_given_guess    <- if (sum(tab_fg[, "guessing"]) > 0)
                           tab_fg["fast", "guessing"] / sum(tab_fg[, "guessing"]) else NA
fis <- fisher.test(tab_fg)

cat("\nDirection 1 - of FAST respondents, flagged as guessing      :", pct(p_guess_given_fast), "\n")

# ---------- chunk ----------
cat("              of NOT-FAST respondents, flagged as guessing  :", pct(p_guess_given_notfast), "\n")

# ---------- chunk ----------
cat("Direction 2 - of respondents flagged as guessing, were fast :", pct(p_fast_given_guess), "\n")

# ---------- chunk ----------
cat(sprintf("Fisher exact test: OR = %.2f (95%% CI %.2f-%.2f), p = %.3g\n",
            fis$estimate, fis$conf.int[1], fis$conf.int[2], fis$p.value))

# ---------- chunk ----------
# -- At comparable score levels (number correct bands; chance = 6 of 30) --
n_correct  <- rowSums(resp_01, na.rm = TRUE)
score_band <- cut(n_correct, breaks = c(-Inf, 6, 9, 12, 15, Inf),
                  labels = c("0-6 (<= chance)", "7-9", "10-12", "13-15", "16+"))
by_band <- data.frame(score_band, fast, p_guess, flag = guessed == "guessing") |>
  group_by(score_band, fast) |>
  summarise(n = n(), mean_P_guess = round(mean(p_guess), 3),
            pct_flagged = round(mean(flag) * 100, 1), .groups = "drop") |>
  as.data.frame()
print(by_band, row.names = FALSE)

# ---------- chunk ----------
mh <- tryCatch(mantelhaen.test(table(fast, guessed, score_band)),
               error = function(e) NULL)
if (!is.null(mh)) {
  cat(sprintf("Mantel-Haenszel (fast x guessing, controlling score band): chi2 = %.2f, p = %.3g, common OR = %.2f\n",
              mh$statistic, mh$p.value, mh$estimate))
} else cat("Mantel-Haenszel not computable (sparse strata) - interpret the band table descriptively.\n")

# ---------- chunk ----------
fast_guess_stats <- data.frame(
  Statistic = c("IRT model", "Fast respondents (RT < threshold)", "Flagged as guessing (IRT)",
                "P(guessing | fast)", "P(guessing | not fast)", "P(fast | guessing)",
                "Fisher OR", "Fisher OR 95% CI", "Fisher p",
                "Mantel-Haenszel common OR (score-controlled)", "Mantel-Haenszel p",
                "Spearman rho RT ratio vs P(guess)"),
  Value = c(final_name, sum(fast == "fast"), sum(guessed == "guessing"),
            pct(p_guess_given_fast), pct(p_guess_given_notfast), pct(p_fast_given_guess),
            sprintf("%.2f", fis$estimate), sprintf("%.2f-%.2f", fis$conf.int[1], fis$conf.int[2]),
            sprintf("%.3g", fis$p.value),
            if (is.null(mh)) NA else sprintf("%.2f", mh$estimate),
            if (is.null(mh)) NA else sprintf("%.3g", mh$p.value),
            sprintf("%.3f", ct_rt$estimate)))

# ---------- chunk ----------
gmm_df <- data.frame(lz = lz_vec, infit = infit_vec, outfit = outfit_vec, rt = rt_ratio)
# Neutral values for missing person-fit: lz = 0, infit/outfit = 1
gmm_df$lz[is.na(gmm_df$lz)]         <- 0
gmm_df$infit[is.na(gmm_df$infit)]   <- 1
gmm_df$outfit[is.na(gmm_df$outfit)] <- 1

set.seed(SEED)
gmm_fit <- Mclust(scale(gmm_df), G = 2, verbose = FALSE)

gmm_profile <- aggregate(gmm_df, by = list(cluster = gmm_fit$classification), FUN = mean)
gmm_profile$n   <- as.vector(table(gmm_fit$classification))
gmm_profile$pct <- round(gmm_profile$n / N * 100, 1)
cat("GMM: G = 2 | model =", gmm_fit$modelName, "| BIC =", round(gmm_fit$bic, 2), "\n\n")

# ---------- chunk ----------
print(cbind(gmm_profile[1], round(gmm_profile[2:5], 3), gmm_profile[6:7]), row.names = FALSE)

# ---------- chunk ----------
cat("\nGMM cluster x RT label:\n")

# ---------- chunk ----------
print(table(cluster = gmm_fit$classification, RT_label = label_target))

# ---------- chunk ----------
n_invalid <- sum(label_target == "invalid"); n_valid <- sum(label_target == "valid")
cat("valid:", n_valid, "| invalid:", n_invalid,
    "| ratio minority/majority =", round(n_invalid / n_valid, 3), "\n")

# ---------- chunk ----------
cat("SMOTE is applied inside training folds only (Step 9).\n")

# ---------- chunk ----------
feat_df <- data.frame(resp_01, resp_enc,
                      prop_correct = prop_correct,
                      entropy_distr = entropy_distractor)
feat_df <- as.data.frame(lapply(feat_df, as.numeric))
feat_df[is.na(feat_df)] <- 0   # missing item response -> 0 (incorrect / no option)

set.seed(SEED)
idx_train <- createDataPartition(label_target, p = 0.70, list = FALSE)
X_train <- feat_df[idx_train, ];  y_train <- label_target[idx_train]
X_test  <- feat_df[-idx_train, ]; y_test  <- label_target[-idx_train]

smote_train_only <- function(x, y) {
  d <- smotefamily::SMOTE(X = as.data.frame(x), target = as.integer(y == "invalid"),
                          K = 5, dup_size = 0)$data
  y_new <- factor(ifelse(as.character(d$class) == "1", "invalid", "valid"),
                  levels = levels(y))
  d$class <- NULL
  list(x = d, y = y_new)
}

ctrl_rf <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                        summaryFunction = twoClassSummary, savePredictions = "final",
                        sampling = list(name = "SMOTE", func = smote_train_only, first = TRUE))

p_feat <- ncol(X_train)
set.seed(SEED)
mtry_pool <- seq(2, min(p_feat, floor(sqrt(p_feat) * 4)))
tune_grid <- data.frame(mtry = sort(sample(mtry_pool, min(20, length(mtry_pool)))))

set.seed(SEED)
mod_rf <- train(x = X_train, y = y_train, method = "rf", metric = "ROC",
                trControl = ctrl_rf, tuneGrid = tune_grid, ntree = 300, importance = TRUE)

best_mtry <- mod_rf$bestTune$mtry
cv_best   <- mod_rf$results[mod_rf$results$mtry == best_mtry, ]

prob_test <- predict(mod_rf, X_test, type = "prob")[, "invalid"]
pred_test <- predict(mod_rf, X_test)
cm_rf     <- confusionMatrix(pred_test, y_test, positive = "invalid")
roc_test  <- roc(y_test, prob_test, levels = c("valid", "invalid"), direction = "<", quiet = TRUE)
auc_ci    <- as.numeric(ci.auc(roc_test))

eval_rf <- data.frame(
  Metric = c("Accuracy", "Balanced Accuracy", "Sensitivity", "Specificity",
             "Precision", "F1", "AUC-ROC", "AUC 95% CI", "Kappa", "CV ROC (train)"),
  Value  = c(sprintf("%.3f", c(cm_rf$overall["Accuracy"], cm_rf$byClass["Balanced Accuracy"],
                               cm_rf$byClass["Sensitivity"], cm_rf$byClass["Specificity"],
                               cm_rf$byClass["Precision"], cm_rf$byClass["F1"], auc_ci[2])),
             sprintf("%.3f-%.3f", auc_ci[1], auc_ci[3]),
             sprintf("%.3f", c(cm_rf$overall["Kappa"], cv_best$ROC))))

cat("Train:", nrow(X_train), "| Test:", nrow(X_test), "(real respondents only)\n")

# ---------- chunk ----------
cat("Features:", p_feat, "(no rt_ratio) | best mtry:", best_mtry, "\n\n")

# ---------- chunk ----------
print(eval_rf, row.names = FALSE)

# ---------- chunk ----------
print(cm_rf$table)

# ---------- chunk ----------
imp_df <- data.frame(Feature = rownames(varImp(mod_rf)$importance),
                     Importance = varImp(mod_rf)$importance[, "invalid"]) |>
  arrange(desc(Importance))
head(imp_df, 15)

# ---------- chunk ----------
set.seed(SEED)
folds_all <- createFolds(label_target, k = 5)
prob_oof  <- rep(NA_real_, N)
mtry_fold <- integer(length(folds_all))
for (k in seq_along(folds_all)) {
  te <- folds_all[[k]]
  set.seed(SEED + k)
  inner <- train(x = feat_df[-te, ], y = label_target[-te], method = "rf", metric = "ROC",
                 trControl = ctrl_rf, tuneGrid = tune_grid, ntree = 300)
  mtry_fold[k] <- inner$bestTune$mtry
  prob_oof[te] <- predict(inner, feat_df[te, ], type = "prob")[, "invalid"]
  cat(sprintf("  outer fold %d: n test = %d, mtry = %d\n", k, length(te), mtry_fold[k]))
}

# ---------- chunk ----------
label_rf <- factor(ifelse(prob_oof >= 0.5, "invalid", "valid"), levels = c("valid", "invalid"))

cm_oof     <- confusionMatrix(label_rf, label_target, positive = "invalid")
table_comp <- table(RT_label = label_target, RF_oof = label_rf)
concord    <- mean(label_rf == label_target)
kappa_oof  <- as.numeric(cm_oof$overall["Kappa"])
print(table_comp)

# ---------- chunk ----------
cat("\nConcordance:", round(concord * 100, 1), "% | Kappa:", round(kappa_oof, 3), "\n")

# ---------- chunk ----------
idx_clean_conservative <- which(label_target == "valid" & label_rf == "valid")
idx_clean_lenient      <- which(label_target == "valid" | label_rf == "valid")
df_clean <- data.frame(id = ids[idx_clean_conservative])
cat("Clean dataset: conservative =", length(idx_clean_conservative),
    "| lenient =", length(idx_clean_lenient), "\n")

# ---------- chunk ----------
label_lz <- factor(ifelse(!is.na(lz_vec) & lz_vec < LZ_CUT, "invalid", "valid"),
                   levels = c("valid", "invalid"))

agree_stats <- function(pred, score, ref, method) {
  cm  <- confusionMatrix(pred, ref, positive = "invalid")
  r   <- roc(ref, score, levels = c("valid", "invalid"), direction = "<", quiet = TRUE)
  ci  <- as.numeric(ci.auc(r))
  data.frame(Method = method,
             Flagged_invalid = sum(pred == "invalid"),
             Concordance = round(mean(pred == ref), 3),
             Kappa = round(cm$overall["Kappa"], 3),
             Sensitivity = round(cm$byClass["Sensitivity"], 3),
             Specificity = round(cm$byClass["Specificity"], 3),
             Balanced_Accuracy = round(cm$byClass["Balanced Accuracy"], 3),
             Precision = round(cm$byClass["Precision"], 3),
             F1 = round(cm$byClass["F1"], 3),
             AUC = round(ci[2], 3),
             AUC_95CI = sprintf("%.3f-%.3f", ci[1], ci[3]),
             row.names = NULL)
}

score_lz <- -lz_vec
score_lz[is.na(score_lz)] <- median(score_lz, na.rm = TRUE)

irt_vs_rf <- rbind(
  agree_stats(label_irt, p_guess, label_target,
              sprintf("IRT %s guessing mixture (posterior > %.2f)", final_name, POST_CUT)),
  agree_stats(label_lz, score_lz, label_target,
              sprintf("IRT %s person-fit (lz < %.3f)", final_name, LZ_CUT)),
  agree_stats(label_rf, prob_oof, label_target, "Random forest (out-of-fold)"))
print(irt_vs_rf, row.names = FALSE)

# ---------- chunk ----------
# Confusion tables against the RT label
cat("\nIRT (guessing mixture) vs RT label:\n")

# ---------- chunk ----------
tbl_irt_rt <- table(RT_label = label_target, IRT = label_irt); print(tbl_irt_rt)

# ---------- chunk ----------
cat("\nRF vs RT label:\n")

# ---------- chunk ----------
tbl_rf_rt  <- table(RT_label = label_target, RF = label_rf); print(tbl_rf_rt)

# ---------- chunk ----------
# Statistical comparison IRT (guessing mixture) vs RF (paired, same respondents)
roc_irt <- roc(label_target, p_guess,  levels = c("valid", "invalid"), direction = "<", quiet = TRUE)
roc_rf  <- roc(label_target, prob_oof, levels = c("valid", "invalid"), direction = "<", quiet = TRUE)
delong  <- roc.test(roc_irt, roc_rf, method = "delong", paired = TRUE)

correct_irt <- factor(label_irt == label_target, levels = c(FALSE, TRUE))
correct_rf  <- factor(label_rf  == label_target, levels = c(FALSE, TRUE))
mcn <- mcnemar.test(table(IRT_correct = correct_irt, RF_correct = correct_rf))

cat(sprintf("\nDeLong test (AUC IRT vs RF): Z = %.3f, p = %.4g\n",
            delong$statistic, delong$p.value))

# ---------- chunk ----------
cat(sprintf("McNemar test (classification accuracy IRT vs RF): chi2 = %.3f, p = %.4g\n",
            mcn$statistic, mcn$p.value))

# ---------- chunk ----------
method_tests <- data.frame(
  Test = c("DeLong (paired AUC)", "McNemar (paired accuracy)"),
  Statistic = round(c(delong$statistic, mcn$statistic), 3),
  p_value = signif(c(delong$p.value, mcn$p.value), 4))

# Descriptive only (NOT validation: both use the same response data)
cat("\nIRT vs RF cross-table (descriptive):\n")

# ---------- chunk ----------
print(table(IRT = label_irt, RF = label_rf))

# ---------- chunk ----------
profile_df <- data.frame(
  RT = label_target, IRT = label_irt, RF = label_rf,
  rt_sec = rt_ratio * TIME_LIMIT_SEC, prop_correct, entropy = entropy_distractor,
  P_guess = p_guess, theta = theta_vec, lz = lz_vec, infit = infit_vec, outfit = outfit_vec)
profile_tbl <- profile_df |>
  group_by(RT, IRT, RF) |>
  summarise(n = n(), across(rt_sec:outfit, ~ round(mean(.x, na.rm = TRUE), 3)),
            pct_lz_below_minus2 = round(mean(lz < -2, na.rm = TRUE) * 100, 1),
            .groups = "drop") |>
  as.data.frame()
print(profile_tbl, row.names = FALSE)

# ---------- chunk ----------
pal2 <- c(valid = "#27AE60", invalid = "#E74C3C")

p1 <- ggplot(data.frame(rt = rt_ratio, label = label_target), aes(rt, fill = label)) +
  geom_histogram(bins = 35, alpha = .75, position = "identity") +
  geom_vline(xintercept = THRESHOLD_FAST, linetype = "dashed") +
  scale_fill_manual(values = pal2) +
  labs(title = "RT ratio by response-time label", x = "RT ratio", y = "Frequency", fill = "Label") +
  theme_minimal(base_size = 11)

p2 <- ggplot(data.frame(theta = theta_vec, rt = rt_ratio, label = label_target),
             aes(theta, rt, colour = label)) +
  geom_point(alpha = .55, size = 1.8) + scale_colour_manual(values = pal2) +
  labs(title = paste0("theta (", final_name, ") vs RT ratio"), x = "theta", y = "RT ratio") +
  theme_minimal(base_size = 11)

p3 <- ggplot(data.frame(lz = lz_vec, label = label_target), aes(label, lz, fill = label)) +
  geom_boxplot(alpha = .7) + geom_hline(yintercept = -2, linetype = "dashed", colour = "red") +
  scale_fill_manual(values = pal2) +
  labs(title = paste0("Person-fit lz (", final_name, ") by response-time label"), x = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

p4 <- head(imp_df, 15) |>
  ggplot(aes(reorder(Feature, Importance), Importance)) +
  geom_col(fill = "#1E8449") + coord_flip() +
  labs(title = "Top 15 RF features (no RT feature)", x = NULL, y = "Scaled importance") +
  theme_minimal(base_size = 10)

# Confusion matrices: IRT vs RT threshold and RF vs RT threshold (same layout)
cm_panel <- function(pred, method_label) {
  cm <- confusionMatrix(pred, label_target, positive = "invalid")
  as.data.frame(table(RT_threshold = label_target, Method_label = pred)) |>
    mutate(Panel = sprintf("%s\nConcordance %.1f%% | Kappa %.3f",
                           method_label, mean(pred == label_target) * 100, cm$overall["Kappa"]),
           lab = paste0(Freq, "\n(", round(Freq / N * 100, 1), "%)"),
           match = ifelse(as.character(RT_threshold) == as.character(Method_label),
                          "agree", "disagree"))
}
cm_df <- rbind(cm_panel(label_irt, paste0("IRT (", final_name, " guessing mixture)")),
               cm_panel(label_rf,  "Random forest (out-of-fold)"))

p5 <- ggplot(cm_df, aes(Method_label, RT_threshold, fill = match)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = lab), size = 4, fontface = "bold") +
  facet_wrap(~ Panel) +
  scale_fill_manual(values = c(agree = "#AED6F1", disagree = "#F5B7B1")) +
  labs(title = "Confusion matrices against the RT threshold: IRT vs RF",
       x = "Label assigned by the method", y = "RT-threshold label", fill = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")

# Direct cross-table IRT x RF (descriptive: both use the same response data)
p5b <- as.data.frame(table(IRT = label_irt, RF = label_rf)) |>
  mutate(lab = paste0(Freq, "\n(", round(Freq / N * 100, 1), "%)")) |>
  ggplot(aes(RF, IRT, fill = Freq)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = lab), size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#EBF5FB", high = "#1A5276") +
  labs(title = paste0("IRT (", final_name, ") vs RF labels (descriptive)"),
       subtitle = sprintf("Agreement %.1f%%", mean(label_irt == label_rf) * 100),
       x = "RF label (out-of-fold)", y = "IRT label") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

p6 <- ggplot(data.frame(pc = prop_correct, label = label_target), aes(label, pc, fill = label)) +
  geom_violin(alpha = .7) + geom_boxplot(width = .15, alpha = .5) +
  scale_fill_manual(values = pal2) +
  labs(title = "Proportion correct by response-time label", x = NULL, y = "Proportion correct") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

roc_df <- rbind(
  data.frame(FPR = 1 - roc_irt$specificities, TPR = roc_irt$sensitivities,
             Method = sprintf("IRT %s guessing (AUC = %.3f)", final_name, auc(roc_irt))),
  data.frame(FPR = 1 - roc_rf$specificities, TPR = roc_rf$sensitivities,
             Method = sprintf("RF out-of-fold (AUC = %.3f)", auc(roc_rf))))
p7 <- ggplot(roc_df, aes(FPR, TPR, colour = Method)) +
  geom_line(linewidth = 1) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  coord_equal() +
  labs(title = "Agreement with the RT threshold: IRT vs RF",
       subtitle = sprintf("DeLong p = %.3g", delong$p.value),
       x = "1 - Specificity", y = "Sensitivity", colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")

p8 <- ggplot(data.frame(rt = rt_ratio, pg = p_guess, zone = time_zone),
             aes(rt, pg, colour = zone)) +
  geom_point(alpha = .6, size = 1.8) +
  geom_vline(xintercept = THRESHOLD_FAST, linetype = "dashed") +
  geom_hline(yintercept = POST_CUT, linetype = "dotted") +
  scale_colour_manual(values = c(fast = "#E74C3C", ambiguous = "#F39C12", normal = "#27AE60")) +
  labs(title = paste0("Response time vs IRT (", final_name, ") guessing indication"),
       subtitle = sprintf("Spearman rho = %.3f", ct_rt$estimate),
       x = "RT ratio (time / recommended duration)", y = "Posterior P(guessing class)",
       colour = "RT zone") +
  theme_minimal(base_size = 11)

plots <- list(plot1_rt_ratio = p1, plot2_theta_rt = p2, plot3_lz = p3,
              plot4_rf_importance = p4, plot5_confusion_IRT_RF_vs_RT = p5,
              plot5b_IRT_vs_RF = p5b, plot6_prop_correct = p6,
              plot7_roc_irt_vs_rf = p7, plot8_rt_vs_guessing = p8)
for (nm in names(plots)) ggsave(file.path(OUT_DIR, paste0(nm, OUT_TAG, ".png")), plots[[nm]], width = 8, height = 5.5, dpi = 150)
cat("Saved:", paste0(names(plots), OUT_TAG, ".png"), sep = "\n  ")

# ---------- chunk ----------
p5; p5b; p8; p7

# ---------- chunk ----------
df_results <- data.frame(id = ids)
df_results$rt_ratio           <- round(rt_ratio, 4)
df_results$time_zone          <- as.character(time_zone)
df_results$prop_correct       <- round(prop_correct, 4)
df_results$distractor_entropy <- round(entropy_distractor, 4)
df_results$theta              <- round(theta_vec, 4)
df_results$theta_se           <- round(theta_se, 4)
df_results$lz                 <- round(lz_vec, 4)
df_results$infit              <- round(infit_vec, 4)
df_results$outfit             <- round(outfit_vec, 4)
df_results$gmm_cluster        <- gmm_fit$classification
df_results$label_rt           <- as.character(label_target)
df_results$p_guess_irt        <- round(p_guess, 4)
df_results$label_irt_guess    <- as.character(label_irt)
df_results$label_irt_lz       <- as.character(label_lz)
df_results$rf_prob_invalid    <- round(prob_oof, 4)
df_results$label_rf_oof       <- as.character(label_rf)
df_results$n_correct          <- n_correct
df_results$in_rf_train_set    <- seq_len(N) %in% idx_train

sheets <- list(All_Respondents = df_results, Clean_Dataset = df_clean,
               IRT_Comparison = irt_compare, Item_Parameters = item_par,
               Q3_Summary = q3_summary, Q3_Pairs = q3_pairs,
               IRT_Guessing_RT = guess_rt_stats, Guessing_by_RT_zone = guess_by_zone,
               Fast_vs_Guess = fast_guess_stats, Fast_vs_Guess_by_score = by_band,
               GMM_Profile = gmm_profile, RF_Test_Eval = eval_rf,
               RF_CV_Folds = mod_rf$resample, RT_vs_RF_OOF = as.data.frame(table_comp),
               IRT_vs_RF_vs_RT = irt_vs_rf, Method_Tests = method_tests,
               IRT_vs_RT_table = as.data.frame(tbl_irt_rt),
               Group_Profile = profile_tbl, RF_Importance = imp_df)

wb <- createWorkbook()
for (s in names(sheets)) {
  addWorksheet(wb, s)
  writeData(wb, s, sheets[[s]], headerStyle = createStyle(textDecoration = "bold"))
}
out_file <- file.path(OUT_DIR, paste0("results_RT_IRT_RF", OUT_TAG, ".xlsx"))
saved <- tryCatch({ saveWorkbook(wb, out_file, overwrite = TRUE); TRUE },
                  warning = function(w) FALSE, error = function(e) FALSE)
if (saved && file.exists(out_file)) {
  cat("Saved Saved:", normalizePath(out_file), "\n  Sheets:", paste(names(sheets), collapse = " | "), "\n")
} else {
  cat("WARNING NOT saved. Close", out_file, "in Excel and rerun this chunk.\n")
}

# ---------- chunk ----------
cat("N =", N, "| items =", n_item, "\n")

# ---------- chunk ----------
cat("IRT final model:", final_name, "(best AIC:", irt_compare$Model[which.min(irt_compare$AIC)],
    "| best BIC:", irt_compare$Model[which.min(irt_compare$BIC)], ")\n")

# ---------- chunk ----------
cat("Q3 (", final_name, "): max |Q3| =", q3_summary[[final_name]][7],
    "| pairs > ", Q3_CUT, "=", q3_summary[[final_name]][9], "\n")

# ---------- chunk ----------
cat("Invalid by RT threshold (RT ratio <", THRESHOLD_FAST, "):", n_invalid,
    sprintf("(%.1f%%)", n_invalid / N * 100), "\n")

# ---------- chunk ----------
cat(sprintf("Invalid by IRT %s guessing mixture: %d (%.1f%%) | rho(RT, P(guess)) = %.3f\n",
            final_name, sum(label_irt == "invalid"), mean(label_irt == "invalid") * 100, ct_rt$estimate))

# ---------- chunk ----------
cat("RF (no RT feature): test AUC =", sprintf("%.3f [%.3f-%.3f]", auc_ci[2], auc_ci[1], auc_ci[3]),
    "| balanced acc =", sprintf("%.3f", cm_rf$byClass["Balanced Accuracy"]),
    "| CV ROC =", sprintf("%.3f", cv_best$ROC), "\n")

# ---------- chunk ----------
cat("Nested-CV mtry per outer fold:", mtry_fold, "\n")

# ---------- chunk ----------
cat(sprintf("P(guessing | fast) = %s | P(fast | guessing) = %s | MH p = %s\n", pct(p_guess_given_fast), pct(p_fast_given_guess), if (is.null(mh)) "NA" else sprintf("%.3g", mh$p.value)))

# ---------- chunk ----------
cat("Out-of-fold agreement with RT label:", sprintf("%.1f%% | kappa = %.3f", concord * 100, kappa_oof), "\n")

# ---------- chunk ----------
cat("\nAgreement with RT label (N =", N, "):\n")

# ---------- chunk ----------
print(irt_vs_rf[, c("Method", "Kappa", "Balanced_Accuracy", "AUC", "AUC_95CI")], row.names = FALSE)

# ---------- chunk ----------
cat(sprintf("DeLong p = %.4g | McNemar p = %.4g\n", delong$p.value, mcn$p.value))

# ---------- chunk ----------
cat("Clean dataset (conservative):", length(idx_clean_conservative), "\n")

# ---------- chunk ----------
# ---------- chunk ----------
# Save the labels, split, and out-of-fold predictions (no answer keys, no item-level responses)
write.csv(data.frame(
  id                 = ids,
  rt_ratio           = round(rt_ratio, 4),
  label_rt           = as.character(label_target),
  p_guess_irt        = round(p_guess, 4),
  label_irt_guess    = as.character(label_irt),
  label_irt_lz       = as.character(label_lz),
  rf_prob_invalid    = round(prob_oof, 4),
  label_rf_oof       = as.character(label_rf),
  in_rf_train_set    = seq_len(N) %in% idx_train),
  file.path(OUT_DIR, "fci2_irt_rf_labels.csv"), row.names = FALSE)
write.csv(item_par, file.path(OUT_DIR, "fci2_item_parameters.csv"), row.names = FALSE)
write.csv(irt_vs_rf, file.path(OUT_DIR, "fci2_irt_vs_rf_agreement.csv"), row.names = FALSE)
write.csv(eval_rf, file.path(OUT_DIR, "fci2_rf_test_evaluation.csv"), row.names = FALSE)
cat("Saved the labels, split, predictions, item parameters, and agreement tables in", OUT_DIR, "\n")

# ---------- chunk ----------
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info_irt_rf.txt"))