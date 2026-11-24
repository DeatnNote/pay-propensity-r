# Rscript R/run_analysis.R   -> results/*.csv, results/results.json, results/*.png
suppressPackageStartupMessages({ library(data.table); library(pROC); library(ggplot2); library(jsonlite) })
source("R/functions.R")
dir.create("results", showWarnings = FALSE)

d <- load_accounts()
res <- list(accounts = nrow(d), default_rate_oct = mean(d$default_oct))

# ------------------------------------------------------------------ 1. October payment: who is likely to pay?
oct <- snapshot(d, 7)
set.seed(42)
oct[, split := "dev"]
oct[oct[, .I[sample(.N, round(0.3 * .N))], by = y]$V1, split := "test"]   # stratified 70/30
dev <- oct[split == "dev"]; test <- oct[split == "test"]

m1 <- glm(model_formula(), binomial, dev)
m2 <- glm(model_formula(PROTECTED), binomial, dev)
test[, `:=`(p_rule = behind_1 + n_behind_3m / 10,        # rule: months behind last month, ties by count
            p_m1 = predict(m1, test, type = "response"),
            p_m2 = predict(m2, test, type = "response"))]

r_rule <- roc(test$y, test$p_rule, quiet = TRUE, direction = "<")
r_m1 <- roc(test$y, test$p_m1, quiet = TRUE, direction = "<")
r_m2 <- roc(test$y, test$p_m2, quiet = TRUE, direction = "<")
auc_ci <- function(r) round(as.numeric(ci.auc(r, method = "delong")), 4)
res$october <- list(
  dev_accounts = nrow(dev), test_accounts = nrow(test), test_default_rate = mean(test$y),
  auc = list(rule = auc_ci(r_rule), model = auc_ci(r_m1), model_plus_demographics = auc_ci(r_m2)),
  model_vs_rule_p = roc.test(r_m1, r_rule, method = "delong")$p.value,
  demographics_vs_model_p = roc.test(r_m2, r_m1, method = "delong")$p.value,
  brier = list(model = mean((test$p_m1 - test$y)^2), base_rate = mean((mean(dev$y) - test$y)^2)),
  ks = ks_stat(test$p_m1, test$y),
  capture = list(rule_top10 = capture_at(test$p_rule, test$y, 0.10), model_top10 = capture_at(test$p_m1, test$y, 0.10),
                 rule_top20 = capture_at(test$p_rule, test$y, 0.20), model_top20 = capture_at(test$p_m1, test$y, 0.20)),
  observed_over_expected = mean(test$y) / mean(test$p_m1)
)
cal <- calibration_table(test$p_m1, test$y)
fwrite(cal, "results/calibration_oct_test.csv")
res$october$max_decile_gap_pts <- max(abs(cal$predicted_default - cal$observed_default)) * 100

band_of <- make_bands(1 - predict(m1, dev, type = "response"))
test[, band := band_of(1 - p_m1)]
bands <- test[, .(accounts = .N, share = .N / nrow(test), paid_rate = 1 - mean(y),
                  mean_p_pay = 1 - mean(p_m1)), keyby = band]
fwrite(bands, "results/score_bands_oct_test.csv")
res$october$bands <- bands

coefs <- as.data.table(summary(m1)$coefficients, keep.rownames = "term")
setnames(coefs, c("term", "estimate", "std_error", "z", "p"))
fwrite(coefs, "results/model_coefficients.csv")

# ------------------------------------------------------------------ 2. Monthly monitoring (real, out of time)
# Develop on July, then score August, September and October as a monthly production run would.
snaps <- rbindlist(lapply(4:7, function(t) snapshot(d, t)))
jul <- snaps[month == "Jul"]
mm <- glm(model_formula(), binomial, jul)
snaps[, p := predict(mm, snaps, type = "response")]
base <- snaps[month == "Jul"]
mon <- snaps[, {
  r <- roc(y, p, quiet = TRUE, direction = "<")
  list(label = if (.BY$month == "Oct") "default flag (issuer)" else "2+ months behind",
       accounts = .N, observed = mean(y), predicted = mean(p), o_e = mean(y) / mean(p),
       auc = as.numeric(auc(r)), capture_top10 = capture_at(p, y, 0.1),
       psi_score = psi(base$p, p),
       psi_status_1 = psi_cat(base$raw_status_1, raw_status_1),
       psi_util_1 = psi(base$util_1, util_1), psi_pay_ratio_1 = psi(base$pay_ratio_1, pay_ratio_1),
       share_status_1_eq_1 = mean(raw_status_1 == 1))
}, by = month]
mon[, `:=`(score_flag = psi_flag(psi_score), status_flag = psi_flag(psi_status_1))]
fwrite(mon, "results/monitoring.csv")
res$monitoring <- mon

# What the September coding change does to the October run
st <- d[, .(month = rep(MONTHS[1:6], each = .N), status = unlist(.SD)), .SDcols = STATUS_COL]
codes <- dcast(st[, .N, by = .(month, status)], status ~ factor(month, MONTHS[1:6]), value.var = "N", fill = 0)
fwrite(codes, "results/status_codes_by_month.csv")
o <- snaps[month == "Oct"]
res$oct_status1 <- list(accounts = sum(o$raw_status_1 == 1),
                        default_rate_status1 = mean(o[raw_status_1 == 1]$y),
                        predicted_status1 = mean(o[raw_status_1 == 1]$p),
                        default_rate_status2plus = mean(o[raw_status_1 >= 2]$y),
                        default_rate_status0 = mean(o[raw_status_1 == 0]$y))

writeLines(toJSON(res, auto_unbox = TRUE, digits = 6, pretty = TRUE), "results/results.json")

# ------------------------------------------------------------------ figures
theme_set(theme_minimal(base_size = 11))
g1 <- ggplot(cal, aes(predicted_default, observed_default)) +
  geom_abline(linetype = 2, colour = "grey60") + geom_point(size = 2.5, colour = "#1f5f8b") +
  geom_line(colour = "#1f5f8b") + coord_equal(xlim = c(0, 0.8), ylim = c(0, 0.8)) +
  labs(x = "Predicted default rate", y = "Observed default rate",
       title = "October calibration, held-out accounts (by decile)")
ggsave("results/calibration.png", g1, width = 5, height = 5, dpi = 150)

ml <- melt(mon[, .(month = factor(month, MONTHS[4:7]), AUC = auc, `Observed / expected` = o_e,
                   `Score PSI` = psi_score, `Status PSI` = psi_status_1)], id.vars = "month")
g2 <- ggplot(ml, aes(month, value, group = 1)) + geom_line(colour = "#1f5f8b") + geom_point(colour = "#1f5f8b") +
  facet_wrap(~variable, scales = "free_y", nrow = 1) + labs(x = NULL, y = NULL,
  title = "Monthly monitoring: model developed on July, scored Aug-Oct")
ggsave("results/monitoring.png", g2, width = 10, height = 3.2, dpi = 150)

print(toJSON(res[c("accounts", "default_rate_oct", "october", "oct_status1")], auto_unbox = TRUE, digits = 4, pretty = TRUE))
print(mon)
print(codes)
