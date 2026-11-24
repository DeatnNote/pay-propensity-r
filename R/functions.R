# Functions for the payment-propensity lab. Sourced by R/run_analysis.R, report.Rmd and the tests.
#
# Data: UCI "Default of Credit Card Clients" (Yeh & Lien, 2009). 30,000 real accounts from a Taiwanese card
# issuer, April-September 2005: monthly repayment status, bill amount and payment amount, plus whether the account
# defaulted on its October payment.
suppressPackageStartupMessages(library(data.table))

MONTHS <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
# column for month k (1 = April ... 6 = September)
STATUS_COL <- c("pay_6", "pay_5", "pay_4", "pay_3", "pay_2", "pay_0")
BILL_COL   <- paste0("bill_amt", 6:1)
PAID_COL   <- paste0("pay_amt", 6:1)
PROTECTED  <- c("age", "sex", "education", "marriage")

load_accounts <- function(path = "data/credit_card_clean.csv") {
  d <- fread(path)
  setnames(d, "default.payment.next.month", "default_oct")
  # the file one-hot encodes sex/education/marriage; collapse back to one column each
  for (g in c("sex", "education", "marriage")) {
    cols <- grep(paste0("^", g, ":"), names(d), value = TRUE)
    d[, (g) := factor(sub(".*:", "", cols)[max.col(as.matrix(.SD), ties.method = "first")]), .SDcols = cols]
    d[, (cols) := NULL]
  }
  d[, account_id := .I]
  d[]
}

# Repayment status: -2 no balance, -1 paid in full, 0 revolving (minimum paid), k = k months behind.
months_behind <- function(s) pmin(pmax(s, 0), 3)

#' Features known at the start of target month t (t = 4..7, i.e. Jul..Oct), from the three months before it.
#' Label: 2+ months behind in month t (t <= 6), or the issuer's own default flag for October (t = 7).
snapshot <- function(d, t) {
  stopifnot(t >= 4, t <= 7)
  s <- function(k) d[[STATUS_COL[t - k]]]
  bill <- function(k) d[[BILL_COL[t - k]]]
  paid <- function(k) d[[PAID_COL[t - k]]]
  out <- data.table(
    account_id   = d$account_id,
    month        = MONTHS[t],
    behind_1     = months_behind(s(1)),
    behind_2     = months_behind(s(2)),
    behind_3     = months_behind(s(3)),
    paid_full_1  = as.integer(s(1) == -1),
    no_balance_1 = as.integer(s(1) == -2),
    n_behind_3m  = (s(1) >= 1) + (s(2) >= 1) + (s(3) >= 1),
    util_1       = pmin(pmax(bill(1) / d$limit_bal, 0), 1.5),
    # payment made last month as a share of the statement before it
    pay_ratio_1  = pmin(pmax(paid(1) / pmax(bill(2), 1), 0), 1),
    log_limit    = log(d$limit_bal),
    raw_status_1 = s(1)
  )
  out[, y := if (t <= 6) as.integer(d[[STATUS_COL[t]]] >= 2) else d$default_oct]
  out[, (PROTECTED) := d[, PROTECTED, with = FALSE]]
  out[]
}

FEATURES <- c("behind_1", "behind_2", "behind_3", "paid_full_1", "no_balance_1", "n_behind_3m",
              "util_1", "pay_ratio_1", "log_limit")

model_formula <- function(extra = character()) reformulate(c(FEATURES, extra), response = "y")

#' Population stability index between a baseline and a current sample, on baseline decile bins.
psi <- function(base, cur, bins = 10, eps = 1e-4) {
  br <- unique(quantile(base, probs = seq(0, 1, length.out = bins + 1), names = FALSE, type = 7))
  if (length(br) < 3) br <- sort(unique(c(base, cur)))          # discrete variable: one bin per value
  br[1] <- -Inf; br[length(br)] <- Inf
  b <- tabulate(cut(base, br, labels = FALSE, right = TRUE), length(br) - 1) / length(base)
  c <- tabulate(cut(cur,  br, labels = FALSE, right = TRUE), length(br) - 1) / length(cur)
  b <- pmax(b, eps); c <- pmax(c, eps)
  sum((c - b) * log(c / b))
}

#' PSI for a categorical variable (e.g. raw status codes): one bin per level.
psi_cat <- function(base, cur, eps = 1e-4) {
  lv <- sort(unique(c(base, cur)))
  b <- pmax(as.numeric(table(factor(base, lv))) / length(base), eps)
  c <- pmax(as.numeric(table(factor(cur, lv))) / length(cur), eps)
  sum((c - b) * log(c / b))
}

psi_flag <- function(x) ifelse(x < 0.10, "stable", ifelse(x < 0.25, "watch", "investigate"))

#' Observed vs. predicted by score decile (decile 1 = most likely to pay).
calibration_table <- function(p_default, y, groups = 10) {
  dt <- data.table(p = p_default, y = y)
  dt[, decile := as.integer(cut(frank(p, ties.method = "first"), groups, labels = FALSE))]
  dt[, .(accounts = .N, predicted_default = mean(p), observed_default = mean(y)), keyby = decile]
}

#' Share of all defaults found by working the riskiest `share` of accounts.
capture_at <- function(p_default, y, share) {
  n <- ceiling(length(y) * share)
  sum(y[order(-p_default)][seq_len(n)]) / sum(y)
}

ks_stat <- function(p_default, y) {
  o <- order(-p_default)
  max(abs(cumsum(y[o]) / sum(y) - cumsum(1 - y[o]) / sum(1 - y)))
}

#' Score bands by likelihood to pay (1 - P(default)); cut points are fixed on the development sample.
make_bands <- function(p_pay_dev, labels = c("E", "D", "C", "B", "A")) {
  cuts <- quantile(p_pay_dev, probs = c(0.1, 0.25, 0.5, 0.75), names = FALSE)
  function(p_pay) factor(labels[findInterval(p_pay, cuts) + 1L], levels = rev(labels))
}
