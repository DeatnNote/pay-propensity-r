source(file.path("..", "..", "R", "functions.R"), chdir = TRUE)

test_that("PSI is zero for identical samples and grows with shift", {
  set.seed(1)
  x <- rnorm(5000)
  expect_lt(psi(x, x), 1e-12)
  expect_lt(psi(x, rnorm(5000)), 0.02)
  expect_gt(psi(x, rnorm(5000, 1)), 0.25)
  expect_equal(psi_flag(c(0.05, 0.15, 0.4)), c("stable", "watch", "investigate"))
})

test_that("categorical PSI flags a new code appearing", {
  base <- rep(c(-1, 0, 2), c(300, 600, 100))
  cur <- rep(c(-1, 0, 1, 2), c(300, 500, 150, 50))
  expect_gt(psi_cat(base, cur), 0.25)
  expect_lt(psi_cat(base, base), 1e-12)
})

test_that("snapshot uses only months before the target month", {
  d <- data.table(limit_bal = 1000, age = 30, sex = factor(1), education = factor(2), marriage = factor(1),
                  pay_6 = -1, pay_5 = 0, pay_4 = 2, pay_3 = 3, pay_2 = 8, pay_0 = 1,
                  bill_amt6 = 100, bill_amt5 = 200, bill_amt4 = 400, bill_amt3 = 800, bill_amt2 = 900, bill_amt1 = 950,
                  pay_amt6 = 0, pay_amt5 = 50, pay_amt4 = 100, pay_amt3 = 0, pay_amt2 = 0, pay_amt1 = 0,
                  default_oct = 1L, account_id = 1L)
  s <- snapshot(d, 4)                       # July target: features from Apr-Jun
  expect_equal(c(s$behind_1, s$behind_2, s$behind_3), c(2, 0, 0))
  expect_equal(s$paid_full_1, 0L)
  expect_equal(s$util_1, 0.4)
  expect_equal(s$pay_ratio_1, 100 / 200)
  expect_equal(s$y, 1L)                     # July status 3 -> 2+ months behind
  s7 <- snapshot(d, 7)
  expect_equal(s7$behind_2, 3)              # August status 8 is capped at 3
  expect_equal(s7$y, 1L)                    # October uses the issuer's default flag
})

test_that("capture and calibration helpers", {
  p <- c(0.9, 0.8, 0.2, 0.1); y <- c(1, 0, 1, 0)
  expect_equal(capture_at(p, y, 0.25), 0.5)
  expect_equal(ks_stat(c(0.9, 0.1), c(1, 0)), 1)
  cal <- calibration_table(seq(0.01, 1, by = 0.01), rep(0:1, 50))
  expect_equal(nrow(cal), 10)
  expect_equal(sum(cal$accounts), 100)
  expect_gt(cal[decile == 10, predicted_default], cal[decile == 1, predicted_default])  # decile 10 = least likely to pay
})

test_that("score bands are ordered A (most likely to pay) to E", {
  band <- make_bands(seq(0, 1, length.out = 1001))
  expect_equal(as.character(band(c(0.99, 0.6, 0.3, 0.15, 0.01))), c("A", "B", "C", "D", "E"))
})
