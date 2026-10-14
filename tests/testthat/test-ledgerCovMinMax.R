## A single fitted ELF rescales its covariates with the ranges its ledger row stores
## (`covMinMax_spread`), not with `sim$covMinMax_spread`: fireSense_spreadFit rebuilds that object in
## every run from the run's own tables, and in a hindcast those come from a formula rebuilt for another
## year, so it need not have the fit's columns. The row's ranges are the fit's.

test_that("one ELF rescales with its ledger row's covMinMax_spread, not sim$covMinMax_spread", {
  ins <- toyInputs()
  ledger <- data.table::copy(ins$covMinMax_spread)
  ledger$MDC <- c(100, 300)
  ins$studyAreaWithSpreadParams$covMinMax_spread <- list(ledger)
  ## as in test-spreadProb-values.R "shifts as well as scales", but the range is in the row only:
  ## cell 4: MDC 200 -> (200 - 100) / 200 = 0.5 -> x = 0.75 + 1 = 1.75; cell 7: 150 -> 0.25 -> 0.375
  v <- predVals(toyRun(ins))
  expect_equal(v[c(4, 7)], handLogistic3(c(1.75, 0.375)), tolerance = 1e-7)
  expect_false(isTRUE(all.equal(v, predVals(toyRun(toyInputs())))))
})

test_that("a row without ranges (an older ledger) falls back to sim$covMinMax_spread", {
  expect_identical(predVals(toyRun(toyInputs())), predVals(toyRun(toyInputs())))
  sa <- data.frame(polygonID = c("A", "B"))
  cmm <- data.table::data.table(MDC = c(0, 200))
  sa$covMinMax_spread <- list(cmm, NULL)
  expect_identical(ledgerCovMinMax(sa, 1L, "default"), cmm)
  expect_identical(ledgerCovMinMax(sa, 2L, "default"), "default")
  expect_identical(ledgerCovMinMax(data.frame(polygonID = "A"), 1L, "default"), "default")
  expect_null(ledgerCovMinMax(data.frame(polygonID = "A"), 1L))
})

test_that("a fitted covariate with no range stops instead of staying unscaled", {
  ins <- toyInputs()
  ins$covMinMax_spread$fuelA <- NULL
  expect_error(toyRun(ins), "no range for the fitted covariate\\(s\\) fuelA")
})
