## A hindcast built its spread formula with a term (treedWetland_agb) the ledger fit does not have; the
## covariates match the fit. Prediction must use the fitted terms, not the formula's.

test_that("a formula term the fit does not have is ignored: the fitted terms predict", {
  ref <- predVals(toyRun(toyInputs()))
  ins <- toyInputs(formula = "~ MDC + youngAge + fuelA + fuelZ - 1")
  expect_identical(predVals(toyRun(ins)), ref)
})

test_that("a formula missing a fitted term still predicts with every fitted term", {
  ref <- predVals(toyRun(toyInputs()))
  ins <- toyInputs(formula = "~ MDC - 1")
  expect_identical(predVals(toyRun(ins)), ref)
})

test_that("covariate columns beyond the fitted terms are not used", {
  covs <- toyCovariates()
  data.table::set(covs, j = "treedWetland_agb", value = 1)
  expect_identical(predVals(toyRun(toyInputs(covs = covs))), predVals(toyRun(toyInputs())))
})
