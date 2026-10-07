## youngAge must be mutually exclusive with every other non-climate covariate at prediction time,
## exactly as in the fit (fireSense_spreadFit::spreadFitPrep()). Before the fix,
## spreadProbOneELF() (fireSense_spreadPredict.R:282 pre-fix) called
## fireSenseUtils::spreadProbFromIntegerCovs() with `mutuallyExclusive = NULL`, so a young pixel's
## fuel biomass and non-forest land-cover columns reached the link unchanged instead of being
## zeroed alongside youngAge = 1.

test_that("a young pixel's covariates entering the link are youngAge = 1 and all others 0", {
  covs <- toyCovariates()
  ## rows are pixelID 7, 1, 3, 2, 8, 4, 6 (see setup-toySpread.R)
  covs$fuelA <- fireSenseUtils::logMinB(c(0, 0, 5000, 0, 20000, 20000, 2500))  # biomass, supplied logged
  covs$nfLCC_40 <- c(0, 0, 0, 1, 1, 0, 0)                                     # land-cover indicator

  formula <- "~ MDC + youngAge + fuelA + nfLCC_40 - 1"
  p <- toyParams(nfLCC_40 = 2)
  ins <- toyInputs(covs = covs, params = p, formula = formula)
  ins$covMinMax_spread$fuelA <- c(0, 1e4)      # a fit on linear fuel biomass, as fireSense_spreadFit makes
  ins$covMinMax_spread$nfLCC_40 <- c(0, 1)

  v <- predVals(toyRun(ins))

  ## cell 8 (pixelID 8): MDC 100, youngAge 1, fuelA biomass 20000, nfLCC_40 = 1.
  ## Without the fix: x = 0.75 - 0.5 + 2 + 2 = 4.25 (fuel and land cover both leak through).
  ## With the fix, youngAge zeroes both: x = 0.75 - 0.5 + 0 + 0 = 0.25
  expect_equal(v[8], handLogistic3(0.25), tolerance = 1e-7)

  ## cell 2 (pixelID 2): MDC 50, youngAge 1, fuelA already 0, nfLCC_40 = 1.
  ## Without the fix: x = 0.375 - 0.5 + 0 + 2 = 1.875. With the fix: x = 0.375 - 0.5 = -0.125
  expect_equal(v[2], handLogistic3(-0.125), tolerance = 1e-7)

  ## cell 3 (pixelID 3) is NOT young: its fuel and land cover are left alone
  ## x = 0.75 + 0 + 0.5 + 0 = 1.25
  expect_equal(v[3], handLogistic3(1.25), tolerance = 1e-7)
})
