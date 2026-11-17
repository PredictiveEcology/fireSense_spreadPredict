## Fuel x climate interactions: a fitted coefficient named "MDC:fuelA" multiplies the product of the two
## centred covariates (0 where youngAge cleared the fuel), formed by the
## function the fit uses (fireSenseUtils::spreadProbFromIntegerCovs()). Toy covariates as in setup-toySpread.R:
## MDC / 200, youngAge, fuelA / 8 (fuelA / 8 uses the toy range 0..8, as in test-spreadProb-values.R).

icptI <- fireSenseUtils::spreadInterceptTxt
paramsX <- function(intercept = NULL) {
  p <- data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1,
                  MDC = 1.5, youngAge = -0.5, fuelA = 1, `MDC:fuelA` = -2, check.names = FALSE)
  if (!is.null(intercept)) p <- cbind(p[1:3], stats::setNames(data.frame(intercept), icptI), p[4:7])
  p
}
insX <- function(...) toyInputs(params = paramsX(...), formula = "~ MDC + youngAge + fuelA + MDC:fuelA - 1")

test_that("the interaction's coefficient multiplies the product of the rescaled covariates", {
  cv <- toyCovariates()
  o <- order(cv$pixelID)
  mdc <- cv$MDC[o] / 200; ya <- cv$youngAge[o]; fuel <- cv$fuelA[o] / 8
  x <- 1.5 * mdc - 0.5 * ya + 1 * fuel - 2 * mdc * fuel
  expect_equal(predVals(toyRun(insX()))[c(1, 2, 3, 4, 6, 7, 8)], handLogistic3(x), tolerance = 1e-7)
  ## and it matters: without the term the prediction is another one
  ref <- predVals(toyRun(toyInputs()))
  expect_gt(max(abs(predVals(toyRun(insX())) - ref), na.rm = TRUE), 0.01)
})

test_that("with an intercept and centres, the interaction is the product of the centred covariates", {
  centre <- list(MDC = 0.4, youngAge = 0.2, fuelA = 0.3)
  ins <- insX(intercept = 0.4)
  ins$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitCovCentreTxt]] <- list(centre)
  ins$fireSense_spreadFormula <- "~ 1 + MDC + youngAge + fuelA + MDC:fuelA"
  cv <- toyCovariates()
  o <- order(cv$pixelID)
  mdc <- cv$MDC[o] / 200; ya <- cv$youngAge[o]; fuel <- cv$fuelA[o] / 8
  x <- 0.4 + 1.5 * (mdc - 0.4) - 0.5 * (ya - 0.2) + (fuel - 0.3) - 2 * (mdc - 0.4) * (fuel - 0.3)
  expect_equal(predVals(toyRun(ins))[c(1, 2, 3, 4, 6, 7, 8)], handLogistic3(x), tolerance = 1e-7)
})

test_that("a fitted interaction whose fuel covariate is absent is formed from no biomass; another missing covariate stops, naming it", {
  covs <- toyCovariates()
  withFuel <- data.table::copy(covs)
  data.table::set(withFuel, j = "fuelA", value = fireSenseUtils::logMinB(rep(0, NROW(covs))))
  data.table::set(covs, j = "fuelA", value = NULL)
  ins <- insX()
  ins$fireSense_SpreadCovariates <- covs
  ref <- insX()
  ref$fireSense_SpreadCovariates <- withFuel
  expect_equal(suppressMessages(predVals(toyRun(ins))), predVals(toyRun(ref)))
  data.table::set(covs, j = "youngAge", value = NULL)
  ins$fireSense_SpreadCovariates <- covs
  expect_error(toyRun(ins), "youngAge")
})

test_that("the prediction is the fit's spreadProb for the same covariates, with fuel cleared where youngAge is 1", {
  ## fuel arrives logged, with the fit's linear range; the fit takes it linear, per fire year, from the annual
  ## (climate, youngAge) and non-annual (fuel) tables, as the objective does through spreadProbFromIntegerCovs()
  covs <- toyCovariates()
  biomass <- c(0, 0, 4000, 0, 8000, 8000, 2000)
  data.table::set(covs, j = "fuelA", value = fireSenseUtils::logMinB(biomass))
  ins <- insX(intercept = -0.3)
  ins$fireSense_SpreadCovariates <- covs
  rng <- fireSenseUtils::fuelLinearRange
  ins$covMinMax_spread <- data.table::data.table(MDC = c(0, 200), youngAge = c(0, 1), fuelA = rng)
  centre <- list(MDC = 0.4, youngAge = 0.2, fuelA = 0.3)
  ins$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitCovCentreTxt]] <- list(centre)
  v <- predVals(toyRun(ins))

  lin <- data.table::copy(covs)
  data.table::set(lin, j = "fuelA", value = fireSenseUtils::fuelLogToLinear(lin$fuelA))
  x1000 <- fireSenseUtils::toX1000(list(lin))[[1]] |> data.table::setDT()
  ann <- x1000[, c("pixelID", "MDC", "youngAge"), with = FALSE]
  nonAnn <- list(`2000` = x1000[, c("pixelID", "fuelA"), with = FALSE])
  cols <- fireSenseUtils::spreadDesignCols("~ 1 + MDC + youngAge + fuelA + MDC:fuelA")
  used <- fireSenseUtils::spreadProbFromIntegerCovs(
    NULL, ann, nonAnn, fireSenseUtils::nonAnnualIndex(nonAnn), 2001, ins$covMinMax_spread,
    list(youngAge = "fuelA"), cols, FALSE, NULL, NULL, NULL, NULL, covCentre = centre)
  p <- paramsX(intercept = -0.3)
  covPars <- unlist(p[cols])
  fit <- fireSenseUtils::spreadProbFromCovs(as.matrix(used[, cols, with = FALSE]), unlist(p[1:3]), covPars, 0.13)
  expect_equal(unname(v[used$pixelID]), as.numeric(fit), tolerance = 1e-9)
  ## the fuel of the young pixels (cells 2 and 8) is cleared: centred it is -0.3, but its interaction is 0
  expect_equal(used$fuelA[used$pixelID == 8], -0.3)
  expect_equal(used[["MDC:fuelA"]][used$pixelID == 8], 0)
  expect_equal(used[["MDC:fuelA"]][used$pixelID == 3], (used$MDC[used$pixelID == 3]) * (0.4 - 0.3))
})
