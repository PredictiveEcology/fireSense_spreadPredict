## A fit made with fireSense_dataPrepFit's spreadClimateHinge has `MDC_hinge = pmax(0, MDC - k)`, k the ELF's knot stored in the
## ledger row (`climateHingeKnots_spread`). Prediction builds the term from the pixel's own MDC and that knot
## (fireSenseUtils::addClimateHinge(), the function the fit used), rescaled with c(0, 100) (fireSenseUtils::climateHingeRanges()).

hingeParams <- function(hinge = 1.2, fuel = 1) {
  data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1, MDC = 1.5, MDC_hinge = hinge, fuelA = fuel)
}
hingeInputs <- function(knots = list(MDC = 100), params = hingeParams(), covs = toyCovariates()) {
  ins <- toyInputs(covs = covs, params = params, formula = "~ MDC + MDC_hinge + fuelA - 1")
  ins$covMinMax_spread <- data.table::data.table(MDC = c(0, 200), MDC_hinge = c(0, 100), fuelA = c(0, 8))
  if (!is.null(knots)) ins$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitClimateHingeKnotsTxt]] <- I(list(knots))
  ins
}

test_that("the hinge is built from the pixel's MDC and the ledger row's knot, as the fit built it", {
  covs <- toyCovariates()
  v <- predVals(toyRun(hingeInputs(covs = covs)))
  o <- order(covs$pixelID)
  mdc <- covs$MDC[o]; fuel <- covs$fuelA[o]
  ## the fit's own function, on the same raw values
  fit <- data.table::data.table(MDC = mdc)
  fireSenseUtils::addClimateHinge(fit, list(MDC = 100))
  expect_identical(fit$MDC_hinge, pmax(0, mdc - 100))
  x <- 1.5 * (mdc / 200) + 1.2 * (fit$MDC_hinge / 100) + 1 * (fuel / 8)
  cells <- c(1, 2, 3, 4, 6, 7, 8)
  expect_equal(v[cells], handLogistic3(x), tolerance = 1e-7)
})

test_that("the hinge term changes the prediction only above the knot", {
  covs <- toyCovariates()
  with0 <- predVals(toyRun(hingeInputs(params = hingeParams(hinge = 0))))
  with1 <- predVals(toyRun(hingeInputs(params = hingeParams(hinge = 1.2))))
  above <- covs$pixelID[covs$MDC > 100]; atOrBelow <- covs$pixelID[covs$MDC <= 100]
  expect_true(all(with1[above] > with0[above]))
  expect_equal(with1[atOrBelow], with0[atOrBelow], tolerance = 1e-12)
})

test_that("a hinge coefficient with no knot in the ledger row stops, naming it", {
  expect_error(toyRun(hingeInputs(knots = NULL)), "MDC_hinge.*need a knot")
  expect_error(toyRun(hingeInputs(knots = list(CMD = 5))), "MDC_hinge.*need a knot")
})

test_that("a fit without hinge terms predicts as before, even with knots in its row", {
  base <- predVals(toyRun())
  ins <- toyInputs()
  ins$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitClimateHingeKnotsTxt]] <- I(list(list(MDC = 100)))
  expect_identical(predVals(toyRun(ins)), base)
  expect_false(any(fireSenseUtils::isClimateHingeCol(names(toyParams()))))
})

test_that("with a split fit the hinge is of the pixel's own value, beside the split terms", {
  covs <- toyCovariates()
  p <- data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1, MDC_yr = 1.5, MDC_dev = 0.8, MDC_hinge = 1.2, fuelA = 1)
  ins <- toyInputs(covs = covs, params = p, formula = "~ MDC_yr + MDC_dev + MDC_hinge + fuelA - 1")
  ins$covMinMax_spread <- data.table::data.table(MDC_yr = c(0, 200), MDC_dev = c(-100, 100), MDC_hinge = c(0, 100), fuelA = c(0, 8))
  ins$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitClimateHingeKnotsTxt]] <- I(list(list(MDC = 100)))
  v <- predVals(toyRun(ins))
  o <- order(covs$pixelID)
  mdc <- covs$MDC[o]; fuel <- covs$fuelA[o]
  med <- stats::median(covs$MDC)
  x <- 1.5 * (med / 200) + 0.8 * ((mdc - med + 100) / 200) + 1.2 * (pmax(0, mdc - 100) / 100) + 1 * (fuel / 8)
  expect_equal(v[c(1, 2, 3, 4, 6, 7, 8)], handLogistic3(x), tolerance = 1e-7)
})
