## A fit made with fireSense_dataPrepFit's spreadClimateCurve has `MDC_curve = softplus(MDC - k, s)`, k and s the ELF's median and sd stored in the
## ledger row (`climateCurvePars_spread`). Prediction builds the term from the pixel's own MDC and those
## (fireSenseUtils::addClimateCurve(), the function the fit used), rescaled with c(0, 100) (fireSenseUtils::climateCurveRanges()).

curveParams <- function(curve = 1.2, fuel = 1) {
  data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1, MDC = 1.5, MDC_curve = curve, fuelA = fuel)
}
curveInputs <- function(knots = list(MDC = c(k = 100, s = 40)), params = curveParams(), covs = toyCovariates()) {
  ins <- toyInputs(covs = covs, params = params, formula = "~ MDC + MDC_curve + fuelA - 1")
  ins$covMinMax_spread <- data.table::data.table(MDC = c(0, 200), MDC_curve = c(0, 100), fuelA = c(0, 8))
  if (!is.null(knots)) ins$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitClimateCurveParsTxt]] <- I(list(knots))
  ins
}

test_that("the curve is built from the pixel's MDC and the ledger row's k and s, as the fit built it", {
  covs <- toyCovariates()
  v <- predVals(toyRun(curveInputs(covs = covs)))
  o <- order(covs$pixelID)
  mdc <- covs$MDC[o]; fuel <- covs$fuelA[o]
  ## the fit's own function, on the same raw values
  fit <- data.table::data.table(MDC = mdc)
  fireSenseUtils::addClimateCurve(fit, list(MDC = c(k = 100, s = 40)))
  expect_equal(fit$MDC_curve, 40 * log1p(exp((mdc - 100) / 40)))
  x <- 1.5 * (mdc / 200) + 1.2 * (fit$MDC_curve / 100) + 1 * (fuel / 8)
  cells <- c(1, 2, 3, 4, 6, 7, 8)
  expect_equal(v[cells], handLogistic3(x), tolerance = 1e-4)
})

test_that("a positive curve coefficient raises the prediction everywhere (smooth: no pixel is exactly unchanged), a negative one lowers it", {
  with0 <- predVals(toyRun(curveInputs(params = curveParams(curve = 0))))
  pos <- predVals(toyRun(curveInputs(params = curveParams(curve = 1.2))))
  neg <- predVals(toyRun(curveInputs(params = curveParams(curve = -1.2))))
  cells <- c(1, 2, 3, 4, 6, 7, 8)
  expect_true(all(pos[cells] > with0[cells]))
  expect_true(all(neg[cells] < with0[cells]))
})

test_that("a curve coefficient with no k and s in the ledger row stops, naming it", {
  expect_error(toyRun(curveInputs(knots = NULL)), "MDC_curve.*need `k` and `s`")
  expect_error(toyRun(curveInputs(knots = list(CMD = c(k = 5, s = 1)))), "MDC_curve.*need `k` and `s`")
})

test_that("a fit without curve terms predicts as before, even with k and s in its row", {
  base <- predVals(toyRun())
  ins <- toyInputs()
  ins$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitClimateCurveParsTxt]] <- I(list(list(MDC = c(k = 100, s = 40))))
  expect_identical(predVals(toyRun(ins)), base)
  expect_false(any(fireSenseUtils::isClimateTermCol(names(toyParams()), fireSenseUtils::climateCurveSuffix)))
})

test_that("with a split fit the curve is of the pixel's own value, beside the split terms", {
  covs <- toyCovariates()
  p <- data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1, MDC_yr = 1.5, MDC_dev = 0.8, MDC_curve = 1.2, fuelA = 1)
  ins <- toyInputs(covs = covs, params = p, formula = "~ MDC_yr + MDC_dev + MDC_curve + fuelA - 1")
  ins$covMinMax_spread <- data.table::data.table(MDC_yr = c(0, 200), MDC_dev = c(-100, 100), MDC_curve = c(0, 100), fuelA = c(0, 8))
  ins$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitClimateCurveParsTxt]] <- I(list(list(MDC = c(k = 100, s = 40))))
  v <- predVals(toyRun(ins))
  o <- order(covs$pixelID)
  mdc <- covs$MDC[o]; fuel <- covs$fuelA[o]
  med <- stats::median(covs$MDC)
  x <- 1.5 * (med / 200) + 0.8 * ((mdc - med + 100) / 200) + 1.2 * (40 * log1p(exp((mdc - 100) / 40)) / 100) + 1 * (fuel / 8)
  expect_equal(v[c(1, 2, 3, 4, 6, 7, 8)], handLogistic3(x), tolerance = 1e-4)
})
