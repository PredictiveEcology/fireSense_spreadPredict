## One `run` event on a tiny landscape, with inputs shaped like the ones the upstream
## modules make: covariates from fireSense_dataPrepPredict, the spread formula from
## fireSense_dataPrepFit, and `covMinMax_spread` / `studyAreaWithSpreadParams` from
## fireSense_spreadFit.
##
## `fireSense_spreadFormula` and `studyAreaWithSpreadParams` are read by the run event but
## are not declared in this module's `expectsInput()`; they are supplied here regardless,
## because the module cannot run without them.

spreadInputs <- function(nParRows = 2L) {
  flammableRTM <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                              vals = 1)
  set.seed(123)
  pixelID <- seq_len(terra::ncell(flammableRTM))
  covs <- data.table::data.table(pixelID = pixelID,
                                 MDC      = stats::runif(length(pixelID), 0, 200),
                                 youngAge = stats::runif(length(pixelID), 0, 1))
  covMinMax <- data.table::data.table(MDC = c(0, 200), youngAge = c(0, 1))
  ## Three logistic parameters (upper asymptote, slope, shape), named as fitted and one coefficient per
  ## covariate, one row per retained DEoptim solution.
  params <- data.frame(maxAsymptote = rep(0.25, nParRows), hillSlope1 = 2, inflectionPoint1 = 1, MDC = 1.5, youngAge = -0.5)
  saParams <- data.frame(ID = 1L)
  saParams$params <- list(params[seq_len(nParRows), , drop = FALSE])
  list(flammableRTM = flammableRTM,
       fireSense_SpreadCovariates = covs,
       covMinMax_spread = covMinMax,
       fireSense_spreadFormula = "~ MDC + youngAge - 1",
       studyAreaWithSpreadParams = saParams)
}

spreadSim <- function(objects, lowerSpreadProb = 0.13) {
  SpaDES.core::simInit(
    times = list(start = 1, end = 1, timeunit = "year"),
    modules = moduleName,
    params = stats::setNames(list(list(lowerSpreadProb = lowerSpreadProb)), moduleName),
    objects = objects,
    paths = testPaths
  )
}

test_that("simInit() succeeds with inputs shaped like the upstream modules'", {
  sim <- spreadSim(spreadInputs())
  expect_s4_class(sim, "simList")
  expect_true(moduleName %in% unlist(SpaDES.core::modules(sim)))
})

test_that("a run event predicts a spread probability for every flammable pixel", {
  inputs <- spreadInputs()
  sim <- SpaDES.core::spades(spreadSim(inputs), debug = FALSE)

  pred <- sim$fireSense_SpreadPredicted
  expect_s4_class(pred, "SpatRaster")
  expect_identical(dim(pred), dim(inputs$flammableRTM))

  vals <- terra::values(pred, mat = FALSE)[inputs$fireSense_SpreadCovariates$pixelID]
  expect_false(anyNA(vals))
  ## logistic3p() is bounded below by lowerSpreadProb and above by the first logistic
  ## parameter, whatever the covariates are.
  expect_true(all(vals >= 0.13 & vals <= 0.25))
  ## and the covariates move it: not a constant surface
  expect_gt(stats::sd(vals), 0)
})

test_that("a run with no fitted spread parameters stops with an explanation", {
  inputs <- spreadInputs()
  inputs$studyAreaWithSpreadParams <- inputs$studyAreaWithSpreadParams[0, ]
  expect_error(SpaDES.core::spades(spreadSim(inputs), debug = FALSE),
               "holds no fitted spread parameters")
})
