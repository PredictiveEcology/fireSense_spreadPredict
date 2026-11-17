## A fit made with fireSense_dataPrepFit's spreadClimateSplit has the climate as two terms, `MDC_yr` (the year's
## median over the ELF's burnable pixels) and `MDC_dev` (MDC minus it). Prediction builds them from the year's
## climate with fireSenseUtils::splitSpreadClimate(), as the fit did, over the ELF's own pixels.
##
## Rescaling: MDC_yr with c(0, 200), MDC_dev with c(-100, 100) (fireSenseUtils::climateSplitRanges()).

splitParams <- function(yr = 1.5, dev = 0.8, fuel = 1, name = "fuelA") {
  d <- data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1, MDC_yr = yr, MDC_dev = dev, x = fuel)
  names(d)[6] <- name
  d
}
cmmSplit <- function(fuel = "fuelA") {
  d <- data.table::data.table(MDC_yr = c(0, 200), MDC_dev = c(-100, 100), f = c(0, 8))
  data.table::setnames(d, "f", fuel)
  d
}

test_that("a split fit predicts from the year's median over the burnable pixels and each pixel's departure from it", {
  covs <- toyCovariates()
  ins <- toyInputs(covs = covs, params = splitParams(), formula = "~ MDC_yr + MDC_dev + fuelA - 1")
  ins$covMinMax_spread <- cmmSplit()
  v <- predVals(toyRun(ins))
  o <- order(covs$pixelID)
  mdc <- covs$MDC[o]; fuel <- covs$fuelA[o]
  med <- stats::median(covs$MDC)                    # 100: over the 7 burnable pixels of the covariate table
  x <- 1.5 * (med / 200) + 0.8 * ((mdc - med + 100) / 200) + 1 * (fuel / 8)
  expect_equal(v[cells <- c(1, 2, 3, 4, 6, 7, 8)], handLogistic3(x), tolerance = 1e-7)
})

test_that("a non-flammable row in the covariate table is left out of the median, as the fit's burnable pixels are", {
  covs <- toyCovariates()
  extra <- data.table::data.table(pixelID = 5L, MDC = 5000, youngAge = 0, fuelA = 0)   # cell 5 of the toy flammableRTM is 0
  withExtra <- data.table::rbindlist(list(covs, extra))
  run <- function(cv) {
    ins <- toyInputs(covs = cv, params = splitParams(), formula = "~ MDC_yr + MDC_dev + fuelA - 1")
    ins$covMinMax_spread <- cmmSplit()
    predVals(toyRun(ins))
  }
  a <- run(covs); b <- run(withExtra)
  cellsB <- c(1, 2, 3, 4, 6, 7, 8)
  expect_equal(b[cellsB], a[cellsB], tolerance = 1e-12)      # median 100 with and without the extra row
})

test_that("the between term is the same for every pixel: only the within term moves with the pixel", {
  covs <- toyCovariates()
  both <- function(yr, dev) {
    ins <- toyInputs(covs = data.table::copy(covs), params = splitParams(yr, dev, fuel = 0), formula = "~ MDC_yr + MDC_dev + fuelA - 1")
    ins$covMinMax_spread <- cmmSplit()
    predVals(toyRun(ins))
  }
  yrOnly <- both(1.5, 0)
  expect_equal(yrOnly[c(1, 2, 3, 4, 6, 7, 8)], rep(handLogistic3(1.5 * 0.5), 7), tolerance = 1e-7)   # median 100 / 200 everywhere
  devOnly <- both(0, 0.8)
  expect_gt(length(unique(round(devOnly[c(1, 2, 3, 4, 6, 7, 8)], 8))), 1L)
})

test_that("with several ELFs each takes the median over its own pixels", {
  ins <- multiInputs()
  covs <- data.table::data.table(pixelID = 1:10, MDC = c(10, 20, 30, 40, 50, 100, 110, 120, 130, 140), fuelA = 2, fuelB = 3)
  ins$fireSense_SpreadCovariates <- covs
  d <- function(fuel) { x <- data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1, MDC_yr = 1.5, MDC_dev = 0.8, f = 1)
                        names(x)[6] <- fuel; x }
  ins$studyAreaWithSpreadParams$params <- list(d("fuelA"), d("fuelB"))
  ins$studyAreaWithSpreadParams$covMinMax_spread <- list(cmmSplit("fuelA"), cmmSplit("fuelB"))
  ins$fireSense_spreadFormula <- "~ MDC_yr + MDC_dev + fuelA - 1"
  v <- predVals(toyRun(ins, params = list(ELFblendWidth = 20000)))
  ## pixel 1 is only A's (25 km from B): A's median of pixels 1:5 is 30
  xA <- 1.5 * (30 / 200) + 0.8 * ((10 - 30 + 100) / 200) + 1 * (2 / 8)
  expect_equal(v[1], handLogistic3(xA), tolerance = 1e-7)
  ## pixel 10 is only B's: B's median of pixels 6:10 is 120
  xB <- 1.5 * (120 / 200) + 0.8 * ((140 - 120 + 100) / 200) + 1 * (3 / 8)
  expect_equal(v[10], handLogistic3(xB, upper = 0.25), tolerance = 1e-7)
})

test_that("a fit without split terms predicts as before: no median is taken", {
  expect_identical(predVals(toyRun()), predVals(toyRun(toyInputs())))
  expect_false(any(fireSenseUtils::isClimateSplitCol(names(toyParams()))))
})
