## This is a setup file, not a helper: helpers are sourced by pkgload::load_all() into an
## environment that cannot see `moduleName` and `testPaths` from setup.R.
##
## Tests run inside the namespace of the package rendition, which does not import
## data.table, so `dt[i, j]` there is NOT data.table-aware. Tables are therefore edited
## with data.table::set() and base subsetting only.
##
## Toy inputs for the `run` event, small enough that every predicted value can be worked
## out by hand.
##
## flammableRTM is 3 x 3. Cell 5 is non-flammable (0) and cell 9 is NA; neither has a row in
## the covariate table, as in fireSense_dataPrepPredict. Rows are deliberately NOT in
## pixelID order.
##
## The ranges in `covMinMax_spread` are those of the (imaginary) FITTING data and are wider
## than the ranges of the prediction covariates: MDC is 50..150 here but 0..200 in the fit.
toyCovariates <- function() {
  data.table::data.table(
    pixelID  = c(7L,  1L, 3L,  2L,  8L,  4L,  6L),
    MDC      = c(150, 0,  100, 50,  100, 200, 100),
    youngAge = c(0,   0,  0,   1,   1,   0,   0),
    fuelA    = c(0,   0,  4,   0,   8,   8,   2)
  )
}

toyParams <- function(...) {
  ## one row per retained DEoptim solution: logistic parameters first, then one
  ## coefficient per covariate
  data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1,
             MDC = 1.5, youngAge = -0.5, fuelA = 1, ...)
}

toyInputs <- function(covs = toyCovariates(), params = toyParams(),
                      formula = "~ MDC + youngAge + fuelA - 1") {
  flammableRTM <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 3,
                              vals = c(1, 1, 1, 1, 0, 1, 1, 1, NA))
  sa <- data.frame(ID = 1L)
  sa$params <- list(params)
  list(flammableRTM = flammableRTM,
       fireSense_SpreadCovariates = covs,
       covMinMax_spread = data.table::data.table(MDC = c(0, 200), youngAge = c(0, 1),
                                                 fuelA = c(0, 8)),
       fireSense_spreadFormula = formula,
       studyAreaWithSpreadParams = sa)
}

toySim <- function(objects = toyInputs(), params = list(), times = list(start = 1, end = 1)) {
  SpaDES.core::simInit(
    times = c(times, timeunit = "year"),
    modules = moduleName,
    params = stats::setNames(list(params), moduleName),
    objects = objects,
    paths = testPaths
  )
}

toyRun <- function(...) SpaDES.core::spades(toySim(...), debug = FALSE)

## predicted values, as a plain vector indexed by cell number
predVals <- function(sim) terra::values(sim$fireSense_SpreadPredicted, mat = FALSE)

## The 3-parameter logistic written out longhand, for the hand calculations below:
## lower + (upper - lower) / (1 + exp(-slope * x)) ^ shape
handLogistic3 <- function(x, upper = 0.25, slope = 2, shape = 1, lower = 0.13) {
  lower + (upper - lower) / (1 + exp(-slope * x))^shape
}

## set one covariate value for one pixel
setCov <- function(covs, pixel, col, value) {
  data.table::set(covs, which(covs$pixelID == pixel), col, value)
  covs
}

## rows of completed()/events() for this module and one event type, as a data.frame
evOf <- function(dt, type) {
  df <- as.data.frame(dt)
  df[df$moduleName == "fireSense_spreadPredict" & df$eventType == type, , drop = FALSE]
}

## Two ELFs, "A" and "B", for the multi-ELF tests (see test-multiELF.R for the layout).
multiInputs <- function() {
  crs <- "EPSG:3005"
  flam <- terra::rast(nrows = 1, ncols = 10, xmin = 0, xmax = 50000, ymin = 0, ymax = 5000, crs = crs, vals = 1)
  elf <- terra::rast(flam); terra::values(elf) <- rep(1:2, each = 5)
  levels(elf) <- data.frame(id = 1:2, ELFind = c("A", "B"))
  covs <- data.table::data.table(pixelID = 1:10, MDC = 100, fuelA = 2, fuelB = 3)
  pars <- function(max, fuel) {
    d <- data.frame(maxAsymptote = max, hillSlope1 = 2, inflectionPoint1 = 1, MDC = 0, x = 0)
    names(d)[5] <- fuel
    d
  }
  cmm <- function(fuel) { d <- data.table::data.table(MDC = c(0, 200), f = c(0, 8)); data.table::setnames(d, "f", fuel); d }
  sa <- data.frame(polygonID = c("A", "B"))
  sa$params <- list(pars(0.25, "fuelA"), pars(0.27, "fuelB"))
  sa$covMinMax_spread <- list(cmm("fuelA"), cmm("fuelB"))
  list(flammableRTM = flam, rasterToMatchLargeELF = elf, fireSense_SpreadCovariates = covs,
       studyAreaWithSpreadParams = sa, fireSense_spreadFormula = "~ MDC + fuelA - 1",
       covMinMax_spread = cmm("fuelA"))
}
