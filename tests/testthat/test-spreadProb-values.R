## Exact predicted values from a toy fitted model and toy covariates.
##
## The run event (1) turns covariates into integers x 1000, (2) rescales each to [0, 1] with
## the FIT's min/max in `covMinMax_spread`, (3) takes the linear combination with the fitted
## coefficients, (4) puts it through the logistic, (5) uses one parameter row, chosen by `.rep`, and
## (6) writes the values to the cells given by `pixelID`.

test_that("spread probability is the logistic of covariates rescaled with the fit's min/max", {
  sim <- toyRun()
  v <- predVals(sim)

  ## Rescaled covariates: MDC / 200, youngAge / 1, fuelA / 8.
  ## x = 1.5 * MDC/200 - 0.5 * youngAge + 1 * fuelA/8
  ## cell 1: MDC 0,   YA 0, fuelA 0 -> x = 0                   -> 0.13 + 0.12 / 2 = 0.19
  ## cell 2: MDC 50,  YA 1, fuelA 0 -> x = 0.375 - 0.5 = -0.125
  ## cell 3: MDC 100, YA 0, fuelA 4 -> x = 0.75 + 0.5  = 1.25
  ## cell 4: MDC 200, YA 0, fuelA 8 -> x = 1.5 + 1     = 2.5
  ## cell 6: MDC 100, YA 0, fuelA 2 -> x = 0.75 + 0.25 = 1
  ## cell 7: MDC 150, YA 0, fuelA 0 -> x = 1.125
  ## cell 8: MDC 100, YA 1, fuelA 8 -> x = 0.75 - 0.5 + 1 = 1.25
  expect_equal(v[1], 0.19, tolerance = 1e-7)
  ## 0.13 + 0.12 / (1 + exp(0.25)) = 0.13 + 0.12 * 0.4378235 = 0.1825388
  expect_equal(v[2], 0.1825388, tolerance = 1e-6)
  ## 0.13 + 0.12 / (1 + exp(-2.5)) = 0.13 + 0.12 * 0.9241418 = 0.2408970
  expect_equal(v[3], 0.2408970, tolerance = 1e-6)
  ## 0.13 + 0.12 / (1 + exp(-5)) = 0.13 + 0.12 * 0.9933071 = 0.2491969
  expect_equal(v[4], 0.2491969, tolerance = 1e-6)
  expect_equal(v[c(6, 7, 8)], handLogistic3(c(1, 1.125, 1.25)), tolerance = 1e-7)
  ## cells 3 and 8 have different covariates but the same linear predictor
  expect_equal(v[3], v[8], tolerance = 1e-12)
})

test_that("covariates are NOT rescaled to the range of the prediction data", {
  ## Drop the MDC = 0 and MDC = 200 rows: MDC now spans 50..150 in the prediction data.
  ## Rescaled to its own range, MDC = 50 would become 0 and cell 2 would get
  ## x = -0.5 -> 0.1622729. With the fit's 0..200 it is x = -0.125 -> 0.1825388.
  covs <- toyCovariates()
  covs <- covs[which(covs$MDC > 0 & covs$MDC < 200), ]
  sim <- toyRun(toyInputs(covs = covs))
  v <- predVals(sim)
  expect_equal(v[2], 0.1825388, tolerance = 1e-6)
  expect_gt(abs(v[2] - 0.1622729), 0.02)
  ## MDC = 150 stays at 0.75 (own range would make it 1): x = 1.125
  expect_equal(v[7], handLogistic3(1.125), tolerance = 1e-7)
})

test_that("a covMinMax that does not start at zero shifts as well as scales", {
  ins <- toyInputs()
  ins$covMinMax_spread$MDC <- c(100, 300)
  ## cell 4: MDC 200 -> (200 - 100) / 200 = 0.5 -> x = 0.75 + 1 = 1.75
  ## cell 7: MDC 150 -> 0.25 -> x = 0.375
  v <- predVals(toyRun(ins))
  expect_equal(v[c(4, 7)], handLogistic3(c(1.75, 0.375)), tolerance = 1e-7)
})

test_that("values land in the cells named by pixelID; all other cells are NA", {
  sim <- toyRun()
  v <- predVals(sim)
  expect_identical(which(!is.na(v)), c(1L, 2L, 3L, 4L, 6L, 7L, 8L))
  expect_true(all(is.na(v[c(5, 9)])))
  ## same grid as flammableRTM
  expect_true(terra::compareGeom(sim$fireSense_SpreadPredicted, sim$flammableRTM))
  expect_identical(terra::nlyr(sim$fireSense_SpreadPredicted), 1)
})

test_that("row order of the covariate table does not matter", {
  covs <- toyCovariates()
  a <- predVals(toyRun(toyInputs(covs = covs[order(covs$pixelID), ])))
  b <- predVals(toyRun(toyInputs(covs = covs[order(-covs$pixelID), ])))
  expect_equal(a, b, tolerance = 1e-12)
  expect_equal(a[1], 0.19, tolerance = 1e-7)
})

test_that("the run event leaves sim$fireSense_SpreadCovariates untouched", {
  sim <- toyRun()
  ## the x 1000 integer conversion is done by reference, so it must be done on a copy
  expect_equal(sim$fireSense_SpreadCovariates, toyCovariates())
  expect_identical(sim$fireSense_SpreadCovariates$MDC, c(150, 0, 100, 50, 100, 200, 100))
})

test_that("covariates are rounded to 3 decimals by the x 1000 integer step", {
  covs <- toyCovariates()
  setCov(covs, 1L, "youngAge", 0.1236)   # -> 124L -> 0.124
  setCov(covs, 7L, "youngAge", 0.1234)   # -> 123L -> 0.123
  v <- predVals(toyRun(toyInputs(covs = covs)))
  ## cell 1: x = -0.5 * 0.124 = -0.062 ; cell 7: x = 1.125 - 0.5 * 0.123 = 1.0635
  expect_equal(v[1], handLogistic3(-0.062), tolerance = 1e-9)
  expect_equal(v[7], handLogistic3(1.0635), tolerance = 1e-9)
  ## and not the unrounded value, which differs in the 5th decimal
  expect_gt(abs(v[1] - handLogistic3(-0.5 * 0.1236)), 1e-6)
})

test_that("a single parameter row and a single pixel both work", {
  covs <- toyCovariates()
  covs <- covs[which(covs$pixelID == 3L), ]
  v <- predVals(toyRun(toyInputs(covs = covs)))
  expect_identical(which(!is.na(v)), 3L)
  expect_equal(v[3], 0.2408970, tolerance = 1e-6)
})

test_that("coefficients are matched to covariates by name, not by position", {
  p <- toyParams()[, c("maxAsymptote", "hillSlope1", "inflectionPoint1", "fuelA", "youngAge", "MDC")]
  v <- predVals(toyRun(toyInputs(params = p)))
  expect_equal(v[c(2, 3, 4)], c(0.1825388, 0.2408970, 0.2491969), tolerance = 1e-6)
})

test_that("two logistic parameters use the 2-parameter logistic (shape fixed at 0.5)", {
  p <- toyParams()
  p$inflectionPoint1 <- NULL
  v <- predVals(toyRun(toyInputs(params = p)))
  ## cell 1, x = 0: 0.13 + 0.12 / sqrt(2) = 0.2148528
  expect_equal(v[1], 0.2148528, tolerance = 1e-6)
  ## cell 4, x = 2.5: 0.13 + 0.12 / sqrt(1 + exp(-5)) = 0.2495964
  expect_equal(v[4], 0.13 + 0.12 / sqrt(1 + exp(-5)), tolerance = 1e-7)
})

test_that("the shape parameter of the 3-parameter logistic is used", {
  p <- toyParams()
  p$inflectionPoint1 <- 2
  v <- predVals(toyRun(toyInputs(params = p)))
  ## cell 1, x = 0: 0.13 + 0.12 / 2^2 = 0.16
  expect_equal(v[1], 0.16, tolerance = 1e-7)
})

test_that("lowerSpreadProb is the lower asymptote", {
  v <- predVals(toyRun(params = list(lowerSpreadProb = 0.05)))
  ## cell 1, x = 0: 0.05 + (0.25 - 0.05) / 2 = 0.15
  expect_equal(v[1], 0.15, tolerance = 1e-7)
  ## strongly negative x tends to the lower asymptote: make MDC's coefficient -40
  p <- toyParams(); p$MDC <- -40
  v <- predVals(toyRun(toyInputs(params = p), params = list(lowerSpreadProb = 0.05)))
  ## cell 4: x = -40 + 1 = -39 -> 0.05 + 0.2 / (1 + exp(78)) = 0.05 to 1e-12
  expect_equal(v[4], 0.05, tolerance = 1e-9)
})

test_that("maxFireSpread does not change the prediction", {
  a <- predVals(toyRun(params = list(maxFireSpread = 0.28)))
  b <- predVals(toyRun(params = list(maxFireSpread = 0.20)))
  expect_identical(a, b)
  expect_equal(a[4], 0.2491969, tolerance = 1e-6) # above 0.20: not capped
})

test_that("a covariate with no fitted coefficient is dropped with a warning", {
  covs <- toyCovariates()
  data.table::set(covs, NULL, "fuelB", 5)
  ins <- toyInputs(covs = covs)
  ins$covMinMax_spread$fuelB <- c(0, 10)
  expect_warning(sim <- toyRun(ins), "fuelB\\s+\\.\\.\\.that are not in the sim\\$studyAreaWithSpreadParams")
  ## fuelB contributes nothing
  expect_equal(predVals(sim)[c(1, 3)], c(0.19, 0.2408970), tolerance = 1e-6)
})

## ---- fuel biomass: linear fits and earlier log fits ------------------------------------------
## Fuel reaches this module logged (fireSenseUtils::logMinB()). fireSense_spreadFit now fits it on the
## linear scale divided by 1e4, and records that as covMinMax_spread = c(0, 1e4) for the fuel column.

test_that("a fit on linear fuel biomass is predicted with biomass / 1e4", {
  covs <- toyCovariates()
  ## rows are pixelID 7, 1, 3, 2, 8, 4, 6. Biomass by cell: 1: 0, 2: 0, 3: 5000, 4: 20000, 6: 2500,
  ## 7: 0, 8: 20000 -- supplied logged, as dataPrepPredict does
  covs$fuelA <- fireSenseUtils::logMinB(c(0, 0, 5000, 0, 20000, 20000, 2500))
  ins <- toyInputs(covs = covs)
  ins$covMinMax_spread$fuelA <- c(0, 1e4)
  v <- predVals(toyRun(ins))
  ## x = 1.5 * MDC/200 - 0.5 * youngAge + 1 * biomass/1e4
  ## cell 1: 0 + 0 + 0 = 0                (absent fuel is exactly 0, not the log floor)
  ## cell 3: 0.75 + 0.5 = 1.25            cell 4: 1.5 + 2 = 3.5   (biomass/1e4 = 2: above 1, not clamped)
  ## cell 6: 0.75 + 0.25 = 1              cell 7: 1.125 + 0
  ## cell 8: youngAge = 1 zeroes fuelA (mutually exclusive, as in the fit): 0.75 - 0.5 + 0 = 0.25
  expect_equal(v[c(1, 3, 4, 6, 7, 8)], handLogistic3(c(0, 1.25, 3.5, 1, 1.125, 0.25)), tolerance = 1e-7)
})

test_that("a fit made on the log scale is predicted on the log scale, as before", {
  covs <- toyCovariates()
  lb <- fireSenseUtils::logMinB(c(0, 0, 5000, 0, 20000, 20000, 2500))   # by row: pixelID 7, 1, 3, 2, 8, 4, 6
  covs$fuelA <- lb
  ins <- toyInputs(covs = covs)
  ins$covMinMax_spread$fuelA <- c(3.605, 10)        # the log range an earlier fit stored
  v <- predVals(toyRun(ins))
  r <- (lb - 3.605) / (10 - 3.605)                  # rescaled LOG biomass, untouched by fuelLogToLinear()
  mdc <- covs$MDC / 200; ya <- covs$youngAge
  ## 1e-5: covariates travel as integers x 1000, so the log floor 3.60517 is 3.605
  expect_equal(v[covs$pixelID], handLogistic3(1.5 * mdc - 0.5 * ya + r), tolerance = 1e-5)
  ## and it is NOT what the linear transform would give. Cell 3 has 5000: 0.5 on the linear scale
  ## (x = 1.25 -> 0.2409) but (8.517 - 3.605) / 6.395 = 0.768 on the log scale (x = 1.518 -> 0.2445)
  expect_gt(abs(v[3] - handLogistic3(1.25)), 0.003)
})

test_that("a fit with upperTail1 is predicted with the upper-tail link, from the parameter's name", {
  ## fireSense_spreadFit's link "logistic3pUpper" stores one more logistic parameter, upperTail1
  ## (fireSenseUtils::logistic3pUpper(), Stukel 1988). It changes the curve only where
  ## hillSlope1 * x > 0: there u = hillSlope1 * x becomes -log(1 - a * u) / a for a < 0.
  v <- predVals(toyRun(toyInputs(params = toyParams(upperTail1 = -0.5))))
  handUpper <- function(x, a = -0.5) {
    u <- 2 * x
    u[u > 0] <- -log(1 - a * u[u > 0]) / a
    0.13 + 0.12 / (1 + exp(-u))
  }
  ## cell 4: x = 2.5, u = 5 -> 2 * log(3.5) = 2.505526 -> 0.13 + 0.12 * 0.9245283 = 0.2409434
  expect_equal(v[4], 0.2409434, tolerance = 1e-6)
  expect_equal(v[c(1, 3, 4, 6, 7, 8)], handUpper(c(0, 1.25, 2.5, 1, 1.125, 1.25)), tolerance = 1e-7)
  ## below the inflection nothing changes: cell 2 (x = -0.125) is the logistic3p value
  expect_equal(v[2], 0.1825388, tolerance = 1e-6)
  ## and upperTail1 = 0 is logistic3p everywhere
  v0 <- predVals(toyRun(toyInputs(params = toyParams(upperTail1 = 0))))
  expect_equal(v0, predVals(toyRun()), tolerance = 1e-12)
})

test_that("one ELF: yearSpreadSD is not a coefficient; fireSense_SpreadSD is the chosen set's value", {
  base <- toyRun()
  p2 <- rbind(toyParams(), toyParams())
  p2$yearSpreadSD <- c(0.3, 0.5)
  sim <- toyRun(toyInputs(params = p2))
  expect_equal(predVals(sim), predVals(base), tolerance = 1e-12)
  expect_equal(sim$fireSense_SpreadSD, 0.3)
  expect_identical(toyRun()$fireSense_SpreadSD, 0)
})
