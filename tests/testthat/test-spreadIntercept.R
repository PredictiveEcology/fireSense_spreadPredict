## A ledger row from a fit with an intercept holds a coefficient named fireSenseUtils::spreadInterceptTxt
## and, in the column fireSenseUtils::spreadFitCovCentreTxt, the covariate means the fit subtracted from the
## rescaled covariates. Prediction applies both. A row without them predicts as it always did.
##
## x = intercept + sum(beta * (rescaled covariate - centre)); the toy covariates are those of
## test-spreadProb-values.R: MDC / 200, youngAge, fuelA / 8.

icptName <- fireSenseUtils::spreadInterceptTxt
centreCol <- fireSenseUtils::spreadFitCovCentreTxt
centreI <- list(MDC = 0.45, youngAge = 0.2, fuelA = 0.3)

paramsI <- function(intercept = 0.4) {
  data.frame(maxAsymptote = 0.25, hillSlope1 = 2, inflectionPoint1 = 1, `(Intercept)` = intercept,
             MDC = 1.5, youngAge = -0.5, fuelA = 1, check.names = FALSE)
}
inputsI <- function(intercept = TRUE, centre = TRUE) {
  ins <- toyInputs(params = if (intercept) paramsI() else toyParams(),
                   formula = if (intercept) "~ 1 + MDC + youngAge + fuelA" else "~ MDC + youngAge + fuelA - 1")
  if (centre) ins$studyAreaWithSpreadParams[[centreCol]] <- list(centreI)
  ins
}

## the linear predictor at each toy cell, by hand
xHand <- function(intercept = 0.4, centre = list(MDC = 0, youngAge = 0, fuelA = 0)) {
  cv <- toyCovariates()
  o <- order(cv$pixelID)
  mdc <- cv$MDC[o] / 200; ya <- cv$youngAge[o]; fuel <- cv$fuelA[o] / 8
  ## in pixelID order, which is cell order for the cells that have one
  intercept + 1.5 * (mdc - centre$MDC) - 0.5 * (ya - centre$youngAge) + 1 * (fuel - centre$fuelA)
}
cells <- c(1, 2, 3, 4, 6, 7, 8)

test_that("a row with an intercept and centres predicts the logistic of intercept + centred covariates", {
  v <- predVals(toyRun(inputsI()))
  expect_equal(v[cells], handLogistic3(xHand(0.4, centreI)), tolerance = 1e-7)
  expect_true(all(is.na(v[c(5, 9)])))
})

test_that("an intercept without centres, and centres without an intercept, each apply on their own", {
  v1 <- predVals(toyRun(inputsI(centre = FALSE)))
  expect_equal(v1[cells], handLogistic3(xHand(0.4)), tolerance = 1e-7)
  v2 <- predVals(toyRun(inputsI(intercept = FALSE)))
  expect_equal(v2[cells], handLogistic3(xHand(0, centreI)), tolerance = 1e-7)
})

test_that("an intercept with a centre is the same model as the intercept i - sum(beta * centre) with none", {
  shift <- 0.4 - (1.5 * centreI$MDC - 0.5 * centreI$youngAge + 1 * centreI$fuelA)
  a <- predVals(toyRun(inputsI()))
  ins <- toyInputs(params = paramsI(shift), formula = "~ 1 + MDC + youngAge + fuelA")
  expect_equal(predVals(toyRun(ins)), a, tolerance = 1e-9)
})

test_that("a row without an intercept or centres predicts exactly as before", {
  v <- predVals(toyRun(inputsI(intercept = FALSE, centre = FALSE)))
  expect_equal(v[cells], handLogistic3(xHand(0)), tolerance = 1e-7)
  expect_identical(v, predVals(toyRun()))
  expect_null(ledgerCovCentre(toyInputs()$studyAreaWithSpreadParams, 1L))
})

test_that("an old ledger row's missing centre is NULL, also beside a row that has one", {
  sa <- data.frame(polygonID = c("A", "B"))
  sa[[centreCol]] <- list(centreI, NULL)         # as rbindlist(fill = TRUE) leaves an older row
  expect_identical(ledgerCovCentre(sa, 1L), centreI)
  expect_null(ledgerCovCentre(sa, 2L))
  expect_null(ledgerCovCentre(data.frame(polygonID = "A"), 1L))
})

test_that("with several ELFs each uses its own intercept and centre; an older ELF's row predicts as before", {
  ins <- multiInputs()
  ## ELF A: intercept and centre (MDC, fuelA); ELF B: none. All coefficients 0 but the intercept.
  ins$studyAreaWithSpreadParams$params[[1]][[icptName]] <- 0.5
  ins$studyAreaWithSpreadParams$params[[1]] <- ins$studyAreaWithSpreadParams$params[[1]][c(1:3, 6, 4:5)]
  ins$studyAreaWithSpreadParams[[centreCol]] <- list(list(MDC = 0.1, fuelA = 0.2), NULL)
  v <- predVals(toyRun(ins, params = list(ELFblendWidth = 6000)))
  ## coefficients are 0, so the centre does not matter here: A is lower + (0.25 - 0.13) * plogis(2 * 0.5)
  expect_equal(v[1], 0.13 + 0.12 * stats::plogis(2 * 0.5), tolerance = 1e-9)
  expect_equal(v[10], 0.20, tolerance = 1e-9)    # B: no intercept, as before
})
