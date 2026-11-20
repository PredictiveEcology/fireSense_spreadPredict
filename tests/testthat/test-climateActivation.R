## A ledger row from a fit made with fireSense_spreadFit's spreadClimate = "activation" records the mode
## (fireSenseUtils::spreadClimateTxt), has a "powerLawSmooth" link at the floor, the climate reference, p50, p50 as
## maxAsymptote and an activationIntercept. Fuel then sets each pixel's ceiling and climate is x
## (fireSenseUtils::spreadLinkInputs()):
##   pc = floor + (p50 - floor) * plogis(eta), eta = the non-climate terms; x = a + sum(beta_clim * (climate - climateRef)).
## Toy covariates and coefficients: setup-toySpread.R (MDC is the climate, rescaled MDC / 200).

modeColCA <- fireSenseUtils::spreadClimateTxt
actTxtCA <- fireSenseUtils::activationInterceptTxt

inputsCA <- function(mode = "activation", a = 0.8, ref = list(MDC = 0.4), p50 = 0.3, shape = 0.5) {
  ins <- inputsPL(gamma = 1.9, link = "powerLawSmooth")
  sa <- ins$studyAreaWithSpreadParams
  sa[[fireSenseUtils::spreadFitLowerAsymptoteTxt]] <- 0.13
  sa[[shapeColPL]] <- shape
  sa[[fireSenseUtils::spreadFitClimateRefTxt]] <- list(ref)
  sa[[fireSenseUtils::spreadFitPowerLawPc50Txt]] <- p50
  if (!is.null(mode)) sa[[modeColCA]] <- mode
  pars <- sa$params[[1]]
  pars$maxAsymptote <- p50
  if (!is.null(a)) pars[[actTxtCA]] <- a
  sa$params <- list(pars)
  ins$studyAreaWithSpreadParams <- sa
  ins
}

## by hand, in cell order
handCA <- function(a = 0.8, p50 = 0.3, floor = 0.13, ref = 0.4, gamma = 1.9, shape = 0.5) {
  cv <- toyCovariates(); o <- order(cv$pixelID)
  eta <- -0.5 * cv$youngAge[o] + 1 * cv$fuelA[o] / 8
  pc <- floor + (p50 - floor) * plogis(eta)
  x <- a + 1.5 * (cv$MDC[o] / 200 - ref)
  list(p = ifelse(x >= 0, pc - (pc - floor) * exp(-(pmax(x, 0) / gamma)^shape), floor), pc = pc, x = x)
}

test_that("an activation row predicts with fuel setting each pixel's ceiling and climate in x", {
  sim <- toyRun(inputsCA())
  h <- handCA()
  expect_equal(predVals(sim)[cellsPL], h$p, tolerance = 1e-9)
  expect_true(all(predVals(sim)[cellsPL] <= 0.3))
  expect_equal(terra::values(sim$fireSense_SpreadLinear, mat = FALSE)[cellsPL], h$x, tolerance = 1e-9)
  pl <- sim$fireSense_SpreadPowerLaw
  expect_s4_class(pl, "SpatRaster")
  expect_identical(names(pl), fireSenseUtils::spreadLinkParNames[["powerLawSmooth"]])
  expect_equal(terra::values(pl[["pc"]], mat = FALSE)[cellsPL], h$pc, tolerance = 1e-9)
  ## the intercept is not a covariate: the random effects come through as they did, and the row predicts as activation,
  ## not as climate in the ceiling (the same row without the mode)
  expect_false(isTRUE(all.equal(predVals(sim), predVals(toyRun(inputsCA(mode = NULL, a = NULL))))))
})

test_that("fireSense_burn's reading of the outputs gives the predicted p, and the fit's own link gives it too", {
  sim <- toyRun(inputsCA())
  pl <- sim$fireSense_SpreadPowerLaw
  ## as fireSense_burn's powerLawPars(): one column per name, one row per cell
  pars <- vapply(names(pl), function(nm) terra::values(pl[[nm]], mat = FALSE), numeric(terra::ncell(pl)))
  x <- terra::values(sim$fireSense_SpreadLinear, mat = FALSE)
  ok <- !is.na(x)
  expect_false(anyNA(pars[ok, ]))
  expect_equal(fireSenseUtils::linearLinkProb(x[ok], "powerLawSmooth", pars[ok, , drop = FALSE]), predVals(sim)[ok],
               tolerance = 1e-9)
  ## the fit's objective calls spreadLinkInputs() on its design matrix (rescaled covariates, in the formula's order)
  cv <- toyCovariates(); o <- order(cv$pixelID)
  mat <- cbind(MDC = cv$MDC[o] / 200, youngAge = cv$youngAge[o], fuelA = cv$fuelA[o] / 8)
  fit <- fireSenseUtils::spreadLinkInputs(mat, c(0.3, 2, 1), c(MDC = 1.5, youngAge = -0.5, fuelA = 1), 0.13,
                                          "powerLawSmooth", list(MDC = 0.4), 0.3, powerLawGamma = 1.9, powerLawShape = 0.5,
                                          lowerAsymptote = 0.13, activationIntercept = 0.8)
  expect_equal(predVals(sim)[cellsPL], fireSenseUtils::linearLinkProb(fit$x, "powerLawSmooth", fit$pars), tolerance = 1e-12)
})

test_that("a row without the mode column predicts as it did: ceiling with climateRef and a power-law link", {
  ceil <- inputsCA(mode = NULL, a = NULL)
  sa <- ceil$studyAreaWithSpreadParams
  expect_identical(ledgerSpreadClimate(sa, 1L), "ceiling")
  sa[[modeColCA]] <- "ceiling"
  ceil2 <- ceil; ceil2$studyAreaWithSpreadParams <- sa
  expect_identical(predVals(toyRun(ceil)), predVals(toyRun(ceil2)))
  ## and a row with neither is "predictor"
  sa0 <- inputsPL()$studyAreaWithSpreadParams
  expect_identical(ledgerSpreadClimate(sa0, 1L), "predictor")
  expect_false("spreadClimate" %in% names(ledgerLinkArgs(sa0, 1L)))
  expect_identical(ledgerLinkArgs(inputsCA()$studyAreaWithSpreadParams, 1L)$spreadClimate, "activation")
})

test_that("an activation row and its parameters must agree, and the link must be powerLawSmooth at the floor", {
  expect_error(toyRun(inputsCA(a = NULL)), "has no 'activationIntercept'")
  expect_error(toyRun(inputsCA(mode = "ceiling")), "is not 'activation'")
  bad <- inputsCA()
  bad$studyAreaWithSpreadParams[[fireSenseUtils::spreadFitLowerAsymptoteTxt]] <- 0.05
  expect_error(toyRun(bad), "lowerAsymptote = lowerSpreadProb")
  sa <- inputsCA()$studyAreaWithSpreadParams
  sa[[modeColCA]] <- "wet"
  expect_error(ledgerSpreadClimate(sa, 1L), "unknown")
})
