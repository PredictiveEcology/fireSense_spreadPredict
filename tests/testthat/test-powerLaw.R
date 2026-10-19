## A ledger row from a fit made with fireSense_spreadFit's spreadLink = "powerLaw" carries "powerLaw" in
## fireSenseUtils::spreadLinkTxt and the fit's gamma in fireSenseUtils::spreadFitPowerLawGammaTxt (the columns
## fireSense_spreadFit's spreadFitLedgerRow() writes). Prediction applies the same link through
## fireSenseUtils::spreadProbFromCovs(): p = pc - (pc - 0.13) * exp(-x / gamma), x = covariates %*% coefficients,
## pc the row's maxAsymptote. A row of any other link predicts as it always did.
##
## The toy covariates are those of test-spreadProb-values.R: MDC / 200, youngAge and fuelA / 8; the toy parameters
## have maxAsymptote = 0.25, hillSlope1 = 2, and coefficients 1.5, -0.5, 1 (the power law ignores hillSlope1:
## the fit fixes it at 1, and it is the linear predictor that p depends on).

linkColPL <- fireSenseUtils::spreadLinkTxt
gammaColPL <- fireSenseUtils::spreadFitPowerLawGammaTxt
cellsPL <- c(1, 2, 3, 4, 6, 7, 8)

inputsPL <- function(gamma = 1.9, link = "powerLaw") {
  ins <- toyInputs()
  if (!is.null(link)) ins$studyAreaWithSpreadParams[[linkColPL]] <- link
  if (!is.null(gamma)) ins$studyAreaWithSpreadParams[[gammaColPL]] <- gamma
  ins
}

## by hand, in cell order
pHandPL <- function(gamma = 1.9, pc = 0.25, floor = 0.13) {
  cv <- toyCovariates(); o <- order(cv$pixelID)
  x <- 1.5 * cv$MDC[o] / 200 - 0.5 * cv$youngAge[o] + 1 * cv$fuelA[o] / 8
  pmax(pc - (pc - floor) * exp(-x / gamma), 0)
}

test_that("a powerLaw row predicts p = pc - (pc - floor) * exp(-x / gamma), with its own gamma", {
  v <- predVals(toyRun(inputsPL()))
  expect_equal(v[cellsPL], pHandPL(1.9), tolerance = 1e-9)
  expect_true(all(is.na(v[c(5, 9)])))
  ## cell 1: x = 0, so the floor; cell 2 (youngAge 1, x < 0) is below it; cell 4 (x = 2.5) is near pc and below it
  expect_equal(v[1], 0.13, tolerance = 1e-9)
  expect_lt(v[2], 0.13)
  expect_lt(v[4], 0.25)
  expect_equal(predVals(toyRun(inputsPL(gamma = 4)))[cellsPL], pHandPL(4), tolerance = 1e-9)
  ## the logit link of the same row would give something else
  expect_false(isTRUE(all.equal(v, predVals(toyRun()))))
})

test_that("a row of any other link predicts exactly as before", {
  base <- predVals(toyRun())
  expect_identical(predVals(toyRun(inputsPL(gamma = NULL, link = NULL))), base)
  expect_identical(predVals(toyRun(inputsPL(gamma = 4, link = "ceiling"))), base)   # gamma ignored
  sa <- toyInputs()$studyAreaWithSpreadParams
  expect_null(ledgerPowerLawGamma(sa, 1L))
  sa[[linkColPL]] <- "climateOutside"
  expect_null(ledgerPowerLawGamma(sa, 1L))
  sa[[linkColPL]] <- "powerLaw"; sa[[gammaColPL]] <- 2.2
  expect_identical(ledgerPowerLawGamma(sa, 1L), 2.2)
  ## a powerLaw row without a usable gamma stops, instead of predicting with the wrong link
  sa[[gammaColPL]] <- NA_real_
  expect_error(ledgerPowerLawGamma(sa, 1L), "no valid")
  sa[[gammaColPL]] <- NULL
  expect_error(ledgerPowerLawGamma(sa, 1L), "no valid")
})

test_that("with several ELFs each row applies its own link, blended like the probabilities", {
  ins <- multiInputs()
  ## ELF A is a power-law fit (gamma 3), ELF B the default link; every other coefficient is 0 (as in test-climateOutside.R)
  ins$studyAreaWithSpreadParams$params <- lapply(ins$studyAreaWithSpreadParams$params, function(p) { p$MDC <- 1; p })
  ins$studyAreaWithSpreadParams[[linkColPL]] <- c("powerLaw", "ceiling")
  ins$studyAreaWithSpreadParams[[gammaColPL]] <- c(3, NA)
  v <- predVals(toyRun(ins, params = list(ELFblendWidth = 20000)))
  pA <- pmax(0.25 - (0.25 - 0.13) * exp(-0.5 / 3), 0)      # MDC is 100 everywhere: x = 1 * 0.5; pc = ELF A's maxAsymptote
  pB <- handLogistic3(0.5, upper = 0.27)                    # ELF B keeps the logistic (its hillSlope1 is 2)
  rA <- c(1, 1, 1, 1, 1, 0.75, 0.5, 0.25, 0, 0); rB <- rev(rA)
  expect_equal(v, (rA * pA + rB * pB) / (rA + rB), tolerance = 1e-9)
  expect_equal(v[1], pA, tolerance = 1e-9)
  expect_equal(v[10], pB, tolerance = 1e-9)
})

test_that("a powerLaw row with a year effect warns, once, that fireSense_burn adds it on the logit", {
  ins <- inputsPL()
  ins$studyAreaWithSpreadParams$params <- lapply(ins$studyAreaWithSpreadParams$params, function(p) { p$yearSpreadSD <- 0.3; p })
  expect_warning(sim <- toyRun(ins), "fireSense_burn adds fireSense_SpreadSD on the")
  expect_equal(predVals(sim)[cellsPL], pHandPL(1.9), tolerance = 1e-9)
  expect_equal(sim$fireSense_SpreadSD, 0.3)
  ## no warning without a year effect, or with the default link
  expect_no_warning(toyRun(inputsPL()))
  ins2 <- toyInputs()
  ins2$studyAreaWithSpreadParams$params <- lapply(ins2$studyAreaWithSpreadParams$params, function(p) { p$yearSpreadSD <- 0.3; p })
  expect_no_warning(toyRun(ins2))
})
