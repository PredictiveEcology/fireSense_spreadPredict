## A ledger row from a fit made with climate outside the maxAsymptote ceiling (fireSense_spreadFit's
## spreadLink = "climateOutside") carries, in fireSenseUtils::spreadFitClimateRefTxt, the fit's reference value of
## each climate covariate (on the rescaled scale), and "climateOutside" in fireSenseUtils::spreadLinkTxt. Prediction
## applies the same link through fireSenseUtils::spreadProbFromCovs(), the function the fit used:
## p0 = logistic of the other covariates, under the ceiling; p = plogis(qlogis(p0) + beta_clim * (climate - ref)),
## uncapped. A row without a reference predicts as it always did.
##
## The toy covariates are those of test-spreadProb-values.R: MDC / 200 is the climate covariate, youngAge and
## fuelA / 8 the rest; the toy parameters have hillSlope1 = 2, so p0 = 0.13 + 0.12 * plogis(2 * x_fuel), and the
## climate shift is beta_clim * (MDC / 200 - ref) with no slope factor (the fit fixes hillSlope1 at 1).

refCol <- fireSenseUtils::spreadFitClimateRefTxt
linkCol <- fireSenseUtils::spreadLinkTxt
cellsCO <- c(1, 2, 3, 4, 6, 7, 8)

inputsCO <- function(ref = list(MDC = 0.4), link = "climateOutside") {
  ins <- toyInputs()
  if (!is.null(link)) ins$studyAreaWithSpreadParams[[linkCol]] <- link
  ins$studyAreaWithSpreadParams[[refCol]] <- list(ref)
  ins
}

## by hand, in cell order
pHandCO <- function(ref = 0.4) {
  cv <- toyCovariates(); o <- order(cv$pixelID)
  mdc <- cv$MDC[o] / 200; ya <- cv$youngAge[o]; fuel <- cv$fuelA[o] / 8
  p0 <- handLogistic3(-0.5 * ya + 1 * fuel)
  plogis(qlogis(p0) + 1.5 * (mdc - ref))
}

test_that("a row with a climate reference predicts climate on the logit after the fuel ceiling, uncapped", {
  v <- predVals(toyRun(inputsCO()))
  expect_equal(v[cellsCO], pHandCO(0.4), tolerance = 1e-7)
  expect_true(all(is.na(v[c(5, 9)])))
  ## cell 4 (MDC 200, fuelA 8): p0 = 0.13 + 0.12 * plogis(2) = 0.2358, then + 1.5 * (1 - 0.4) on the logit
  expect_equal(v[4], plogis(qlogis(handLogistic3(1)) + 0.9), tolerance = 1e-7)
  expect_gt(v[4], 0.25)                      # above maxAsymptote: the ceiling no longer binds for climate
  ## cell 1 (MDC 0, no fuel): p0 = 0.19, then 1.5 * (0 - 0.4) = -0.6 on the logit: below p0
  expect_equal(v[1], plogis(qlogis(0.19) - 0.6), tolerance = 1e-7)
  ## at climate = the reference the new link is p0: cell 3 has MDC 100 -> 0.5, so with ref 0.5 it is the
  ## fuel-only logistic (fuelA 4 -> 0.5, x_fuel = 0.5)
  v2 <- predVals(toyRun(inputsCO(ref = list(MDC = 0.5))))
  expect_equal(v2[3], handLogistic3(0.5), tolerance = 1e-7)
  expect_equal(v2[cellsCO], pHandCO(0.5), tolerance = 1e-7)
})

test_that("a row without a reference predicts exactly as before", {
  base <- predVals(toyRun())
  ## the column is there but empty: a fit with climate under the ceiling, written beside one without
  expect_identical(predVals(toyRun(inputsCO(ref = list(), link = "ceiling"))), base)
  expect_identical(predVals(toyRun(inputsCO(ref = NULL, link = NULL))), base)
  ## and the reference changes the prediction only where climate differs from it
  withRef <- predVals(toyRun(inputsCO(ref = list(MDC = 0.5))))
  expect_false(isTRUE(all.equal(withRef, base)))
  sa <- toyInputs()$studyAreaWithSpreadParams
  expect_null(ledgerClimateRef(sa, 1L))
  sa[[refCol]] <- list(list())
  expect_null(ledgerClimateRef(sa, 1L))
  sa[[refCol]] <- list(list(MDC = 0.4))
  expect_identical(ledgerClimateRef(sa, 1L), list(MDC = 0.4))
})

test_that("with several ELFs each row applies its own reference, blended like the probabilities", {
  ins <- multiInputs()
  ## a climate effect for both ELFs; MDC is 100 everywhere, i.e. 0.5 rescaled
  ins$studyAreaWithSpreadParams$params <- lapply(ins$studyAreaWithSpreadParams$params, function(p) { p$MDC <- 1; p })
  ins$studyAreaWithSpreadParams[[linkCol]] <- "climateOutside"
  ins$studyAreaWithSpreadParams[[refCol]] <- list(list(MDC = 0.5), list(MDC = 0.25))
  v <- predVals(toyRun(ins, params = list(ELFblendWidth = 20000)))
  pA <- 0.19                                     # at its reference: the fuel-only value (all other coefficients 0)
  pB <- plogis(qlogis(0.20) + 1 * (0.5 - 0.25))  # 0.25 above its reference on the logit
  rA <- c(1, 1, 1, 1, 1, 0.75, 0.5, 0.25, 0, 0); rB <- rev(rA)
  expect_equal(v, (rA * pA + rB * pB) / (rA + rB), tolerance = 1e-9)
  expect_equal(v[1], pA, tolerance = 1e-9)
  expect_equal(v[10], pB, tolerance = 1e-9)
})
