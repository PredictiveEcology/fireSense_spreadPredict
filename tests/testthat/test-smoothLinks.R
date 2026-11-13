## Ledger rows of fireSense_spreadFit's spreadLink = "powerLawSmooth" and "richards": the link in
## fireSenseUtils::spreadLinkTxt, the lower asymptote in spreadFitLowerAsymptoteTxt, nu in spreadFitRichardsNuTxt (and gamma
## and shape for "powerLawSmooth" only). Prediction applies the same link through fireSenseUtils::spreadProbFromCovs(), and
## hands fireSense_burn the link (fireSense_SpreadLink) with its parameters (fireSense_SpreadPowerLaw) and x.
## Same toy covariates as test-powerLaw.R.

linkColSL <- fireSenseUtils::spreadLinkTxt
lowerColSL <- fireSenseUtils::spreadFitLowerAsymptoteTxt
nuColSL <- fireSenseUtils::spreadFitRichardsNuTxt
gammaColSL <- fireSenseUtils::spreadFitPowerLawGammaTxt
cellsSL <- c(1, 2, 3, 4, 6, 7, 8)

## a ledger row as the fit writes it; `gamma` and `nu` only for the links that have them
inputsSL <- function(link, lower = 0.05, gamma = 1.9, nu = 0.5) {
  ins <- toyInputs()
  sa <- ins$studyAreaWithSpreadParams
  sa[[linkColSL]] <- link
  sa[[lowerColSL]] <- lower
  if (link == "powerLawSmooth") sa[[gammaColSL]] <- gamma
  if (link == "richards") sa[[nuColSL]] <- nu
  ins$studyAreaWithSpreadParams <- sa
  ins
}

## the linear predictor of the toy covariates, in cell order
xSL <- function() {
  cv <- toyCovariates(); o <- order(cv$pixelID)
  1.5 * cv$MDC[o] / 200 - 0.5 * cv$youngAge[o] + 1 * cv$fuelA[o] / 8
}

test_that("a powerLawSmooth row predicts the smooth link with its gamma and lower asymptote", {
  for (L in c(0, 0.05, 0.13)) {
    sim <- toyRun(inputsSL("powerLawSmooth", lower = L, gamma = 3))
    v <- predVals(sim)
    expect_equal(v[cellsSL], fireSenseUtils::powerLawSmoothLink(xSL(), 0.25, 3, 0.13, 1, L), tolerance = 1e-9)
    expect_true(all(is.na(v[c(5, 9)])))
    expect_true(all(v[cellsSL] >= L))
    expect_gt(min(v[cellsSL]), 0 - 1e-12)
    ## the x < 0 cell (2) is the power law's 0 only without a lower asymptote
    expect_equal(v[2] >= L, TRUE)
    expect_identical(sim$fireSense_SpreadLink, "powerLawSmooth")
    expect_identical(sim$fireSense_SpreadPowerLaw, c(pc = 0.25, gamma = 3, floor = 0.13, shape = 1, lower = L))
    expect_equal(terra::values(sim$fireSense_SpreadLinear, mat = FALSE)[cellsSL], xSL(), tolerance = 1e-9)
  }
  ## the same x on a "powerLaw" row is below the floor and, once the link clips it, 0 on a low enough x
  expect_equal(fireSenseUtils::powerLawLink(-50, 0.25, 3, 0.13), 0)
})

test_that("a richards row predicts the Richards link with no gamma in the ledger row, and hands burn pc, lower, nu, B", {
  ins <- inputsSL("richards", lower = 0.02, nu = 2)
  expect_false(gammaColSL %in% names(ins$studyAreaWithSpreadParams))
  sim <- toyRun(ins)
  v <- predVals(sim)
  expect_equal(v[cellsSL], fireSenseUtils::richardsLink(xSL(), 0.25, 0.02, 2), tolerance = 1e-9)
  expect_true(all(v[cellsSL] >= 0.02))
  expect_identical(sim$fireSense_SpreadLink, "richards")
  expect_identical(sim$fireSense_SpreadPowerLaw,
                   c(pc = 0.25, lower = 0.02, nu = 2, B = 1 / fireSenseUtils::spreadPowerLawGamma))
  expect_equal(terra::values(sim$fireSense_SpreadLinear, mat = FALSE)[cellsSL], xSL(), tolerance = 1e-9)
  expect_null(ledgerPowerLawGamma(ins$studyAreaWithSpreadParams, 1L))
  ## nu = 0 is Gompertz
  g <- predVals(toyRun(inputsSL("richards", lower = 0.13, nu = 0)))
  expect_equal(g[cellsSL], 0.13 + (0.25 - 0.13) * exp(-exp(-xSL() / fireSenseUtils::spreadPowerLawGamma)), tolerance = 1e-9)
})

test_that("with L = 0.13 a low-x pixel keeps 0.13 on both new links, and the old links leave nothing for burn to read", {
  for (lk in c("powerLawSmooth", "richards")) {
    ins <- inputsSL(lk, lower = 0.13)
    ins$studyAreaWithSpreadParams$params <- lapply(ins$studyAreaWithSpreadParams$params, function(p) { p$youngAge <- -40; p })
    v <- predVals(toyRun(ins))
    expect_equal(v[2], 0.13, tolerance = 1e-9)    # cell 2: youngAge 1
  }
  expect_null(toyRun()$fireSense_SpreadLink)
})

test_that("rows without the columns their link needs stop; the helpers read the columns", {
  sa <- inputsSL("richards")$studyAreaWithSpreadParams
  expect_identical(ledgerLowerAsymptote(sa, 1L), 0.05)
  expect_identical(ledgerRichardsNu(sa, 1L), 0.5)
  expect_identical(ledgerLinkArgs(sa, 1L)$link, "richards")
  expect_false("powerLawGamma" %in% names(ledgerLinkArgs(sa, 1L)))
  sa[[nuColSL]] <- NULL
  expect_error(ledgerRichardsNu(sa, 1L), "richardsNu_spread")
  sa <- inputsSL("powerLawSmooth")$studyAreaWithSpreadParams
  sa[[lowerColSL]] <- 0.5
  expect_error(ledgerLowerAsymptote(sa, 1L), "lowerAsymptote_spread")
  sa[[lowerColSL]] <- NULL
  expect_error(ledgerLowerAsymptote(sa, 1L), "lowerAsymptote_spread")
  expect_null(ledgerRichardsNu(inputsSL("powerLawSmooth")$studyAreaWithSpreadParams, 1L))
  ## the other links have none
  expect_null(ledgerLowerAsymptote(toyInputs()$studyAreaWithSpreadParams, 1L))
  expect_identical(ledgerLinkArgs(toyInputs()$studyAreaWithSpreadParams, 1L), list(powerLawShape = 1))
})

test_that("with several ELFs of one new link the parameters blend; with different links it stops", {
  ins <- multiInputs()
  ins$studyAreaWithSpreadParams$params <- lapply(ins$studyAreaWithSpreadParams$params, function(p) { p$MDC <- 1; p })
  ins$studyAreaWithSpreadParams[[linkColSL]] <- c("richards", "richards")
  ins$studyAreaWithSpreadParams[[lowerColSL]] <- c(0, 0.1)
  ins$studyAreaWithSpreadParams[[nuColSL]] <- c(0.5, 2)
  sim <- toyRun(ins, params = list(ELFblendWidth = 20000))
  pl <- sim$fireSense_SpreadPowerLaw
  expect_identical(names(pl), c("pc", "lower", "nu", "B"))
  lo <- terra::values(pl[["lower"]], mat = FALSE)
  expect_equal(lo[1], 0); expect_equal(lo[10], 0.1)
  expect_identical(sim$fireSense_SpreadLink, "richards")
  ins$studyAreaWithSpreadParams[[linkColSL]] <- c("richards", "powerLawSmooth")
  ins$studyAreaWithSpreadParams[[gammaColSL]] <- c(NA, 2)
  expect_error(toyRun(ins, params = list(ELFblendWidth = 20000)), "different spread links")
})
