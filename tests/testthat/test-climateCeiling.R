## A ledger row from a fit made with fireSense_spreadFit's spreadClimate = "ceiling" has a power-law link, the climate
## reference (fireSenseUtils::spreadFitClimateRefTxt) and the landscape's p50 (spreadFitPowerLawPc50Txt). Climate then
## moves the ceiling pc toward p50 (fireSenseUtils::spreadLinkInputs()) and x holds the fuels only:
##   shift = sum(beta_clim * (climate - climateRef)); pc = max(p50 - (p50 - pc0) * exp(-shift), floor).
## A row without a climateRef predicts as it always did. Toy covariates and coefficients: test-powerLaw.R (MDC is the climate).

pc50ColCC <- fireSenseUtils::spreadFitPowerLawPc50Txt
refColCC <- fireSenseUtils::spreadFitClimateRefTxt

## MDC / 200 is its rescaled value; the toy fit's climate reference is 0.4 on that scale
inputsCC <- function(gamma = 1.9, link = "powerLaw", ref = list(MDC = 0.4), p50 = 0.3) {
  ins <- inputsPL(gamma = gamma, link = link)
  if (!is.null(ref)) ins$studyAreaWithSpreadParams[[refColCC]] <- list(ref)
  if (!is.null(p50)) ins$studyAreaWithSpreadParams[[pc50ColCC]] <- p50
  ins
}

## by hand, in cell order
handCC <- function(gamma = 1.9, pc0 = 0.25, floor = 0.13, p50 = 0.3, ref = 0.4) {
  cv <- toyCovariates(); o <- order(cv$pixelID)
  shift <- 1.5 * (cv$MDC[o] / 200 - ref)
  x <- -0.5 * cv$youngAge[o] + 1 * cv$fuelA[o] / 8
  pc <- pmax(p50 - (p50 - pc0) * exp(-shift), floor)
  list(p = pmax(pc - (pc - floor) * exp(-sign(x) * abs(x / gamma)), 0), pc = pc, x = x)
}

test_that("ledgerPowerLawPc50() reads the row's p50, NULL when absent or NA", {
  sa <- toyInputs()$studyAreaWithSpreadParams
  expect_null(ledgerPowerLawPc50(sa, 1L))
  sa[[pc50ColCC]] <- 0.3
  expect_identical(ledgerPowerLawPc50(sa, 1L), 0.3)
  sa[[pc50ColCC]] <- NA_real_
  expect_null(ledgerPowerLawPc50(sa, 1L))
})

test_that("a row with climateRef and a power-law link predicts with climate moving the ceiling toward p50", {
  sim <- toyRun(inputsCC())
  h <- handCC()
  expect_equal(predVals(sim)[cellsPL], h$p, tolerance = 1e-9)
  ## the climate is in the ceiling, not in x
  expect_equal(terra::values(sim$fireSense_SpreadLinear, mat = FALSE)[cellsPL], h$x, tolerance = 1e-9)
  ## pc never passes p50, and the climate at the reference gives pc0
  expect_true(all(h$pc <= 0.3))
  expect_equal(handCC(ref = 0.5)$pc[cellsPL == 3L], 0.25)
  expect_false(isTRUE(all.equal(predVals(sim), predVals(toyRun(inputsCC(ref = NULL))))))
})

test_that("fireSense_SpreadPowerLaw is a per-pixel raster, with pc per pixel, even for one ELF", {
  sim <- toyRun(inputsCC())
  pl <- sim$fireSense_SpreadPowerLaw
  expect_s4_class(pl, "SpatRaster")
  expect_identical(names(pl), fireSenseUtils::spreadLinkParNames[["powerLaw"]])
  h <- handCC()
  expect_equal(terra::values(pl[["pc"]], mat = FALSE)[cellsPL], h$pc, tolerance = 1e-9)
  expect_equal(terra::values(pl[["gamma"]], mat = FALSE)[cellsPL], rep(1.9, length(cellsPL)))
  ## how fireSense_burn reads it (its powerLawPars(): one column per name, one row per cell) reproduces p from x
  pars <- vapply(names(pl), function(nm) terra::values(pl[[nm]], mat = FALSE), numeric(terra::ncell(pl)))
  x <- terra::values(sim$fireSense_SpreadLinear, mat = FALSE)
  ok <- !is.na(x)
  expect_false(anyNA(pars[ok, ]))
  expect_equal(fireSenseUtils::linearLinkProb(x[ok], "powerLaw", pars[ok, , drop = FALSE]), predVals(sim)[ok], tolerance = 1e-9)
})

test_that("a row without climateRef is unchanged: the same p, x and a vector of parameters", {
  sim <- toyRun(inputsCC(ref = NULL))
  expect_equal(predVals(sim)[cellsPL], pHandPL(1.9), tolerance = 1e-9)
  expect_identical(sim$fireSense_SpreadPowerLaw, c(pc = 0.25, gamma = 1.9, floor = 0.13, shape = 1))
  expect_identical(predVals(sim), predVals(toyRun(inputsPL())))
})

test_that("a climateRef row without a p50 stops, naming it", {
  expect_error(toyRun(inputsCC(p50 = NULL)), "powerLawPc50")
})

test_that("with several ELFs the per-pixel parameters are blended, an ELF without climateRef contributing its vector", {
  ins <- multiInputs()
  ins$studyAreaWithSpreadParams$params <- lapply(ins$studyAreaWithSpreadParams$params, function(p) { p$MDC <- 1; p })
  ins$studyAreaWithSpreadParams[[linkColPL]] <- c("powerLaw", "powerLaw")
  ins$studyAreaWithSpreadParams[[gammaColPL]] <- c(2, 4)
  ins$studyAreaWithSpreadParams[[pc50ColCC]] <- c(0.3, 0.3)
  ins$studyAreaWithSpreadParams[[refColCC]] <- list(list(MDC = 0.25), NULL)   # ELF A: climate in the ceiling; ELF B: not
  sim <- toyRun(ins, params = list(ELFblendWidth = 20000))
  pl <- sim$fireSense_SpreadPowerLaw
  expect_s4_class(pl, "SpatRaster")
  pc <- terra::values(pl[["pc"]], mat = FALSE)
  ## MDC is 100 everywhere: rescaled 0.5, shift = 1 * (0.5 - 0.25); ELF A's pc, ELF B's maxAsymptote 0.27
  pcA <- 0.3 - (0.3 - 0.25) * exp(-0.25)
  expect_equal(pc[1], pcA, tolerance = 1e-9); expect_equal(pc[10], 0.27, tolerance = 1e-9)
  expect_true(all(pc >= min(pcA, 0.27) - 1e-9 & pc <= max(pcA, 0.27) + 1e-9, na.rm = TRUE))
  ## x has the fuels only: fuelA / 8 (coefficient 0) = 0 in A; with MDC out of x, x is 0 where MDC was the only term
  expect_equal(terra::values(sim$fireSense_SpreadLinear, mat = FALSE)[1], 0, tolerance = 1e-9)
})
