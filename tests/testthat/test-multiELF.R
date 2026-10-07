## Several fitted ELFs in one study area (the 2-ELF Mackenzie forecast, 2026-09). Every pixel gets a spread
## probability. Each ELF's model predicts its own pixels and those within ELFblendWidth of them; where two
## overlap they are averaged with weights falling linearly from 1 inside an ELF to 0 at ELFblendWidth
## outside it, normalised to sum to 1 (Eliot, 2026-09-22).
##
## Toy: one row of 10 pixels, 5 km wide; ELF "A" is columns 1-5, "B" columns 6-10. All coefficients are 0,
## so each ELF predicts a constant: lower + (maxAsymptote - lower) / 2 = 0.19 for A (max 0.25) and
## 0.20 for B (max 0.27). A is fitted on fuelA, B on fuelB; the covariate table holds both.

test_that("each ELF predicts its own pixels, and the boundary is a distance-weighted blend", {
  v <- predVals(toyRun(multiInputs(), params = list(ELFblendWidth = 20000)))
  ## raw weights at pixel centres (5 km apart): A = 1 on 1-5, then 0.75, 0.5, 0.25, 0, 0 on 6-10
  rA <- c(1, 1, 1, 1, 1, 0.75, 0.5, 0.25, 0, 0); rB <- rev(rA)
  expect_equal(v, (rA * 0.19 + rB * 0.20) / (rA + rB), tolerance = 1e-9)
  expect_equal(v[1], 0.19, tolerance = 1e-9)             # 25 km from B: A only
  expect_equal(v[10], 0.20, tolerance = 1e-9)
  expect_equal(v[5], (0.19 + 0.75 * 0.20) / 1.75, tolerance = 1e-9)
  expect_true(all(is.finite(v)))                         # every pixel has a probability
})

test_that("a narrow blend width leaves only the boundary pixels mixed", {
  v <- predVals(toyRun(multiInputs(), params = list(ELFblendWidth = 6000)))
  ## 5 km from the other ELF: raw weight 1 - 5/6
  expect_equal(v[c(1:4, 7:10)], rep(c(0.19, 0.20), each = 4), tolerance = 1e-9)
  expect_equal(v[5], (0.19 + (1/6) * 0.20) / (1 + 1/6), tolerance = 1e-9)
})

test_that("with several ELFs, a missing ELF raster stops with a message that says what is needed", {
  ins <- multiInputs(); ins$rasterToMatchLargeELF <- NULL
  expect_error(toyRun(ins), "rasterToMatchLargeELF")
})

## ---- the per-year random effect (yearSpreadSD) ----
withSD <- function(inputs, sds) {
  inputs$studyAreaWithSpreadParams$params <- Map(function(p, s) { p$yearSpreadSD <- s; p },
                                                 inputs$studyAreaWithSpreadParams$params, sds)
  inputs
}

test_that("yearSpreadSD does not change the spread probability, and is blended into fireSense_SpreadSD", {
  base <- toyRun(multiInputs(), params = list(ELFblendWidth = 20000))
  sim <- toyRun(withSD(multiInputs(), c(0.2, 0.6)), params = list(ELFblendWidth = 20000))
  expect_equal(predVals(sim), predVals(base), tolerance = 1e-12)
  sdv <- terra::values(sim$fireSense_SpreadSD, mat = FALSE)
  rA <- c(1, 1, 1, 1, 1, 0.75, 0.5, 0.25, 0, 0); rB <- rev(rA)
  expect_equal(sdv, (rA * 0.2 + rB * 0.6) / (rA + rB), tolerance = 1e-9)   # the probabilities' weights
  expect_equal(sdv[c(1, 10)], c(0.2, 0.6))
})

test_that("without yearSpreadSD in the fits the sd is 0 everywhere", {
  sim <- toyRun(multiInputs(), params = list(ELFblendWidth = 20000))
  expect_true(all(terra::values(sim$fireSense_SpreadSD, mat = FALSE) == 0))
})
