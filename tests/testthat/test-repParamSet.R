## Each prediction replicate uses ONE whole fitted parameter set (ledger row), never an average:
## row ((rep - 1) %% number of sets) + 1, and fireSense_SpreadSD is that same row's yearSpreadSD.

threeSets <- function() {
  p <- rbind(toyParams(), toyParams(), toyParams())
  p$maxAsymptote <- c(0.25, 0.35, 0.20)
  p$MDC <- c(1.5, 0, 3)
  p$yearSpreadSD <- c(0.1, 0.2, 0.4)
  p
}

test_that("rep selects parameter set ((rep - 1) %% nSets) + 1, and the prediction is that set's, not the mean", {
  p <- threeSets()
  for (r in 1:4) {
    row <- ((r - 1) %% 3) + 1
    v <- predVals(toyRun(toyInputs(params = p), params = list(.rep = r)))
    one <- predVals(toyRun(toyInputs(params = p[row, , drop = FALSE])))
    expect_equal(v, one, tolerance = 1e-12, info = paste("rep", r))
  }
  ## the three sets give different maps, so the check above can tell them apart
  vs <- lapply(1:3, function(r) predVals(toyRun(toyInputs(params = p), params = list(.rep = r))))
  expect_gt(max(abs(vs[[1]] - vs[[2]]), na.rm = TRUE), 1e-3)
  expect_gt(max(abs(vs[[1]] - vs[[3]]), na.rm = TRUE), 1e-3)
})

test_that("fireSense_SpreadSD is the yearSpreadSD of the same row as the coefficients", {
  p <- threeSets()
  sds <- vapply(1:4, function(r) toyRun(toyInputs(params = p), params = list(.rep = r))$fireSense_SpreadSD, numeric(1))
  expect_equal(sds, c(0.1, 0.2, 0.4, 0.1))
})

test_that("with several ELFs each ELF takes the modulo of its own number of sets", {
  ins <- multiInputs()
  sa <- ins$studyAreaWithSpreadParams
  ## ELF A has 3 sets (max 0.25, 0.30, 0.35), ELF B has 2 (max 0.27, 0.21); sd likewise
  more <- function(p, max, sd) { p <- p[rep(1L, length(max)), , drop = FALSE]; p$maxAsymptote <- max; p$yearSpreadSD <- sd; p }
  sa$params <- list(more(sa$params[[1]], c(0.25, 0.30, 0.35), c(0.1, 0.2, 0.3)),
                    more(sa$params[[2]], c(0.27, 0.21), c(0.5, 0.7)))
  ins$studyAreaWithSpreadParams <- sa
  ## all coefficients are 0: each ELF's value is lower + (max - lower) / 2
  lev <- function(max) 0.13 + (max - 0.13) / 2
  for (r in 1:4) {
    sim <- toyRun(ins, params = list(.rep = r, ELFblendWidth = 6000))
    a <- ((r - 1) %% 3) + 1; b <- ((r - 1) %% 2) + 1
    v <- predVals(sim); sdv <- terra::values(sim$fireSense_SpreadSD, mat = FALSE)
    expect_equal(v[1], lev(sa$params[[1]]$maxAsymptote[a]), tolerance = 1e-9, info = paste("rep", r))
    expect_equal(v[10], lev(sa$params[[2]]$maxAsymptote[b]), tolerance = 1e-9, info = paste("rep", r))
    expect_equal(sdv[1], sa$params[[1]]$yearSpreadSD[a], tolerance = 1e-9, info = paste("rep", r))
    expect_equal(sdv[10], sa$params[[2]]$yearSpreadSD[b], tolerance = 1e-9, info = paste("rep", r))
  }
})
