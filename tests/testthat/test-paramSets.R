## paramSets / repsPerSet: the simulated ELF's parameter sets come from a file; replicate r uses set
## ceiling(r / repsPerSet); a set's surface is computed once.

twoSets <- function() {
  p <- rbind(toyParams(), toyParams())
  p$maxAsymptote <- c(0.20, 0.35)
  p$MDC <- c(1.5, 3)
  p$yearSpreadSD <- c(0.1, 0.2)
  p
}

setsFile <- function(p, ext = "rds") {
  f <- tempfile(fileext = paste0(".", ext))
  if (ext == "rds") saveRDS(p, f) else utils::write.csv(p, f, row.names = FALSE)
  f
}

## one job with `n` replicates, as the hindcast runs them
runReps <- function(ins, params, n) {
  sim <- toySim(ins, params = c(params, list(paramSetByReplicate = TRUE)), times = list(start = 1, end = 1))
  sets <- integer(0); vals <- list()
  for (r in seq_len(n)) {
    end(sim) <- r
    sim <- SpaDES.core::spades(sim, debug = FALSE)
    sets <- c(sets, unname(sim$fireSense_SpreadParamSet)); vals[[r]] <- predVals(sim)
  }
  list(sets = sets, vals = vals)
}

test_that("default: paramSets NA gives the same output as before", {
  p <- twoSets()
  expect_equal(predVals(toyRun(toyInputs(params = p), params = list(.rep = 2L))),
               predVals(toyRun(toyInputs(params = p), params = list(.rep = 2L, paramSets = NA_character_, repsPerSet = 5L))))
})

test_that("2 sets x 2 reps: reps 1-2 use set 1, reps 3-4 set 2, each with its own surface and sd", {
  p <- twoSets()
  ledger <- toyParams()   ## the ledger row is not one of the sets
  for (ext in c("rds", "csv")) {
    res <- runReps(toyInputs(params = ledger), list(paramSets = setsFile(p, ext), repsPerSet = 2L), 4)
    expect_identical(res$sets, c(1L, 1L, 2L, 2L))
    for (r in 1:4)
      expect_equal(res$vals[[r]], predVals(toyRun(toyInputs(params = p[(r - 1) %/% 2 + 1, , drop = FALSE]))),
                   tolerance = 1e-12, info = paste(ext, "rep", r))
  }
  expect_gt(max(abs(res$vals[[1]] - res$vals[[3]]), na.rm = TRUE), 1e-3)
  sd <- vapply(c(1, 3), function(r) toyRun(toyInputs(params = ledger),
                 params = list(.rep = r, paramSets = setsFile(p), repsPerSet = 2L))$fireSense_SpreadSD, numeric(1))
  expect_equal(sd, c(0.1, 0.2))
})

test_that("reps of one set reuse its surface: it is computed once per set", {
  sim <- toySim(toyInputs(), params = list(paramSets = setsFile(twoSets()), repsPerSet = 2L, paramSetByReplicate = TRUE))
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  v1 <- predVals(sim)
  ## plant a marker in the stored surface of set 1: a rep of the same set returns it, a rep of set 2 does not
  surf <- sim@.xData$.modObjs[[moduleName]]$surface
  expect_identical(surf$key[[1]], 1L)
  sim@.xData$.modObjs[[moduleName]]$surface$outputs$fireSense_SpreadPredicted[1] <- 99
  end(sim) <- 2
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  expect_identical(predVals(sim)[1], 99)
  expect_identical(unname(sim$fireSense_SpreadParamSet), 1L)
  end(sim) <- 3
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  expect_identical(unname(sim$fireSense_SpreadParamSet), 2L)
  expect_false(identical(predVals(sim)[1], 99))
  expect_false(isTRUE(all.equal(predVals(sim), v1)))
})

test_that("every rep k of a set seeds the random numbers the same for every set", {
  f <- setsFile(twoSets())
  draw <- function(r) { toyRun(toyInputs(), params = list(.rep = r, paramSets = f, repsPerSet = 2L)); stats::runif(1) }
  d <- vapply(1:4, draw, numeric(1))
  expect_equal(d[1], d[3]); expect_equal(d[2], d[4]); expect_false(d[1] == d[2])
})

test_that("a set equal to the ledger row reproduces the ledger surface identically", {
  led <- toyParams(yearSpreadSD = 0.3)
  expect_identical(predVals(toyRun(toyInputs(params = led), params = list(paramSets = setsFile(led)))),
                   predVals(toyRun(toyInputs(params = led))))
})

test_that("a set lacking a coefficient of the ledger row stops with a clear error", {
  p <- twoSets(); p$fuelA <- NULL
  expect_error(toyRun(toyInputs(), params = list(paramSets = setsFile(p))), "lacks the coefficient.*fuelA")
})

test_that("with several ELFs only the simulated ELF's rows are replaced; the set index is reported by ELF", {
  ins <- multiInputs()
  ins$.ELFind <- "B"
  p <- ins$studyAreaWithSpreadParams$params[[2]][c(1, 1), ]
  p$maxAsymptote <- c(0.21, 0.31)
  sim <- toyRun(ins, params = list(.rep = 2L, ELFblendWidth = 6000, paramSets = setsFile(p), repsPerSet = 1L))
  expect_identical(unname(sim$fireSense_SpreadParamSet), c(1L, 2L))
  expect_equal(predVals(sim)[1], 0.13 + (0.25 - 0.13) / 2, tolerance = 1e-9)   ## A unchanged
  expect_equal(predVals(sim)[10], 0.13 + (0.31 - 0.13) / 2, tolerance = 1e-9)  ## B is set 2
})
