## Which events are scheduled, and when.

test_that("run repeats every .runInterval from .runInitialTime, at priority 5.12", {
  sim <- toyRun(times = list(start = 1, end = 3))
  done <- evOf(SpaDES.core::completed(sim), "run")
  expect_equal(done$eventTime, c(1, 2, 3))
  expect_equal(unique(done$eventPriority), 5.12)
  ## the next one is queued for year 4
  nxt <- evOf(SpaDES.core::events(sim), "run")
  expect_equal(nxt$eventTime, 4)
  expect_equal(nxt$eventPriority, 5.12)
  expect_equal(evOf(SpaDES.core::completed(sim), "init")$eventTime, 1)
})

test_that(".runInitialTime delays the first run; .runInterval sets the step", {
  sim <- toyRun(params = list(.runInitialTime = 2, .runInterval = 2),
                times = list(start = 1, end = 5))
  expect_equal(evOf(SpaDES.core::completed(sim), "run")$eventTime, c(2, 4))
  expect_equal(evOf(SpaDES.core::events(sim), "run")$eventTime, 6)
})

test_that(".runInterval = NA predicts once", {
  sim <- toyRun(params = list(.runInterval = NA_real_), times = list(start = 1, end = 3))
  expect_equal(evOf(SpaDES.core::completed(sim), "run")$eventTime, 1)
  expect_identical(nrow(evOf(SpaDES.core::events(sim), "run")), 0L)
  ## and the one prediction is there
  expect_equal(predVals(sim)[1], 0.19, tolerance = 1e-7)
})

test_that("no save event is scheduled when .saveInitialTime is NA (the default)", {
  sim <- toyRun(times = list(start = 1, end = 2))
  expect_identical(nrow(evOf(SpaDES.core::completed(sim), "save")), 0L)
  expect_identical(nrow(evOf(SpaDES.core::events(sim), "save")), 0L)
})

test_that("the save event says it does nothing, and leaves sim as it was", {
  ref <- toyRun(times = list(start = 1, end = 2))
  expect_message(
    sim <- toyRun(params = list(.saveInitialTime = 1), times = list(start = 1, end = 2)),
    "the save event does nothing", fixed = TRUE
  )
  expect_equal(evOf(SpaDES.core::completed(sim), "save")$eventTime, 1)
  ## same objects, same prediction, same queue: it is not rescheduled either
  expect_identical(sort(ls(sim)), sort(ls(ref)))
  expect_identical(predVals(sim), predVals(ref))
  expect_identical(as.data.frame(SpaDES.core::events(sim)), as.data.frame(SpaDES.core::events(ref)))
})

test_that("nothing is predicted before the first run event", {
  sim <- toyRun(params = list(.runInitialTime = 5), times = list(start = 1, end = 2))
  expect_null(sim$fireSense_SpreadPredicted)
  expect_equal(evOf(SpaDES.core::events(sim), "run")$eventTime, 5)
})

test_that("the prediction follows the covariates from year to year", {
  sim <- toySim(times = list(start = 1, end = 1))
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  expect_equal(predVals(sim)[4], 0.2491969, tolerance = 1e-6)
  ## next year's covariates: cell 4 becomes young, with no fuel and MDC 0 -> x = -0.5
  covs <- toyCovariates()
  for (cn in c("MDC", "fuelA")) setCov(covs, 4L, cn, 0)
  setCov(covs, 4L, "youngAge", 1)
  sim$fireSense_SpreadCovariates <- covs
  SpaDES.core::end(sim) <- 2
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  ## 0.13 + 0.12 / (1 + exp(1)) = 0.13 + 0.12 * 0.2689414 = 0.1622730
  expect_equal(predVals(sim)[4], 0.1622730, tolerance = 1e-6)
  expect_equal(predVals(sim)[1], 0.19, tolerance = 1e-7)
})
