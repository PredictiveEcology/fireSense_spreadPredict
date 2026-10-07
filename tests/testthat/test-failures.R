## Failure paths of the run event, with the message each one gives.

test_that("a formula term that is not a covariate column stops, naming the term", {
  ins <- toyInputs(formula = "~ MDC + youngAge + fuelA + fuelZ - 1")
  expect_error(toyRun(ins), "'fuelZ' not found in data objects")
})

test_that("several missing terms are counted in the message", {
  ins <- toyInputs(formula = "~ MDC + fuelY + fuelZ + fuelW - 1")
  expect_error(toyRun(ins), "'fuelY' \\(and 2 others\\) not found in data objects")
  ins <- toyInputs(formula = "~ MDC + fuelY + fuelZ - 1")
  expect_error(toyRun(ins), "'fuelY' \\(and 1 other\\) not found in data objects")
})

test_that("a response on the left of the formula is ignored", {
  ins <- toyInputs(formula = "fires ~ MDC + youngAge + fuelA - 1")
  expect_equal(predVals(toyRun(ins))[1], 0.19, tolerance = 1e-7)
})

test_that("no fitted parameters stops with an explanation that names the run", {
  ins <- toyInputs()
  ins$studyAreaWithSpreadParams <- NULL
  expect_error(toyRun(ins), "holds no fitted spread parameters for this run \\(unknown\\)")

  ins <- toyInputs()
  ins$studyAreaWithSpreadParams$params <- list(toyParams()[0, ])
  ins$.runName <- "toyRunName"
  expect_error(toyRun(ins), "holds no fitted spread parameters for this run \\(toyRunName\\)")
})

test_that("a missing spread formula stops", {
  ins <- toyInputs()
  ins$fireSense_spreadFormula <- NULL
  expect_error(toyRun(ins), "argument is not a valid model")
})

test_that("a fitted coefficient with no covariate stops at once, naming it and the covariates available", {
  ## a fit made with non-forest groups nfLCC_100_60 and nfLCC_40_50_80; the covariates were built
  ## with a single group `nf`, so neither coefficient has a column
  ins <- toyInputs(params = toyParams(nfLCC_100_60 = 1, nfLCC_40_50_80 = 1))
  expect_error(toyRun(ins), "nfLCC_100_60, nfLCC_40_50_80")
  expect_error(toyRun(ins), "MDC, youngAge, fuelA")
  expect_error(toyRun(ins), "non-forest groups")
  expect_error(toyRun(ins), "differ from the fit's")
  ## and not the late, unexplained failure
  err <- tryCatch(toyRun(ins), error = conditionMessage)
  expect_false(grepl("at least two dimensions", err))
})
