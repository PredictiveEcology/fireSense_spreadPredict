defineModule(sim, list(
  name = "fireSense_spreadPredict",
  description = "Predicts a surface of fire spread probilities using a model fitted with fireSense_spreadFit.",
  keywords = c("fire spread", "fireSense", "predict"),
  authors = c(
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = "aut"),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut"),
    person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = "aut"),
    person("Alex M.", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(),
  version = list(fireSense_spreadPredict = "1.2.0.9002", SpaDES.core = "0.1.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_spreadPredict.Rmd"),
  reqdPkgs = list("data.table", "magrittr", "Matrix", "methods", "terra", "SpaDES.core (>=3.0.4)", "stats",
                  "ggplot2", "viridis",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9107)"),
  parameters = bindrows(
    defineParameter(name = "lowerSpreadProb", class = "numeric", default = fireSenseUtils::spreadProbFloor,
                    desc = paste("Lower asymptote of the 2- and 3-parameter logistic. Default",
                                 "`fireSenseUtils::spreadProbFloor`, the same constant `fireSense_spreadFit` fits with.")),
    defineParameter("ELFblendWidth", "numeric", default = 20000,
                    desc = paste("With several ELFs: each ELF's model also predicts this far (m) outside its own",
                                 "pixels, and where predictions overlap they are averaged with weights that fall",
                                 "linearly from 1 inside the ELF to 0 at this distance outside it. 50/50 at a",
                                 "boundary. The default is the buffer fireSenseUtils::makeELFs() puts around ELFs.")),
    defineParameter("maxFireSpread", "numeric", default = fireSenseUtils::spreadProbCeiling,
                    desc = paste("Upper limit on `spreadProb` used when fitting (default",
                                 "`fireSenseUtils::spreadProbCeiling`). Here it is only checked",
                                 "to be the same in every module that defines it.")),
    defineParameter(".rep", "integer", default = 1L, min = 1L, max = NA,
                    desc = paste("Which replicate this prediction is. It selects parameter set",
                                 "((rep - 1) %% number of sets) + 1 of each ELF, so replicates cycle through the",
                                 "fitted sets, each used whole (with its own `yearSpreadSD`), never averaged.")),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "Time of the first prediction."),
    defineParameter(name = ".runInterval", class = "numeric", default = 1,
                    desc = "Interval between predictions, in years. `NA` predicts once."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA,
                    desc = "Time of the `save` event, which does nothing. `NA` means never."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity and time are not relevant"))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "covMinMax_spread", objectClass = "data.table",
                 desc = paste("Minimum and maximum (2 rows) of each covariate in the fitting data,",
                              "used to rescale the covariates as in `fireSense_spreadFit`.")),
    expectsInput(objectName = "fireSense_SpreadCovariates", objectClass = "data.table",
                 desc = paste("This year's covariates, from `fireSense_dataPrepPredict`.",
                              "`pixelID` is the cell index of `flammableRTM`.")),
    expectsInput(objectName = "rasterToMatchLargeELF", objectClass = "SpatRaster", sourceURL = NA,
                 desc = paste("Only with several fitted ELFs: each pixel's ELF (`ELFind`), on the grid of",
                              "`flammableRTM`, from `fireSense_ELFs` with a `studyAreaLarge`.")),
    expectsInput(objectName = "flammableRTM", objectClass = "SpatRaster", sourceURL = NA,
                 desc = "Binary raster, 1 where the pixel is flammable. Template for `fireSense_SpreadPredicted`.")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "fireSense_SpreadPredicted", objectClass = "SpatRaster",
                  desc = "Spread probability of each flammable pixel, this year."),
    createsOutput(objectName = "fireSense_SpreadSD", objectClass = "SpatRaster|numeric",
                  desc = paste("The fitted sd of the per-year random effect on logit spread probability",
                               "(`yearSpreadSD` of the parameter set chosen by `.rep`; 0 if the fit has none), for",
                               "`fireSense_burn`. One number with one",
                               "fitted ELF; with several, a raster blended across ELFs with the weights of",
                               "`fireSense_SpreadPredicted`."))
  ))
)

#' Event dispatcher
#'
#' Events: `init`, `run` (predict, repeated every `.runInterval`), `save` (does nothing).
#'
#' @param sim A `simList`.
#' @param eventTime Time of the event.
#' @param eventType Name of the event.
#' @param debug Not used.
#'
#' @return The `simList`, invisibly.
doEvent.fireSense_spreadPredict <- function(sim, eventTime, eventType, debug = FALSE) {
  moduleName <- current(sim)$moduleName

  switch(
    eventType,
    init = {

      SpaDES.core::paramCheckOtherMods(sim, paramToCheck = "maxFireSpread", moduleToUse = "all")
      SpaDES.core::paramCheckOtherMods(sim, paramToCheck = "maxFireSpread", moduleToUse = "all")
      
      sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run",
                           eventPriority = 5.12)

      if (!is.na(P(sim)$.saveInitialTime)) {
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
      }
    },
    run = {
      sim <- spreadPredictRun(sim)

      if (!is.na(P(sim)$.runInterval)) {
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, moduleName, "run",
                             eventPriority = 5.12)
      }
    },
    save = {
      message("fireSense_spreadPredict: the save event does nothing")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'",
                  sep = ""
    ))
  )

  invisible(sim)
}

#' Predict this year's spread probability
#'
#' Rescales `sim$fireSense_SpreadCovariates` with the fit's covariate ranges, computes the spread
#' probability with ONE parameter set (row) of the fit, chosen by the `.rep` parameter (see
#' `paramSetForRep()`), and writes it to `sim$fireSense_SpreadPredicted`. Parameter sets are not averaged.
#'
#' With one fitted ELF (one row of `sim$studyAreaWithSpreadParams`) every pixel uses its parameters and
#' its `covMinMax_spread` (`ledgerCovMinMax()`; `sim$covMinMax_spread` only when the row has none). With several, each ELF's model (its parameter set, its `covMinMax_spread`, and
#' only the covariates it was fitted with) predicts its own pixels, from `sim$rasterToMatchLargeELF`, and
#' those within `ELFblendWidth` of them; overlapping predictions are averaged with the weights of
#' `ELFblendWeights()`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
spreadPredictRun <- function(sim) {
  covs <- copy(sim$fireSense_SpreadCovariates)
  sa <- sim$studyAreaWithSpreadParams

  # Without fitted parameters there is nothing to predict from; say so instead of
  # dying in rowMeans() on an empty matrix (which is what an unfitted ELF produced
  # when fireSense_spreadFit had not run first). This must come before anything
  # indexes `params[[1]]`: with zero rows that fails first, "subscript out of bounds".
  nPar <- tryCatch(NROW(sa$params[[1]]), error = function(e) 0L)
  if (NROW(sa) == 0L || is.null(nPar) || nPar == 0L)
    stop("fireSense_spreadPredict: sim$studyAreaWithSpreadParams holds no fitted spread ",
         "parameters for this run (", if (!is.null(sim$.runName)) sim$.runName else "unknown",
         "). Either fireSense_spreadFit has not run yet -- its `run` event must precede this ",
         "module's -- or the shared ledger has no row for this polygon.", call. = FALSE)

  if (NROW(sa) == 1L) {
    params1 <- paramSetForRep(sa$params[[1]], P(sim)$.rep)
    gamma1 <- ledgerPowerLawGamma(sa, 1L)
    warnPowerLawYearEffect(gamma1, params1)
    pred <- spreadProbOneELF(covs, params = params1, covMinMax = ledgerCovMinMax(sa, 1L, sim$covMinMax_spread),
                             yr = time(sim),
                             maxFireSpread = P(sim)$maxFireSpread, lowerSpreadProb = P(sim)$lowerSpreadProb,
                             covCentre = ledgerCovCentre(sa, 1L), climateRef = ledgerClimateRef(sa, 1L),
                             powerLawGamma = gamma1)
    sim$fireSense_SpreadSD <- yearSpreadSDOf(params1)
  } else {
    ids <- as.character(sa[[fireSenseUtils::polygonIDTxt]])
    ## each ELF's weight at each pixel: static, so computed once
    if (is.null(mod$ELFweights) || !identical(attr(mod$ELFweights, "key"), list(ids, covs$pixelID)))
      mod$ELFweights <- ELFblendWeights(sim$rasterToMatchLargeELF, sim$flammableRTM, covs$pixelID, ids,
                                        width = P(sim)$ELFblendWidth)
    w <- mod$ELFweights
    none <- rowSums(w) == 0
    if (any(none))
      warning("fireSense_spreadPredict: ", sum(none), " flammable pixels are more than ",
              P(sim)$ELFblendWidth, " m from every fitted ELF; they get no spread probability", call. = FALSE)
    acc <- numeric(NROW(covs)); wsum <- numeric(NROW(covs)); accSD <- numeric(NROW(covs))
    for (i in seq_along(ids)) {
      these <- which(w[, i] > 0)
      if (!length(these)) next
      parsI <- paramSetForRep(sa$params[[i]], P(sim)$.rep)
      gammaI <- ledgerPowerLawGamma(sa, i)
      warnPowerLawYearEffect(gammaI, parsI)
      p <- spreadProbOneELF(covs[these], params = parsI, covMinMax = sa$covMinMax_spread[[i]],
                            yr = time(sim), maxFireSpread = P(sim)$maxFireSpread,
                            lowerSpreadProb = P(sim)$lowerSpreadProb,
                            covCentre = ledgerCovCentre(sa, i), climateRef = ledgerClimateRef(sa, i),
                            powerLawGamma = gammaI)
      rows <- these[match(p$pixelID, covs$pixelID[these])]
      acc[rows] <- acc[rows] + w[rows, i] * p$spreadProb
      accSD[rows] <- accSD[rows] + w[rows, i] * yearSpreadSDOf(parsI)
      wsum[rows] <- wsum[rows] + w[rows, i]
    }
    ok <- wsum > 0
    pred <- data.table(pixelID = covs$pixelID[ok], spreadProb = acc[ok] / wsum[ok])
    ## each ELF's year effect sd, blended like the probabilities: fireSense_burn scales one z per year by it
    sim$fireSense_SpreadSD <- rast(sim$flammableRTM)
    sim$fireSense_SpreadSD[covs$pixelID[ok]] <- accSD[ok] / wsum[ok]
  }

  # Return to raster format
  sim$fireSense_SpreadPredicted <- rast(sim$flammableRTM) ## use flammableRTM as template
  sim$fireSense_SpreadPredicted[pred$pixelID] <- pred$spreadProb

  invisible(sim)
}

#' Each ELF's weight at each pixel
#'
#' An ELF's raw weight is 1 on its own pixels and falls linearly to 0 at `width` metres from them (distance
#' to the nearest pixel of the ELF). Weights are then normalised to sum to 1 at each pixel, so a pixel on a
#' boundary between two ELFs is 50/50 and one `width` inside an ELF uses that ELF only.
#'
#' @param elfRas `sim$rasterToMatchLargeELF`: `ELFind` per pixel, as a categorical raster (or the id).
#' @param template `sim$flammableRTM`; `pixelID` indexes its cells, so `elfRas` must share its grid.
#' @param pixelID integer; the cells to weight.
#' @param ids character; the ELFs, in the order of the rows of `studyAreaWithSpreadParams`.
#' @param width numeric; metres.
#' @return matrix, one row per `pixelID`, one column per ELF (`ids`), rows summing to 1 (or 0 where no
#'   ELF is within `width`). `attr(, "key")` records `ids` and `pixelID`.
ELFblendWeights <- function(elfRas, template, pixelID, ids, width) {
  if (is.null(elfRas))
    stop("fireSense_spreadPredict: sim$studyAreaWithSpreadParams has several ELFs, so each pixel's ELF ",
         "must come from sim$rasterToMatchLargeELF (fireSense_ELFs, with a studyAreaLarge); it is missing",
         call. = FALSE)
  if (!isTRUE(terra::compareGeom(elfRas, template, stopOnError = FALSE)))
    stop("fireSense_spreadPredict: sim$rasterToMatchLargeELF is not on the grid of sim$flammableRTM",
         call. = FALSE)
  r <- elfRas[[1]]
  v <- terra::values(r, mat = FALSE)
  lv <- terra::levels(r)[[1]]
  lab <- if (is.data.frame(lv) && NCOL(lv) >= 2) as.character(lv[[2]][match(v, lv[[1]])]) else as.character(v)
  w <- vapply(ids, function(id) {
    core <- r; terra::values(core) <- ifelse(lab %in% id, 1, NA)
    if (all(is.na(terra::values(core, mat = FALSE)))) return(numeric(length(pixelID)))
    d <- terra::values(terra::distance(core), mat = FALSE)[pixelID]
    pmax(0, 1 - d / width)
  }, numeric(length(pixelID)))
  w <- matrix(w, ncol = length(ids), dimnames = list(NULL, ids))
  tot <- rowSums(w)
  w[tot > 0, ] <- w[tot > 0, , drop = FALSE] / tot[tot > 0]
  attr(w, "key") <- list(ids, pixelID)
  w
}

#' Spread probability for the pixels of one ELF
#'
#' @param covs `data.table` of this ELF's pixels: `pixelID` and covariates (as `fireSense_dataPrepPredict`
#'   makes them; fuel biomass logged). It can hold more than the fit used (every ELF's covariates, or a term the
#'   spread formula has but the fit does not); only the covariates named in `params` are used.
#' @param params one-row `data.frame`: the ELF's parameter set for this replicate (`paramSetForRep()`).
#' @param covMinMax the ELF's `covMinMax_spread` (`ledgerCovMinMax()`): the ranges the fit rescaled its
#'   covariates with. Every fitted covariate must be a column of it, or it is left unscaled.
#' @param yr,maxFireSpread,lowerSpreadProb as for `fireSenseUtils::spreadProbFromIntegerCovs()` and
#'   `fireSenseUtils::logisticAll()`.
#' @param covCentre the ELF's `covCentre_spread` (`ledgerCovCentre()`): the covariate means the fit subtracted
#'   from the rescaled covariates; `NULL` (a fit without an intercept, or an older ledger row) centres nothing.
#'   A parameter set with `fireSenseUtils::spreadInterceptTxt` has an intercept, added as a column of 1s after
#'   the covariates are rescaled and centred, as in the fit.
#' @param climateRef the ELF's `climateRef_spread` (`ledgerClimateRef()`): with a fit made with climate outside the
#'   `maxAsymptote` ceiling (`fireSense_spreadFit`'s `spreadLink = "climateOutside"`), the reference value of each
#'   climate covariate; `fireSenseUtils::spreadProbFromCovs()` then applies that link, exactly as the fit did.
#'   `NULL` (a fit with climate under the ceiling, or an older ledger row) is the link as it always was.
#' @param powerLawGamma the ELF's `powerLawGamma_spread` (`ledgerPowerLawGamma()`): with a fit made with
#'   `fireSense_spreadFit`'s `spreadLink = "powerLaw"`, its `gamma`; `fireSenseUtils::spreadProbFromCovs()` then
#'   applies that link (`fireSenseUtils::powerLawLink()`, with `maxAsymptote` as `p_c`), exactly as the fit did.
#'   `NULL` (any other link) is the link as it always was.
#' @return `data.table` with `pixelID` and `spreadProb`, from the one parameter set in `params`.
spreadProbOneELF <- function(covs, params, covMinMax, yr, maxFireSpread, lowerSpreadProb,
                             covCentre = NULL, climateRef = NULL, powerLawGamma = NULL) {
  moduleName <- "fireSense_spreadPredict"
  covs <- copy(covs)
  ## the per-year random effect is not a covariate coefficient: fireSense_burn applies it (fireSense_SpreadSD)
  if (yearSpreadSDTxt %in% names(params)) {
    params <- as.data.frame(params)
    params[[yearSpreadSDTxt]] <- NULL
  }

  ## Fuel biomass arrives logged (fireSenseUtils::logMinB()). A fit made on LINEAR fuel biomass has
  ## fireSenseUtils::fuelLinearRange, c(0, 1e4), as that covariate's covMinMax_spread, and its
  ## coefficients only mean anything for biomass / 1e4: undo the log with the function the fit used.
  ## A fit made on the log scale has the log range there, and its covariates are left as they are.
  fuelCols <- character()
  for (cn in intersect(names(covMinMax), names(covs))) {
    if (fireSenseUtils::isLinearFuelRange(covMinMax[[cn]])) {
      covs[[cn]] <- fireSenseUtils::fuelLogToLinear(covs[[cn]])
      fuelCols <- c(fuelCols, cn)
    }
  }

  ## every fitted coefficient needs a covariate: without one the logistic gets no covariates and
  ## fails later in rowMeans(), with no hint of the cause
  ## the intercept, if the fit has one, is a coefficient with no covariate: it multiplies a column of 1s
  hasIntercept <- fireSenseUtils::spreadInterceptTxt %in% names(params)
  fitted <- setdiff(names(params), c(unlist(fireSenseUtils::logisticParamNames), fireSenseUtils::spreadInterceptTxt))
  noCov <- setdiff(fitted, names(covs))
  if (length(noCov)) {
    stop(
      moduleName, "> the fitted coefficients ", paste(noCov, collapse = ", "),
      " have no covariate. The covariates available are ",
      paste(setdiff(names(covs), "pixelID"), collapse = ", "),
      ". The non-forest groups / fuel classes used to build the covariates differ from the fit's."
    )
  }

  ## the covariates this ELF was fitted with, not the spread formula's terms: they can differ (a hindcast
  ## rebuilds the formula from another year's land cover), and `covs` can hold more than this ELF's
  covs <- covs[, c("pixelID", fitted), with = FALSE]
  ## spreadProbFromIntegerCovs() rescales only the columns of covMinMax: a fitted covariate it lacks would
  ## silently stay on its raw scale (x1000)
  noRange <- setdiff(fitted, names(covMinMax))
  if (length(noRange))
    stop(moduleName, "> covMinMax_spread has no range for the fitted covariate(s) ",
         paste(noRange, collapse = ", "), "; it has ", paste(names(covMinMax), collapse = ", "),
         ". The ranges must be the fit's (its ledger row).")

  # integers x 1000, the form `spreadProbFromIntegerCovs` expects
  shortAnnDTx1000 <- toX1000(list(covs))[[1]] |> setDT()
  colsToUse <- setdiff(names(covs), "pixelID")
  ## what the coefficients multiply: the intercept first, as in the fit (fireSenseUtils::spreadDesignCols())
  designCols <- c(if (hasIntercept) fireSenseUtils::spreadInterceptTxt, colsToUse)

  ## youngAge is mutually exclusive with every other non-climate covariate, exactly as in the fit
  ## (fireSense_spreadFit::spreadFitPrep()): wherever youngAge is non-zero, fuel biomass, nfLCC_*
  ## and treedWetland are zero. fireSenseUtils::fireSenseCovariatesCreate() already applies this
  ## when the covariates are built, but that must not be this module's only defence -- it is
  ## re-applied here so a young pixel's covariates are correct regardless of how they arrived.
  mutuallyExclusive <- if (fireSenseUtils::youngAgeTxt %in% colsToUse) {
    fireSenseUtils::youngAgeExclusiveCols(colsToUse, fuelCols = fuelCols)
  } else {
    NULL
  }

  shortAnnDT <-
    spreadProbFromIntegerCovs(shortAnnDTx1000 = shortAnnDTx1000,
                              yr = yr,
                              covMinMax = covMinMax,
                              mutuallyExclusive = mutuallyExclusive,
                              colsToUse = designCols,
                              doAssertions = FALSE,
                              logisticPars = params,
                              maxFireSpread = maxFireSpread,
                              covCentre = covCentre
                              )

  mat <- as.matrix(shortAnnDT[, ..designCols])

  par <- params[1L, ] |> as.vector() |> unlist()
  covPars <- par[intersect(names(par), designCols)]
  logisticPars <- par[setdiff(names(par), names(covPars))]
  # Make sure the order is correct in the matrix
  matching <- intersect(names(covPars), colnames(mat))

  ## the one function the fit computes its spread probability with, so the prediction cannot differ from it
  data.table(pixelID = shortAnnDT$pixelID,
             spreadProb = spreadProbFromCovs(mat[, matching, drop = FALSE], logisticPars, covPars, lowerSpreadProb,
                                             link = if (!is.null(powerLawGamma)) "powerLaw",
                                             climateRef = climateRef, powerLawGamma = powerLawGamma))
}

yearSpreadSDTxt <- "yearSpreadSD"

#' Warn, once, that `fireSense_burn` adds the year effect on the wrong scale for a power-law fit
#'
#' A fit made with `spreadLink = "powerLaw"` adds its year effect (`yearSpreadSD`) to the linear predictor, not to
#' the logit of `p`; `fireSense_burn` adds `fireSense_SpreadSD` on the logit, so with a power-law fit and a
#' `yearSpreadSD` above 0 the simulated years are not those of the fit.
#'
#' @param powerLawGamma the row's `gamma` (`ledgerPowerLawGamma()`); `NULL` for another link.
#' @param params one-row `data.frame`: the parameter set in use (`paramSetForRep()`).
#' @return `NULL`, invisibly.
warnPowerLawYearEffect <- function(powerLawGamma, params) {
  if (!is.null(powerLawGamma) && yearSpreadSDOf(params) > 0 && !isTRUE(mod$warnedPowerLawYearEffect)) {
    mod$warnedPowerLawYearEffect <- TRUE
    warning("fireSense_spreadPredict: this ledger row is a spreadLink = 'powerLaw' fit with yearSpreadSD > 0; ",
            "the fit adds the year effect to the linear predictor, but fireSense_burn adds fireSense_SpreadSD on the ",
            "logit of the spread probability, so simulated years will differ from the fit's.", call. = FALSE)
  }
  invisible(NULL)
}

#' The covariate ranges of one ledger row
#'
#' A fit rescales its covariates with fixed ranges and stores them in its ledger row (`covMinMax_spread`).
#' Prediction rescales with the same values, so it never depends on `sim$covMinMax_spread`, which
#' `fireSense_spreadFit` rebuilds in every run from that run's own covariate tables (in a hindcast, from a
#' formula rebuilt for another year, which can lack a fitted term).
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @param default what to return for a row without ranges (an older ledger row): `sim$covMinMax_spread`.
#' @return The row's `covMinMax_spread` (a `data.table`, 2 rows: min, max), else `default`.
ledgerCovMinMax <- function(sa, i, default = NULL) {
  if (!"covMinMax_spread" %in% names(sa)) return(default)
  cmm <- sa[["covMinMax_spread"]][[i]]
  if (is.null(cmm) || !NROW(cmm)) return(default)
  cmm
}

#' The climate reference of one ledger row
#'
#' A fit made with climate outside the `maxAsymptote` ceiling (`fireSense_spreadFit`'s `spreadLink = "climateOutside"`)
#' stores the reference value of each climate covariate in the ledger row (`fireSenseUtils::spreadFitClimateRefTxt`),
#' with `spreadLink` beside it. Prediction applies the same link with the same reference
#' (`fireSenseUtils::spreadProbFromCovs()`).
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @return A named list, or `NULL` for a row without a reference: a fit with climate under the ceiling, or a row
#'   written before the link existed. Such a row predicts exactly as it always did.
ledgerClimateRef <- function(sa, i) {
  col <- fireSenseUtils::spreadFitClimateRefTxt
  if (!col %in% names(sa)) return(NULL)
  ref <- sa[[col]][[i]]
  if (is.list(ref) && length(ref)) ref else NULL
}

#' The power-law exponent of one ledger row
#'
#' A fit made with `fireSense_spreadFit`'s `spreadLink = "powerLaw"` stores its `gamma` in the ledger row
#' (`fireSenseUtils::spreadFitPowerLawGammaTxt`), with `"powerLaw"` in `fireSenseUtils::spreadLinkTxt`. Prediction
#' applies the same link with the same `gamma` (`fireSenseUtils::spreadProbFromCovs()`).
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @return `gamma`, or `NULL` for a row of any other link, such as every row written before the link existed. Such
#'   a row predicts exactly as it always did. A `"powerLaw"` row without a valid `gamma` stops.
ledgerPowerLawGamma <- function(sa, i) {
  linkCol <- fireSenseUtils::spreadLinkTxt
  if (!linkCol %in% names(sa) || !identical(as.character(sa[[linkCol]][[i]]), "powerLaw")) return(NULL)
  gamma <- sa[[fireSenseUtils::spreadFitPowerLawGammaTxt]][[i]]
  if (!(is.numeric(gamma) && length(gamma) == 1L && is.finite(gamma) && gamma > 0))
    stop("fireSense_spreadPredict: ledger row ", i, " has spreadLink 'powerLaw' but no valid '",
         fireSenseUtils::spreadFitPowerLawGammaTxt, "' (", format(gamma), ")", call. = FALSE)
  gamma
}

#' The covariate centres of one ledger row
#'
#' A fit with an intercept centres its covariates and stores the centres in the ledger row
#' (`fireSenseUtils::spreadFitCovCentreTxt`). Prediction subtracts the same values.
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @return A named list, or `NULL` for a row without centres: a fit without an intercept, or a row written
#'   before they were stored. Such a row predicts exactly as it always did.
ledgerCovCentre <- function(sa, i) {
  col <- fireSenseUtils::spreadFitCovCentreTxt
  if (!col %in% names(sa)) return(NULL)
  centre <- sa[[col]][[i]]
  if (is.list(centre) && length(centre)) centre else NULL
}

#' The parameter set a replicate uses
#'
#' Replicate `rep` uses row `((rep - 1) %% NROW(params)) + 1` of an ELF's fitted parameter sets, whole: its
#' coefficients and its `yearSpreadSD` stay together. Each ELF takes the modulo of its own number of rows.
#'
#' @param params `data.frame` of an ELF's fitted parameter sets, one per row (ledger `params`).
#' @param rep integer; the replicate (the module parameter `.rep`).
#' @return One-row `data.frame`.
paramSetForRep <- function(params, rep) {
  params[((rep - 1L) %% NROW(params)) + 1L, , drop = FALSE]
}

#' The fitted sd of the per-year random effect
#'
#' `yearSpreadSD` (fireSense_spreadFit, fireSenseUtils >= 0.2.3.9041) is one eps per year on logit spread
#' probability.
#'
#' @param params one-row `data.frame`: the parameter set in use (`paramSetForRep()`).
#' @return Numeric; 0 when the fit has no `yearSpreadSD`.
yearSpreadSDOf <- function(params) {
  if (!yearSpreadSDTxt %in% names(params)) return(0)
  params[[yearSpreadSDTxt]]
}
