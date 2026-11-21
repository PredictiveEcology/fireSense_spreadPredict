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
  version = list(fireSense_spreadPredict = "1.2.0.9018", SpaDES.core = "0.1.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_spreadPredict.Rmd"),
  reqdPkgs = list("data.table", "magrittr", "Matrix", "methods", "terra", "SpaDES.core (>=3.0.4)", "stats",
                  "ggplot2", "viridis",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9186)"), # 9186: splitSpreadClimate(), climateSplitRefPixels(), elfCellLabels() (fireSenseUtils#244); 9183: spreadLinkInputs(activationIntercept =), spreadClimateTxt (fireSenseUtils#255); 9181: spreadLinkInputs(), powerLawPc50 (fireSenseUtils#250); 9165: lowerAsymptote minimum (#230); 9162: smooth links, checkLinkParams() (#229); 9138: powerLawShape
  parameters = bindrows(
    defineParameter(name = "lowerSpreadProb", class = "numeric", default = fireSenseUtils::spreadProbFloor,
                    desc = paste("Lower asymptote of the 2- and 3-parameter logistic. Default",
                                 "`fireSenseUtils::spreadProbFloor`, the same constant `fireSense_spreadFit` fits with.")),
    defineParameter("ELFblendWidth", "numeric", default = 20000,
                    desc = paste("With several ELFs: each ELF's model also predicts this far (m) outside its own",
                                 "pixels, and where predictions overlap they are averaged with weights that fall",
                                 "linearly from 1 inside the ELF to 0 at this distance outside it. 50/50 at a",
                                 "boundary. The default is the buffer fireSenseUtils::makeELFs() puts around ELFs.")),
    defineParameter("escapeSizeHa", "numeric", default = fireSenseUtils::fireSenseEscapeSizeHa,
                    desc = paste("Size (ha) at and above which a fire counts as an escape. The ledger rows must have been",
                                 "fitted with this definition, else the module stops",
                                 "(`fireSenseUtils::checkEscapeDefinition()`). Checked to be the same in every module that",
                                 "defines it.")),
    defineParameter("fireCauses", "character", default = fireSenseUtils::fireSenseFireCauses,
                    desc = paste("Cause groups of the fires that count as escapes (`fireSenseUtils::fireCauseCodes()`);",
                                 "part of the escape definition, checked like `escapeSizeHa`.")),
    defineParameter("maxFireSpread", "numeric", default = fireSenseUtils::spreadProbCeiling,
                    desc = paste("Upper limit on `spreadProb` used when fitting (default",
                                 "`fireSenseUtils::spreadProbCeiling`). Here it is only checked",
                                 "to be the same in every module that defines it.")),
    defineParameter("paramSets", "character", default = NA, min = NA, max = NA,
                    desc = paste("Path to an `.rds` or `.csv` of spread parameter vectors, one per row, for the simulated ELF",
                                 "(`sim$.ELFind`; the only row with one ELF): e.g. a DEoptim population, or the `params` of",
                                 "the rows of another ledger file (a table with a `params` list column, its rows",
                                 "for this ELF). They replace the simulated ELF's rows of the ledger; neighbour ELFs keep",
                                 "theirs, so blending is unchanged. The link, lower asymptote, `gamma` and covariate",
                                 "centres still come from the ledger row. A set must have every coefficient the ledger",
                                 "row has, else the module stops. `NA` uses the ledger rows. Replicate r uses set",
                                 "`ceiling(r / repsPerSet)` (cycling through the sets), and calls `set.seed(k)`",
                                 "with k the replicate's number within its set, so every set gets the same random numbers",
                                 "in `fireSense_burn` (common random numbers).")),
    defineParameter("repsPerSet", "integer", default = 1L, min = 1L, max = NA,
                    desc = "With `paramSets`: the number of replicates that use each set. Not used otherwise."),
    defineParameter(".rep", "integer", default = 1L, min = 1L, max = NA,
                    desc = paste("Which replicate this prediction is. It selects parameter set",
                                 "((rep - 1) %% number of sets) + 1 of each ELF, so replicates cycle through the",
                                 "fitted sets, each used whole (with its own `yearSpreadSD`), never averaged.",
                                 "Ignored if `paramSetByReplicate` is `TRUE`.")),
    defineParameter("paramSetByReplicate", "logical", default = FALSE, min = NA, max = NA,
                    desc = paste("If `TRUE`, the replicate is the `run` event within the job, not `.rep`:",
                                 "round((time - .runInitialTime) / .runInterval) + 1 (as `fireSense_summary` numbers",
                                 "hindcast replicates), so one job cycles through all the fitted parameter sets.",
                                 "With `FALSE` the set follows `.rep`.")),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "Time of the first prediction."),
    defineParameter(name = ".runInterval", class = "numeric", default = 1,
                    desc = "Interval between predictions, in years. `NA` predicts once."),
    defineParameter(".plots", "character", "screen", NA, NA,
                    desc = paste("Passed to `types` in `Plots()`, e.g. \"screen\" (default), \"png\": the spread probability",
                                 "map is plotted each `run`. `NA` or `NULL`: no plots.")),
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
                              "`flammableRTM`, from `fireSense_ELFs` with a `studyArea_fireSense`.")),
    expectsInput(objectName = ".ELFind", objectClass = "character", sourceURL = NA,
                 desc = paste("The simulated ELF, as in `fireSense_dataPrepPredict`. Only used with `paramSets` and",
                              "several fitted ELFs, to find the simulated ELF's row.")),
    expectsInput(objectName = "flammableRTM", objectClass = "SpatRaster", sourceURL = NA,
                 desc = "Binary raster, 1 where the pixel is flammable. Template for `fireSense_SpreadPredicted`.")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "fireSense_SpreadPredicted", objectClass = "SpatRaster",
                  desc = "Spread probability of each flammable pixel, this year."),
    createsOutput(objectName = "fireSense_SpreadParamSet", objectClass = "integer",
                  desc = paste("Row of the fitted parameter sets used by this prediction, named by ELF with several",
                               "ELFs, for `fireSense_summary`.")),
    createsOutput(objectName = "fireSense_SpreadSD", objectClass = "SpatRaster|numeric",
                  desc = paste("The fitted sd of the per-year random effect on logit spread probability",
                               "(`yearSpreadSD` of the parameter set chosen by `.rep`; 0 if the fit has none), for",
                               "`fireSense_burn`. One number with one",
                               "fitted ELF; with several, a raster blended across ELFs with the weights of",
                               "`fireSense_SpreadPredicted`.")),
    createsOutput(objectName = "fireSense_SpreadFireSD", objectClass = "SpatRaster|numeric",
                  desc = paste("The fitted sd of the per-fire random effect on spread probability (`fireSpreadSD` of the",
                               "parameter set chosen by `.rep`; 0 for a ledger row without it), for `fireSense_burn`.",
                               "Shaped like `fireSense_SpreadSD`: one number with one fitted ELF; with several, a raster",
                               "of each ELF's value blended with the weights of `fireSense_SpreadPredicted`.")),
    createsOutput(objectName = "fireSense_EscapeDefinition", objectClass = "data.frame",
                  desc = paste("The escape definition the ledger rows were fitted with (`escapeSizeHa`, `fireCauses`;",
                               "`fireSenseUtils::escapeDefinition()`), one row per fitted ELF (with its `polygonID`);",
                               "`NA` for a row fitted before the definition was recorded. Each row has been checked",
                               "against this run's `escapeSizeHa` and `fireCauses`.")),
    createsOutput(objectName = "fireSense_SpreadLink", objectClass = "character",
                  desc = paste("Only for a fit made with `spreadLink = \"powerLaw\"`, `\"powerLawSmooth\"` or `\"richards\"` (else",
                               "NULL): the link the ledger row records, for `fireSense_burn`. The names of",
                               "`fireSense_SpreadPowerLaw` are those of this link (`fireSenseUtils::spreadLinkParNames`). Fitted ELFs",
                               "with different such links cannot be blended: an error.")),
    createsOutput(objectName = "fireSense_SpreadPowerLaw", objectClass = "SpatRaster|numeric",
                  desc = paste("Only for a fit made with `spreadLink = \"powerLaw\"`, `\"powerLawSmooth\"` or `\"richards\"` (else NULL):",
                               "the parameters of its link, named as `fireSenseUtils::spreadLinkParNames` says for",
                               "`fireSense_SpreadLink`, for `fireSense_burn`, which adds the year effect to the",
                               "linear predictor, as the fit does. \"powerLaw\": `pc` (the row's",
                               "`maxAsymptote`), `gamma`, `floor` (`lowerSpreadProb`) and `shape` of",
                               "`fireSenseUtils::powerLawLink()`; \"powerLawSmooth\" also `lower` (the row's",
                               "`lowerAsymptote`, at least `fireSenseUtils::spreadLowerAsymptoteMin`, 0.001: `fireSenseUtils::checkLinkParams()`); \"richards\": `pc`, `lower`, `nu` (the row's `richardsNu`) and `B`.",
                               "A numeric vector with one fitted ELF; with several, a raster with one layer per name,",
                               "blended like `fireSense_SpreadSD`. With a fit made with climate in the ceiling",
                               "(`fireSense_spreadFit`'s `spreadClimate = \"ceiling\"`: a ledger row with `climateRef_spread` and a",
                               "power-law link), `pc` differs by pixel and year, so it is a raster even with one ELF.",
                               "So with `spreadClimate = \"activation\"` (the row's `fireSenseUtils::spreadClimateTxt`), where fuel sets",
                               "each pixel's `pc`.")),
    createsOutput(objectName = "fireSense_SpreadLinear", objectClass = "SpatRaster",
                  desc = paste("Only for a fit made with a link of `fireSenseUtils::spreadLinearLinks` (else NULL): the linear predictor x of",
                               "each flammable pixel (covariates %*% coefficients), the step before",
                               "the link (`fireSenseUtils::linearLinkProb()`) turns it into `fireSense_SpreadPredicted`; `NA` where that is.",
                               "With `\"powerLaw\"` a pixel with a negative x has a spread probability of 0, so `fireSense_burn` needs x itself to",
                               "add the year effect there. With several ELFs, blended like `fireSense_SpreadPowerLaw`."))
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

      SpaDES.core::paramCheckOtherMods(sim, paramToCheck = "maxFireSpread", moduleToUse = "all", ifSetButDifferent = "error")
      fireSenseUtils::escapeDefinitionOf(sim)
      
      sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run",
                           eventPriority = 5.12)

      if (!is.na(P(sim)$.saveInitialTime)) {
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
      }
    },
    run = {
      sim <- spreadPredictRun(sim)
      ## the year's spread probability
      if (anyPlotting(P(sim)$.plots))
        Plots(sim$fireSense_SpreadPredicted, types = P(sim)$.plots,
              filename = paste0("SpreadProbability_yr", time(sim)),
              deviceArgs = list(width = 10, height = 8, units = "in", res = 144))  # png() alone is 480 x 480 px

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

  sim$fireSense_EscapeDefinition <- ledgerEscapeDefinition(sa)
  fireSenseUtils::checkEscapeDefinition(sim$fireSense_EscapeDefinition, fireSenseUtils::escapeDefinitionOf(sim),
                                        "the ledger row")

  rep <- if (isTRUE(P(sim)$paramSetByReplicate))
    replicateOf(time(sim), P(sim)$.runInitialTime, P(sim)$.runInterval) else P(sim)$.rep
  sim$fireSense_SpreadParamSet <- stats::setNames(vapply(sa$params, function(p) paramSetIndex(NROW(p), rep), integer(1)),
                                                  if (NROW(sa) > 1L) as.character(sa[[fireSenseUtils::polygonIDTxt]]))

  ## paramSets: the simulated ELF's rows become the chosen set; the surface only changes when the set does
  if (!is.na(P(sim)$paramSets)) {
    ps <- paramSetsForRep(sim, sa, rep)
    sa$params[[ps$own]] <- ps$params
    sim$fireSense_SpreadParamSet[[ps$own]] <- ps$set
    ## the same random numbers for every set (not in the seed), but not for every year, for fireSense_burn
    set.seed(paramSetsSeed(P(sim)$.runInitialTime, ps$k, ps$id))
    ## everything the outputs depend on; `time(sim)` is not in it: with one covariate table it does not change them
    key <- list(P(sim)$paramSets, ps$set, sim$fireSense_SpreadParamSet, covs, sa[setdiff(names(sa), "params")],
                ps$params, rasterVersion("flammableRTM", sim$flammableRTM),
                rasterVersion("rasterToMatchLargeELF", sim$rasterToMatchLargeELF),
                P(sim)$ELFblendWidth, P(sim)$lowerSpreadProb, P(sim)$maxFireSpread)
    if (identical(mod$surface$key, key) &&
        identical(mod$surface$samples, lapply(mod$surface$outputs, rasterSample))) {   ## unedited since stored
      for (nm in surfaceOutputs) sim[[nm]] <- mod$surface$outputs[[nm]]
      return(invisible(sim))
    }
  }

  if (NROW(sa) == 1L) {
    params1 <- paramSetForRep(sa$params[[1]], rep)
    lk1 <- ledgerLinkArgs(sa, 1L, floor = P(sim)$lowerSpreadProb)
    pred <- do.call(spreadProbOneELF, c(list(covs, params = params1, flammableRTM = sim$flammableRTM, covMinMax = ledgerCovMinMax(sa, 1L, sim$covMinMax_spread),
                             yr = time(sim),
                             maxFireSpread = P(sim)$maxFireSpread, lowerSpreadProb = P(sim)$lowerSpreadProb,
                             covCentre = ledgerCovCentre(sa, 1L), climateRef = ledgerClimateRef(sa, 1L),
                             powerLawPc50 = ledgerPowerLawPc50(sa, 1L)), lk1))
    for (nm in names(randomEffectOutputs)) sim[[nm]] <- randomEffectSDOf(params1, randomEffectOutputs[[nm]])
    sim$fireSense_SpreadLink <- lk1$link
    ## the link's parameters: a vector, or with climate in the ceiling (`spreadClimate = "ceiling"`) one row per pixel
    linkPars <- attr(pred, "linkPars")
    sim$fireSense_SpreadPowerLaw <- if (is.matrix(linkPars)) linkParsRas(sim$flammableRTM, pred$pixelID, linkPars) else linkPars
    if (!is.null(lk1$link)) sim$fireSense_SpreadLinear <- linearPredictorRas(sim$flammableRTM, pred)
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
    acc <- numeric(NROW(covs)); wsum <- numeric(NROW(covs)); accSD <- matrix(0, NROW(covs), length(randomEffectOutputs), dimnames = list(NULL, names(randomEffectOutputs)))
    lks <- lapply(seq_along(ids), function(i) ledgerLinkArgs(sa, i, floor = P(sim)$lowerSpreadProb))
    links <- unique(unlist(lapply(lks, `[[`, "link")))
    if (length(links) > 1L)
      stop("fireSense_spreadPredict: the fitted ELFs have different spread links (", paste(links, collapse = ", "),
           "), whose parameters cannot be blended", call. = FALSE)
    ## with the activation x is climate and pc a fuel potential; in the other modes x holds the fuels: a blend of an
    ## activation ELF with another is neither model. (A ceiling ELF blended with a predictor ELF is allowed, as before.)
    modes <- unique(vapply(seq_along(ids), function(i) ledgerSpreadClimate(sa, i), character(1)))
    if (length(modes) > 1L && "activation" %in% modes)
      stop("fireSense_spreadPredict: the fitted ELFs were fitted with different spreadClimate (", paste(modes, collapse = ", "),
           "), whose linear predictors and parameters cannot be blended", call. = FALSE)
    sim$fireSense_SpreadLink <- links
    linkParNames <- if (length(links)) fireSenseUtils::spreadLinkParNames[[links]] else character()
    accPL <- matrix(0, NROW(covs), length(linkParNames), dimnames = list(NULL, linkParNames))
    accX <- numeric(NROW(covs))
    wsumPL <- numeric(NROW(covs))   ## weight of the ELFs with a linear-predictor link only
    ## the ELF label of every cell (a whole-raster read): once, and only if a fit has split climate terms
    isSplit <- vapply(seq_along(ids), function(i)
      any(fireSenseUtils::isClimateSplitCol(fireSenseUtils::spreadSourceCols(names(paramSetForRep(sa$params[[i]], rep))))),
      logical(1))
    elfLab <- if (any(isSplit)) fireSenseUtils::elfCellLabels(sim$rasterToMatchLargeELF[[1]])
    for (i in seq_along(ids)) {
      these <- which(w[, i] > 0)
      if (!length(these)) next
      parsI <- paramSetForRep(sa$params[[i]], rep)
      p <- do.call(spreadProbOneELF, c(list(covs[these], params = parsI, covMinMax = sa$covMinMax_spread[[i]],
                            yr = time(sim), maxFireSpread = P(sim)$maxFireSpread, flammableRTM = sim$flammableRTM,
                            lowerSpreadProb = P(sim)$lowerSpreadProb,
                            elfPixels = if (isSplit[i]) which(elfLab %in% ids[i]),
                            covCentre = ledgerCovCentre(sa, i), climateRef = ledgerClimateRef(sa, i),
                            powerLawPc50 = ledgerPowerLawPc50(sa, i)), lks[[i]]))
      rows <- these[match(p$pixelID, covs$pixelID[these])]
      acc[rows] <- acc[rows] + w[rows, i] * p$spreadProb
      accSD[rows, ] <- accSD[rows, ] + w[rows, i] * rep(vapply(randomEffectOutputs, function(txt) randomEffectSDOf(parsI, txt), numeric(1)), each = length(rows))
      if (!is.null(lks[[i]]$link)) {
        ## one row per pixel with climate in the ceiling, else the ELF's one vector for all its pixels
        linkParsI <- attr(p, "linkPars")
        if (!is.matrix(linkParsI)) linkParsI <- matrix(linkParsI, length(rows), length(linkParsI), byrow = TRUE)
        accPL[rows, ] <- accPL[rows, ] + w[rows, i] * linkParsI
        accX[rows] <- accX[rows] + w[rows, i] * p$linearPredictor
        wsumPL[rows] <- wsumPL[rows] + w[rows, i]
      }
      wsum[rows] <- wsum[rows] + w[rows, i]
    }
    ok <- wsum > 0
    pred <- data.table(pixelID = covs$pixelID[ok], spreadProb = acc[ok] / wsum[ok])
    ## each ELF's random-effect sds, blended like the probabilities: fireSense_burn scales its draws by them
    for (nm in names(randomEffectOutputs)) {
      sim[[nm]] <- rast(sim$flammableRTM)
      sim[[nm]][covs$pixelID[ok]] <- accSD[ok, nm] / wsum[ok]
    }
    if (any(wsumPL > 0)) {   ## a power-law or Richards fit: the link's parameters blended over those ELFs; NA where there is none
      okPL <- ok & wsumPL > 0
      sim$fireSense_SpreadPowerLaw <- linkParsRas(sim$flammableRTM, covs$pixelID[okPL], accPL[okPL, , drop = FALSE] / wsumPL[okPL])
      sim$fireSense_SpreadLinear <- linearPredictorRas(sim$flammableRTM,
                                                       data.table(pixelID = covs$pixelID[okPL],
                                                                  linearPredictor = accX[okPL] / wsumPL[okPL]))
    }
  }

  # Return to raster format
  sim$fireSense_SpreadPredicted <- rast(sim$flammableRTM) ## use flammableRTM as template
  sim$fireSense_SpreadPredicted[pred$pixelID] <- pred$spreadProb

  if (!is.na(P(sim)$paramSets))
  {
    outs <- lapply(stats::setNames(surfaceOutputs, surfaceOutputs),
                   function(nm) if (inherits(sim[[nm]], "SpatRaster")) terra::deepcopy(sim[[nm]]) else sim[[nm]])
    mod$surface <- list(key = key, outputs = outs, samples = lapply(outs, rasterSample))
  }

  invisible(sim)
}

#' The parameter set of the `paramSets` file that a replicate uses
#'
#' Replicate `rep` uses set `ceiling(rep / repsPerSet)` (`paramSetIndex()` cycles through the sets), and is
#' replicate `((rep - 1) %% repsPerSet) + 1` within it. The set keeps the columns of the simulated ELF's ledger
#' row (`sa$params`), and the random-effect sds it has.
#'
#' @param sim A `simList`; parameters `paramSets`, `repsPerSet`.
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param rep integer; the replicate (`.rep`, or `replicateOf()`).
#' @return A list: `own` (the simulated ELF's row of `sa`), `set` (its index in the file), `k` (the replicate
#'   within the set) and `params` (the one-row `data.frame`).
paramSetsForRep <- function(sim, sa, rep) {
  idCol <- fireSenseUtils::polygonIDTxt
  elf <- as.character(sim$.ELFind)
  own <- if (NROW(sa) == 1L) 1L else if (length(elf) == 1L && !is.na(elf)) which(as.character(sa[[idCol]]) == elf) else integer()
  if (length(own) > 1L)
    stop("fireSense_spreadPredict: sim$studyAreaWithSpreadParams has ", length(own), " rows for ELF ", elf,
         "; paramSets needs exactly one", call. = FALSE)
  if (length(own) != 1L)
    stop("fireSense_spreadPredict: paramSets with several fitted ELFs needs sim$.ELFind to be ONE of the ELFs of ",
         "sim$studyAreaWithSpreadParams (", paste(sa[[idCol]], collapse = ", "), "); it is ",
         if (is.null(sim$.ELFind)) "missing" else paste(sim$.ELFind, collapse = ", "), call. = FALSE)
  path <- P(sim)$paramSets
  if (!identical(mod$paramSetsFile$path, path)) {
    sets <- if (grepl("[.]csv$", path, ignore.case = TRUE)) data.table::fread(path, data.table = FALSE) else readRDS(path)
    isLedger <- is.data.frame(sets) && "params" %in% names(sets)
    if (isLedger) {   ## ledger rows
      if (idCol %in% names(sets)) sets <- sets[as.character(sets[[idCol]]) %in% as.character(sa[[idCol]][own]), , drop = FALSE]
      sets <- do.call(rbind, lapply(sets$params, as.data.frame))
    }
    if (!NROW(sets))
      stop("fireSense_spreadPredict: the paramSets file ", path, " has no parameter set",
           if (isLedger) paste0(" (the ledger rows have none for ELF ", sa[[idCol]][own], ")"), call. = FALSE)
    mod$paramSetsFile <- list(path = path, sets = as.data.frame(sets))
  }
  sets <- mod$paramSetsFile$sets
  per <- P(sim)$repsPerSet
  set <- paramSetIndex(NROW(sets), ceiling(rep / per))
  ledgerPars <- names(sa$params[[own]])
  lacks <- setdiff(ledgerPars, names(sets))
  if (length(lacks))
    stop("fireSense_spreadPredict: the paramSets file ", path, " lacks the column(s) ",
         paste(lacks, collapse = ", "), " that the ledger row of ELF ", own, " has (a coefficient the spread formula uses, ",
         "or a random-effect sd the run needs)",
         call. = FALSE)
  keep <- intersect(names(sets), c(ledgerPars, fireSenseUtils::spreadRandomEffectTxt))
  list(own = own, set = set, k = as.integer(((rep - 1L) %% per) + 1L), params = sets[set, keep, drop = FALSE],
       id = if (idCol %in% names(sa)) as.character(sa[[idCol]][own]) else "")
}

#' The random seed of replicate `k` of a year and ELF
#'
#' Not a function of the parameter set, so every set gets the same random numbers (common random numbers);
#' it is of the year and the ELF, so a hindcast's years and ELFs do not repeat each other's draws.
#'
#' @param year numeric; the job's year (`.runInitialTime`).
#' @param k integer; the replicate within its set.
#' @param id character; the simulated ELF (`""` if unknown).
#' @return An integer for `set.seed()`.
paramSetsSeed <- function(year, k, id) {
  idHash <- sum(utf8ToInt(id) * seq_len(nchar(id)))
  as.integer((round(year) * 1000003 + k * 7919 + idHash * 104729) %% .Machine$integer.max)
}

#' Values of a raster at up to 1000 evenly spaced cells
#'
#' A cheap check that a raster has not been edited in place (`terra::set.values()`): reading all the values of
#' a 20-million-cell raster takes about as long as predicting.
#'
#' @param r A `SpatRaster`, or anything else (returned as `NULL`).
#' @return The values, or `NULL`.
rasterSample <- function(r) {
  if (!inherits(r, "SpatRaster")) return(NULL)
  n <- terra::ncell(r)
  r[unique(round(seq(1, n, length.out = min(n, 1000L))))][[1]]
}

#' A number that changes when a raster input changes
#'
#' The version of `r` only changes with a new `SpatRaster` object (terra's `[<-` and `values<-` make one) or
#' different sampled values (`rasterSample()`); `mod$rasterStamp` keeps the last object, so its address cannot be
#' reused by another. It is for the key of `mod$surface`, so the full raster is not read every replicate.
#'
#' @param name character; `mod$rasterStamp` entry.
#' @param r A `SpatRaster`, or `NULL`.
#' @return An integer; `NULL` for `NULL`.
rasterVersion <- function(name, r) {
  if (is.null(r)) return(NULL)
  cur <- mod$rasterStamp[[name]]
  smp <- rasterSample(r)
  if (is.null(cur) || !identical(cur$obj@pntr, r@pntr) || !identical(cur$sample, smp))
    cur <- list(obj = r, sample = smp, version = (if (is.null(cur)) 0L else cur$version) + 1L)
  mod$rasterStamp[[name]] <- cur
  cur$version
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
         "must come from sim$rasterToMatchLargeELF (fireSense_ELFs, with a studyArea_fireSense); it is missing",
         call. = FALSE)
  if (!isTRUE(terra::compareGeom(elfRas, template, stopOnError = FALSE)))
    stop("fireSense_spreadPredict: sim$rasterToMatchLargeELF is not on the grid of sim$flammableRTM",
         call. = FALSE)
  r <- elfRas[[1]]
  lab <- fireSenseUtils::elfCellLabels(r)
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
#' @param elfPixels integer; the `pixelID`s of the ELF's own burnable pixels, the pixels the year's median of a split
#'   climate covariate (`<var>_yr`, `<var>_dev`; `fireSenseUtils::splitSpreadClimate()`) is taken over, as in the fit.
#'   `NULL` (one fitted ELF) is every row of `covs` that is flammable. Not used by a fit without split terms.
#' @param flammableRTM `sim$flammableRTM`; with `elfPixels` it limits the median to the ELF's flammable pixels (`fireSenseUtils::climateSplitRefPixels()`, the fit's choice).
#' @param yr,maxFireSpread,lowerSpreadProb as for `fireSenseUtils::spreadProbFromIntegerCovs()` and
#'   `fireSenseUtils::logisticAll()`.
#' @param covCentre the ELF's `covCentre_spread` (`ledgerCovCentre()`): the covariate means the fit subtracted
#'   from the rescaled covariates; `NULL` (a fit without an intercept, or an older ledger row) centres nothing.
#'   A parameter set with `fireSenseUtils::spreadInterceptTxt` has an intercept, added as a column of 1s after
#'   the covariates are rescaled and centred, as in the fit. A coefficient named `a:b` is an interaction: `covs` must
#'   have `a` and `b`, and `fireSenseUtils::spreadProbFromIntegerCovs()` multiplies them after each is
#'   rescaled and centred (0 where `youngAge` cleared the fuel), exactly as in the fit.
#' @param climateRef the ELF's `climateRef_spread` (`ledgerClimateRef()`): with a fit made with climate outside the
#'   `maxAsymptote` ceiling (`fireSense_spreadFit`'s `spreadLink = "climateOutside"`) or moving it (`spreadClimate =
#'   "ceiling"`), the reference value of each climate covariate; `fireSenseUtils::spreadProbFromCovs()` and
#'   `spreadLinkInputs()` then apply that link, exactly as the fit did.
#'   `NULL` (a fit with climate under the ceiling, or an older ledger row) is the link as it always was.
#' @param powerLawGamma the ELF's `powerLawGamma_spread` (`ledgerPowerLawGamma()`): with a fit made with
#'   `fireSense_spreadFit`'s `spreadLink = "powerLaw"`, its `gamma`; `fireSenseUtils::spreadProbFromCovs()` then
#'   applies that link (`fireSenseUtils::powerLawLink()`, with `maxAsymptote` as `p_c`), exactly as the fit did.
#'   `NULL` (any other link) is the link as it always was.
#' @param powerLawShape the ELF's `powerLawShape_spread` (`ledgerPowerLawShape()`): the `shape` of the link, with
#'   `powerLawGamma`; 1 (the original link) by default.
#' @param link the ELF's link when it is one of `fireSenseUtils::spreadLinearLinks` (`ledgerLinkArgs()`); `NULL` (any
#'   other link). By default `"powerLaw"` when `powerLawGamma` is given, the link of a row written before the
#'   others existed.
#' @param lowerAsymptote,richardsNu the ELF's `lowerAsymptote_spread` and `richardsNu_spread` (`ledgerLowerAsymptote()`,
#'   `ledgerRichardsNu()`): with `"powerLawSmooth"` and `"richards"`, the lower asymptote, and `nu` of `"richards"`.
#' @param powerLawPc50 the ELF's `powerLawPc50_spread` (`ledgerPowerLawPc50()`): with `climateRef` and a power-law `link`
#'   (a fit with `fireSense_spreadFit`'s `spreadClimate = "ceiling"`), the runaway threshold the ceiling moves toward;
#'   `fireSenseUtils::spreadLinkInputs()` ignores it without `climateRef`.
#' @param spreadClimate `NULL` (a row that does not record it: predicted as it always was), or the row's recorded
#'   `spreadClimate` (`ledgerLinkArgs()`). `"predictor"` predicts with climate in x even when the row has a `climateRef`;
#'   `"ceiling"` and `"activation"` need one. With `"activation"` the parameter set's
#'   `activationIntercept` (`fireSenseUtils::activationInterceptTxt`) is not a covariate coefficient: it goes to
#'   `fireSenseUtils::spreadLinkInputs()`, where fuel sets each pixel's `pc` below `powerLawPc50` and climate is `x`.
#' @return `data.table` with `pixelID` and `spreadProb`, from the one parameter set in `params`; with a `link`
#'   also `linearPredictor`, the `x` that `spreadProb` is the link of (the fuels' only with climate in the ceiling), and
#'   the attribute `linkPars`, the link's parameters (`fireSenseUtils::linearLinkPars()`): a vector, or with climate in
#'   the ceiling a matrix with one row per pixel.
spreadProbOneELF <- function(covs, params, covMinMax, yr, maxFireSpread, lowerSpreadProb, elfPixels = NULL, flammableRTM = NULL,
                             covCentre = NULL, climateRef = NULL, powerLawPc50 = NULL, powerLawGamma = NULL,
                             powerLawShape = fireSenseUtils::spreadPowerLawShape,
                             link = if (!is.null(powerLawGamma)) "powerLaw",
                             lowerAsymptote = fireSenseUtils::spreadProbFloor,
                             richardsNu = fireSenseUtils::spreadRichardsNu, spreadClimate = NULL) {
  moduleName <- "fireSense_spreadPredict"
  covs <- copy(covs)
  actTxt <- fireSenseUtils::activationInterceptTxt
  activation <- identical(spreadClimate, "activation")
  if (activation && !actTxt %in% names(params))
    stop(moduleName, "> a parameter set of spreadClimate 'activation' has no '", actTxt, "'", call. = FALSE)
  if (!activation && actTxt %in% names(params))
    stop(moduleName, "> a parameter set has '", actTxt, "', but its ledger row's spreadClimate is not 'activation'",
         call. = FALSE)
  activationIntercept <- if (activation) params[[actTxt]][[1L]]
  ## a recorded mode is the fit's: "predictor" keeps climate in x, even with a climateRef; the other two need one
  if (!is.null(link) && identical(spreadClimate, "predictor")) climateRef <- NULL
  if (any(spreadClimate %in% c("ceiling", "activation")) && is.null(climateRef))
    stop(moduleName, "> a ledger row of spreadClimate '", spreadClimate, "' has no '", fireSenseUtils::spreadFitClimateRefTxt,
         "'", call. = FALSE)
  ## the random effects are not covariate coefficients: fireSense_burn applies them (randomEffectOutputs); nor is the
  ## activation's intercept, which goes to the link (spreadLinkInputs())
  randomEffects <- intersect(names(params), c(fireSenseUtils::spreadRandomEffectTxt, actTxt))
  if (length(randomEffects)) {
    params <- as.data.frame(params)
    params[randomEffects] <- NULL
  }

  ## Fuel biomass arrives logged (fireSenseUtils::logMinB()). A fit made on LINEAR fuel biomass has
  ## fireSenseUtils::fuelLinearRange, c(0, 1e4), as that covariate's covMinMax_spread, and its
  ## coefficients only mean anything for biomass / 1e4: undo the log with the function the fit used.
  ## A fit made on the log scale has the log range there, and its covariates are left as they are.
  fuelCols <- character()
  for (cn in intersect(names(covMinMax), names(covs))) {
    if (fireSenseUtils::isLinearFuelRange(covMinMax[[cn]])) {
      data.table::set(covs, NULL, cn, fireSenseUtils::fuelLogToLinear(covs[[cn]])) # by reference: `[[<-` copies, and splitSpreadClimate() below then cannot add columns
      fuelCols <- c(fuelCols, cn)
    }
  }

  ## every fitted coefficient needs a covariate: without one the logistic gets no covariates and
  ## fails later in rowMeans(), with no hint of the cause
  ## the intercept, if the fit has one, is a coefficient with no covariate: it multiplies a column of 1s
  hasIntercept <- fireSenseUtils::spreadInterceptTxt %in% names(params)
  fitted <- setdiff(names(params), c(unlist(fireSenseUtils::logisticParamNames), fireSenseUtils::spreadInterceptTxt))
  ## an interaction (`CMD:fuel`) is made from its two covariates by spreadProbFromIntegerCovs(), as in the fit
  needed <- fireSenseUtils::spreadSourceCols(fitted)
  ## a fit made with fireSense_dataPrepFit's spreadClimateSplit has the climate as `<var>_yr` and `<var>_dev` (also in an
  ## interaction, `CMD_yr:fuel`): the year's median of the variable over this ELF's burnable pixels, and the departure
  ## from it, made as the fit made them
  splitVars <- unique(fireSenseUtils::climateSplitBase(needed[fireSenseUtils::isClimateSplitCol(needed)]))
  if (length(splitVars) && all(splitVars %in% names(covs))) {
    ref <- covs[covs$pixelID %in% fireSenseUtils::climateSplitRefPixels(covs$pixelID, flammableRTM, elfPixels), splitVars, with = FALSE]
    fireSenseUtils::splitSpreadClimate(covs, splitVars, ref)
  }
  noCov <- setdiff(needed, names(covs))
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
  covs <- covs[, c("pixelID", needed), with = FALSE]
  ## spreadProbFromIntegerCovs() rescales only the columns of covMinMax: a fitted covariate it lacks would
  ## silently stay on its raw scale (x1000)
  noRange <- setdiff(needed, names(covMinMax))
  if (length(noRange))
    stop(moduleName, "> covMinMax_spread has no range for the fitted covariate(s) ",
         paste(noRange, collapse = ", "), "; it has ", paste(names(covMinMax), collapse = ", "),
         ". The ranges must be the fit's (its ledger row).")

  # integers x 1000, the form `spreadProbFromIntegerCovs` expects
  shortAnnDTx1000 <- toX1000(list(covs))[[1]] |> setDT()
  colsToUse <- setdiff(names(covs), "pixelID")
  ## what the coefficients multiply: the intercept first, as in the fit (fireSenseUtils::spreadDesignCols())
  designCols <- c(if (hasIntercept) fireSenseUtils::spreadInterceptTxt, fitted)

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
  mat <- mat[, matching, drop = FALSE]
  if (is.null(link))
    return(data.table(pixelID = shortAnnDT$pixelID,
                      spreadProb = spreadProbFromCovs(mat, logisticPars, covPars, lowerSpreadProb, link = link,
                                                      climateRef = climateRef, powerLawGamma = powerLawGamma,
                                                      powerLawShape = powerLawShape, lowerAsymptote = lowerAsymptote,
                                                      richardsNu = richardsNu)))
  ## a link of `fireSenseUtils::spreadLinearLinks`: x and the link's parameters, which `spreadProbFromCovs()` turns
  ## into p the same way. With climate in the ceiling x is the fuels' only and the parameters (`pc`) are one row per pixel.
  ## "richards" has no gamma, and does not use one
  if (is.null(powerLawGamma)) powerLawGamma <- fireSenseUtils::spreadPowerLawGamma
  inputs <- spreadLinkInputs(mat, logisticPars, covPars, lowerSpreadProb, link, climateRef, powerLawPc50,
                             powerLawGamma = powerLawGamma,
                             powerLawShape = powerLawShape, lowerAsymptote = lowerAsymptote, richardsNu = richardsNu,
                             activationIntercept = activationIntercept)
  ## the link's input: the power law clips it at 0, so it cannot be recovered from spreadProb
  out <- data.table(pixelID = shortAnnDT$pixelID, spreadProb = linearLinkProb(inputs$x, link, inputs$pars),
                    linearPredictor = inputs$x)
  setattr(out, "linkPars", inputs$pars)
  out
}

#' `fireSense_SpreadLinear`: the linear predictor on the grid of `flammableRTM`
#'
#' @param template `sim$flammableRTM`.
#' @param dt `data.table` with `pixelID` (cells of `template`) and `linearPredictor`.
#' @return A `SpatRaster`, `NA` outside `dt`.
linearPredictorRas <- function(template, dt) {
  r <- rast(template)
  r[dt$pixelID] <- dt$linearPredictor
  names(r) <- "fireSense_SpreadLinear"
  r
}

## the outputs that carry a fitted random-effect sd, named by the parameter each takes it from; together they
## are `fireSenseUtils::spreadRandomEffectTxt`
randomEffectOutputs <- list(fireSense_SpreadFireSD = fireSenseUtils::fireSpreadSDTxt,
                            fireSense_SpreadSD = fireSenseUtils::yearSpreadSDTxt)

## the outputs that depend on the parameter set and the covariates only (`mod$surface`)
surfaceOutputs <- c("fireSense_SpreadPredicted", names(randomEffectOutputs), "fireSense_SpreadLink",
                    "fireSense_SpreadPowerLaw", "fireSense_SpreadLinear")

#' `fireSense_SpreadPowerLaw` with a parameter per pixel: the link's parameters on the grid of `flammableRTM`
#'
#' With climate in the ceiling `pc` differs by pixel (`fireSenseUtils::spreadLinkInputs()`), so the parameters are a raster
#' with one layer per name, as with several ELFs.
#'
#' @param template `sim$flammableRTM`.
#' @param pixelID cells of `template`, one per row of `pars`.
#' @param pars matrix, one column per parameter (`fireSenseUtils::spreadLinkParNames` of the link).
#' @return A `SpatRaster`, `NA` outside `pixelID`.
linkParsRas <- function(template, pixelID, pars) {
  r <- rast(template, nlyrs = ncol(pars))
  names(r) <- colnames(pars)
  for (nm in colnames(pars)) r[[nm]][pixelID] <- pars[, nm]
  r
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

#' The runaway threshold of one ledger row
#'
#' A fit made with `fireSense_spreadFit`'s `spreadClimate = "ceiling"` moves `maxAsymptote` toward the landscape's
#' runaway threshold `p50`, stored in the ledger row (`fireSenseUtils::spreadFitPowerLawPc50Txt`) as for every power-law
#' fit. Only a row with `climateRef_spread` (`ledgerClimateRef()`) uses it.
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @return `p50`, or `NULL` for a row without the column or with `NA`. A row with `climateRef_spread` and a power-law
#'   link but no finite `p50` stops, naming the row.
ledgerPowerLawPc50 <- function(sa, i) {
  col <- fireSenseUtils::spreadFitPowerLawPc50Txt
  p50 <- if (col %in% names(sa)) sa[[col]][[i]]
  if (!is.null(ledgerClimateRef(sa, i)) && any(ledgerSpreadLink(sa, i) %in% fireSenseUtils::spreadPowerLawLinks) &&
      !(is.numeric(p50) && length(p50) == 1L && is.finite(p50)))
    stop("fireSense_spreadPredict: ledger row ", i, " has climate in the ceiling ('", fireSenseUtils::spreadFitClimateRefTxt,
         "' and spreadLink '", ledgerSpreadLink(sa, i), "') but no finite '", col, "' (", format(p50), ")", call. = FALSE)
  if (is.null(p50) || is.na(p50)) NULL else p50
}

#' The power-law exponent of one ledger row
#'
#' A fit made with `fireSense_spreadFit`'s `spreadLink = "powerLaw"` or `"powerLawSmooth"` stores its `gamma` in the ledger row
#' (`fireSenseUtils::spreadFitPowerLawGammaTxt`), with the link in `fireSenseUtils::spreadLinkTxt`. Prediction
#' applies the same link with the same `gamma` (`fireSenseUtils::spreadProbFromCovs()`).
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @return `gamma`, or `NULL` for a row of any other link (`"richards"` has none), such as every row written before the
#'   link existed. Such a row predicts exactly as it always did. A `"powerLaw"` or `"powerLawSmooth"` row without a valid
#'   `gamma` stops.
ledgerPowerLawGamma <- function(sa, i) {
  link <- ledgerSpreadLink(sa, i)
  if (!any(link %in% fireSenseUtils::spreadPowerLawLinks)) return(NULL)
  gamma <- sa[[fireSenseUtils::spreadFitPowerLawGammaTxt]][[i]]
  if (!(is.numeric(gamma) && length(gamma) == 1L && is.finite(gamma) && gamma > 0))
    stop("fireSense_spreadPredict: ledger row ", i, " has spreadLink '", link, "' but no valid '",
         fireSenseUtils::spreadFitPowerLawGammaTxt, "' (", format(gamma), ")", call. = FALSE)
  gamma
}

#' The spread link of one ledger row
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @return The row's `fireSenseUtils::spreadLinkTxt`, or `NULL` for a row without one (the default link, and every row
#'   written before the column existed).
ledgerSpreadLink <- function(sa, i) {
  linkCol <- fireSenseUtils::spreadLinkTxt
  if (linkCol %in% names(sa)) as.character(sa[[linkCol]][[i]])
}

#' A number of one ledger row, for the links that record it
#'
#' The row's `lowerAsymptote_spread` and `richardsNu_spread` are checked together by
#' `fireSenseUtils::checkLinkParams()`, the check `fireSense_spreadFit` makes of its parameters; an invalid or missing
#' value stops, naming the row.
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @param col the ledger column.
#' @param links the links that write `col`; any other row gives `NULL`.
#' @param floor the run's `lowerSpreadProb`, which an activation row's lower asymptote must equal.
#' @return The value, or `NULL` for a row of another link.
ledgerLinkNumber <- function(sa, i, col, links, floor = fireSenseUtils::spreadProbFloor) {
  link <- ledgerSpreadLink(sa, i)
  if (!any(link %in% links)) return(NULL)
  num <- function(cn) if (cn %in% names(sa)) sa[[cn]][[i]]
  tryCatch(fireSenseUtils::checkLinkParams(link, num(fireSenseUtils::spreadFitLowerAsymptoteTxt),
                                           num(fireSenseUtils::spreadFitRichardsNuTxt),
                                           activation = identical(ledgerSpreadClimate(sa, i), "activation"), floor = floor),
           error = function(e) stop("fireSense_spreadPredict: ledger row ", i, " has spreadLink '", link, "': ",
                                    conditionMessage(e), " (columns '", fireSenseUtils::spreadFitLowerAsymptoteTxt, "', '",
                                    fireSenseUtils::spreadFitRichardsNuTxt, "')", call. = FALSE))
  num(col)
}

#' The lower asymptote of one ledger row
#'
#' `fireSense_spreadFit`'s `lowerAsymptote` of a `"powerLawSmooth"` or `"richards"` fit, in
#' `fireSenseUtils::spreadFitLowerAsymptoteTxt`.
#'
#' @inheritParams ledgerLinkNumber
#' @return A number from 0 to `fireSenseUtils::spreadProbFloor`; `NULL` for a row of another link.
ledgerLowerAsymptote <- function(sa, i, floor = fireSenseUtils::spreadProbFloor)
  ledgerLinkNumber(sa, i, fireSenseUtils::spreadFitLowerAsymptoteTxt, fireSenseUtils::spreadAsymptoteLinks, floor)

#' The `nu` of one ledger row
#'
#' `fireSense_spreadFit`'s `richardsNu` of a `"richards"` fit, in `fireSenseUtils::spreadFitRichardsNuTxt`.
#'
#' @inheritParams ledgerLinkNumber
#' @return A number, 0 or more; `NULL` for a row of another link.
ledgerRichardsNu <- function(sa, i, floor = fireSenseUtils::spreadProbFloor)
  ledgerLinkNumber(sa, i, fireSenseUtils::spreadFitRichardsNuTxt, "richards", floor)

#' The link of one ledger row and its parameters, as the arguments of `spreadProbOneELF()`
#'
#' @inheritParams ledgerLinkNumber
#' @return A list of `link` (only for a link of `fireSenseUtils::spreadLinearLinks`), `powerLawGamma` (not `"richards"`),
#'   `powerLawShape`, `lowerAsymptote` and `richardsNu` (the links that have them), and `spreadClimate` when the row
#'   records it; the others are left out, so they take `spreadProbOneELF()`'s defaults. A row of any other link,
#'   or without one, has `powerLawShape` only.
ledgerLinkArgs <- function(sa, i, floor = fireSenseUtils::spreadProbFloor) {
  link <- ledgerSpreadLink(sa, i)
  if (!any(link %in% fireSenseUtils::spreadLinearLinks)) link <- NULL
  col <- fireSenseUtils::spreadClimateTxt
  recorded <- col %in% names(sa) && !is.na(sa[[col]][[i]])
  Filter(Negate(is.null), list(link = link, powerLawGamma = ledgerPowerLawGamma(sa, i),
                               powerLawShape = ledgerPowerLawShape(sa, i),
                               lowerAsymptote = ledgerLowerAsymptote(sa, i, floor), richardsNu = ledgerRichardsNu(sa, i, floor),
                               spreadClimate = if (recorded) ledgerSpreadClimate(sa, i)))
}

#' Where the fit of one ledger row put climate
#'
#' `fireSense_spreadFit` records its `spreadClimate` in the row (`fireSenseUtils::spreadClimateTxt`). A row without it
#' (written before it existed) is read as the fit made it: `"ceiling"` when it has `climateRef_spread`
#' (`ledgerClimateRef()`) and a power-law link, else `"predictor"`.
#'
#' @inheritParams ledgerLinkNumber
#' @return One of `fireSenseUtils::spreadClimateOptions`; an unknown value stops, naming the row.
ledgerSpreadClimate <- function(sa, i) {
  col <- fireSenseUtils::spreadClimateTxt
  mode <- if (col %in% names(sa)) as.character(sa[[col]][[i]])
  if (!length(mode) || is.na(mode))
    return(if (!is.null(ledgerClimateRef(sa, i)) && any(ledgerSpreadLink(sa, i) %in% fireSenseUtils::spreadPowerLawLinks))
      "ceiling" else "predictor")
  if (!mode %in% fireSenseUtils::spreadClimateOptions)
    stop("fireSense_spreadPredict: ledger row ", i, " has an unknown '", col, "' (", mode, "); one of ",
         paste(fireSenseUtils::spreadClimateOptions, collapse = ", "), " expected", call. = FALSE)
  mode
}

#' The power-law curvature exponent of one ledger row
#'
#' `fireSense_spreadFit`'s `powerLawShape` is stored in the ledger row (`fireSenseUtils::spreadFitPowerLawShapeTxt`).
#' A row without the column (every row written before it existed) or with `NA` has shape 1, the original link.
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @param i integer; the row.
#' @return `shape`, a positive number; 1 when the row has none.
ledgerPowerLawShape <- function(sa, i) {
  col <- fireSenseUtils::spreadFitPowerLawShapeTxt
  shape <- if (col %in% names(sa)) sa[[col]][[i]]
  if (is.null(shape) || is.na(shape)) return(fireSenseUtils::spreadPowerLawShape)
  if (!(is.numeric(shape) && length(shape) == 1L && is.finite(shape) && shape > 0))
    stop("fireSense_spreadPredict: ledger row ", i, " has an invalid '", col, "' (", format(shape), ")", call. = FALSE)
  shape
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
  params[paramSetIndex(NROW(params), rep), , drop = FALSE]
}

#' The row of the parameter sets a replicate uses
#'
#' @param nSets integer; the number of fitted parameter sets of an ELF.
#' @param rep integer; the replicate.
#' @return integer, `((rep - 1) %% nSets) + 1`.
paramSetIndex <- function(nSets, rep) {
  as.integer(((rep - 1L) %% nSets) + 1L)
}

#' The replicate a `run` event is
#'
#' `round((time - start) / interval) + 1`, the numbering `fireSense_summary`'s `hindcastRecord()` uses
#' (`hindcastYear`, `hindcastInterval`); the formula is written in both modules. Times need not be whole years.
#'
#' @param time numeric; `time(sim)`.
#' @param start numeric; time of the first `run` event (`.runInitialTime`).
#' @param interval numeric; `.runInterval`; `NA` (one prediction) gives replicate 1.
#' @return integer.
replicateOf <- function(time, start, interval) {
  if (is.na(interval)) return(1L)
  as.integer(round((time - start) / interval)) + 1L
}

#' The fitted sd of a random effect
#'
#' `yearSpreadSD` (fireSense_spreadFit, fireSenseUtils >= 0.2.3.9041) is one eps per year on logit spread
#' probability; `fireSpreadSD` is one per fire. Both are in `fireSenseUtils::spreadRandomEffectTxt`.
#'
#' @param params one-row `data.frame`: the parameter set in use (`paramSetForRep()`).
#' @param txt the parameter name, `fireSenseUtils::yearSpreadSDTxt` or `fireSenseUtils::fireSpreadSDTxt`.
#' @return Numeric; 0 when the fit has no such parameter (a ledger row written before it existed).
randomEffectSDOf <- function(params, txt) {
  if (!txt %in% names(params)) return(0)
  params[[txt]]
}

#' The escape definition each ledger row recorded
#'
#' @param sa `sim$studyAreaWithSpreadParams`, one row per ELF.
#' @return A `data.frame` with the columns of `fireSenseUtils::escapeDefinition()` (`NA` for a row without them) and,
#'   if `sa` has it, `polygonID`: what `fireSenseUtils::checkEscapeDefinition()` checks against this run's definition.
ledgerEscapeDefinition <- function(sa) {
  tmpl <- fireSenseUtils::escapeDefinition()
  def <- as.data.frame(lapply(stats::setNames(names(tmpl), names(tmpl)), function(cl)
    if (cl %in% names(sa)) rep_len(sa[[cl]], NROW(sa)) else rep(tmpl[[cl]][NA_integer_], NROW(sa))))
  idCol <- fireSenseUtils::polygonIDTxt
  if (idCol %in% names(sa)) def[[idCol]] <- as.character(sa[[idCol]])
  def
}
