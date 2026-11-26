# fireSense_spreadPredict (development version)

- A fit made with `fireSense_dataPrepFit`'s `spreadClimateCurve` has a coefficient `<var>_curve` for each climate variable. Prediction builds that covariate, `softplus(<var> - k, s)`, from the pixel's own value and the `k` and `s` stored in the ledger row (`climateCurvePars_spread`, `fireSenseUtils::ledgerClimateCurvePars()`, `fireSenseUtils::addClimateCurve()`), as the fit did, and no new parameter is needed. A curve coefficient with no `k` and `s` in its row stops. A row without curve terms predicts as before. Needs fireSenseUtils with `addClimateCurve()` (>= 0.2.3.9196).

- The functions that read a ledger row (`ledgerLinkArgs()`, `ledgerCovCentre()`, `ledgerClimateRef()`, `ledgerPowerLawPc50()`, `ledgerSpreadClimate()`, `ledgerCovMinMax()` and their helpers) now live in `fireSenseUtils`, which also uses them (`spreadFuelEffect()`, the fuel effect on the ELF map), so a row is read in one place. They are moved unchanged; the only difference is that their errors say "spread ledger row" instead of "fireSense_spreadPredict: ledger row". Needs fireSenseUtils >= 0.2.3.9191 (PredictiveEcology/fireSenseUtils#260, version to be set after it merges).

- A ledger row of `fireSense_spreadFit`'s `spreadClimate = "activation"` predicts as that fit: fuel sets each pixel's ceiling `pc` below p50 and climate is the linear predictor (`fireSenseUtils::spreadLinkInputs(activationIntercept =)`). The row's mode is read by the new `ledgerSpreadClimate()` (column `fireSenseUtils::spreadClimateTxt`); a row without it is "ceiling" when it has `climateRef_spread` and a power-law link, else "predictor", as before. The parameter set's `activationIntercept` is not a covariate coefficient: it goes to the link, and a set and its row that disagree stop. `fireSense_SpreadPowerLaw` is then a per-pixel raster, and `fireSense_SpreadLinear` is the climate term, so `fireSense_burn` adds the year effect to it unchanged. A row that records its mode is predicted by it: "predictor" keeps climate in x even with a `climateRef_spread`, and "ceiling" or "activation" without one stops. An activation ELF is not blended with an ELF of another mode (an error, beside the one for different links; ceiling with predictor still blends), and the activation's lower-asymptote check uses the run's `lowerSpreadProb`. Other rows predict as before. Needs fireSenseUtils >= 0.2.3.9183 (PredictiveEcology/fireSenseUtils#255).

- The yearly spread probability map (`.plots = "png"`) is bigger. Its png is now 10 x 8 inches at 144 dpi (1440 x 1152 pixels), as fireSense_burn's; it was R's `png()` default of 480 x 480, which looked small in `SpaDES.shiny::shine()`.

- A ledger row from a `fireSense_spreadFit` with `spreadClimate = "ceiling"` (a power-law `spreadLink` with `climateRef_spread` and `powerLawPc50_spread`) is predicted with climate moving the ceiling `pc` toward p50 (`fireSenseUtils::spreadLinkInputs()`): new `ledgerPowerLawPc50()`; `spreadProbOneELF()` takes `powerLawPc50`, returns the fuels' linear predictor as `linearPredictor` and the link's parameters as the attribute `linkPars` (one row per pixel here). `fireSense_SpreadPowerLaw` is then a raster with a `pc` per pixel even for one ELF, and several ELFs blend the per-pixel parameters (the vector `powerLawParsOf()` is gone: `spreadLinkInputs()` makes the parameters). Rows without `climateRef_spread` predict exactly as before. `fireSense_burn` already reads a per-cell raster of the link's parameters. Needs fireSenseUtils >= 0.2.3.9181 (PredictiveEcology/fireSenseUtils#250).

- New parameter `.plots` (default `"screen"`): the spread probability map (`fireSense_SpreadPredicted`) is plotted each `run` (`SpreadProbability_yr<year>`). `NA` or `NULL` for none.

- A ledger row with an interaction coefficient (`a:b`, e.g. `CMD:dom_agb_Pice_mar`, from a `fireSense_spreadFit` fitted with `fireSense_dataPrepFit`'s `spreadInteractions = "climate:fuel"`) predicts with it: `spreadProbOneELF()` needs the covariates `a` and `b` (and their ranges in `covMinMax_spread`), and `fireSenseUtils::spreadProbFromIntegerCovs()` forms their product after each is rescaled and centred with the row's `covCentre_spread` entry for it (0 where `youngAge` cleared the fuel), and multiplies it by the coefficient, as the fit does. A row without interactions predicts exactly as before. Needs fireSenseUtils >= 0.2.3.9177 (#242).

- A split fit with fuel in the linear range (every fit with `spreadClimateSplit` and fuel) predicted nothing: `spreadProbOneELF()` turned the logged fuel to linear with `covs[[cn]] <-`, which copies the table, so `fireSenseUtils::splitSpreadClimate()` then could not add `CMD_yr` and `CMD_dev` and the prediction stopped with "fitted coefficients CMD_yr, CMD_dev have no covariate". It now sets the fuel by reference. A test covers a `CMD_yr:fuel` interaction with an intercept, centres and `youngAge`.
* A ledger row fitted with the split climate covariate (`fireSense_dataPrepFit`'s `spreadClimateSplit`: parameters `<var>_yr` and `<var>_dev`) is predicted with the same two terms: `spreadProbOneELF()` makes them from the year's climate with `fireSenseUtils::splitSpreadClimate()`, the median being over the ELF's own burnable pixels (`elfPixels`: every row with one fitted ELF, the ELF's own pixels of `rasterToMatchLargeELF` with several, not the blend zone), and only its flammable pixels (`fireSenseUtils::climateSplitRefPixels()`, the choice the fit makes). `ELFblendWeights()` reads the cells' ELF labels with `fireSenseUtils::elfCellLabels()`. A row without split terms predicts as before. Needs fireSenseUtils with `splitSpreadClimate()` (Depends-on PredictiveEcology/fireSenseUtils); the `reqdPkgs` floor is set after that merges.

- With `paramSets`: the cache of a parameter set's surface no longer reads all the values of `flammableRTM` and `rasterToMatchLargeELF` every replicate (a new raster object or a change at 1000 sampled cells is a change) and returns the stored surface without copying it (a changed stored surface is recomputed). On a 20-million-cell raster a replicate that reuses a surface takes 0.35 s instead of 0.9 s, most of it SpaDES. `paramSets` stops if the ledger rows have the simulated ELF twice, or if a file of ledger rows has none for it.

- New parameters `paramSets` (path to an `.rds` or `.csv` of spread parameter vectors for the simulated ELF; default `NA`, behaviour unchanged) and `repsPerSet`. The sets replace the simulated ELF's ledger rows (neighbour ELFs keep theirs); replicate r uses set `ceiling(r / repsPerSet)`, calls `set.seed(k)` with k its number within the set (common random numbers across sets), and the surface is recomputed only when the set changes. The link, lower asymptote, `gamma` and covariate centres come from the ledger row; a set lacking a coefficient of the row stops. The set index is output in `fireSense_SpreadParamSet`. New input `.ELFind`, as in `fireSense_dataPrepPredict`.

- A ledger row with `spreadLink` `"powerLawSmooth"` or `"richards"` (and its `lowerAsymptote_spread`, `richardsNu_spread`) is predicted with that link (`fireSenseUtils::spreadProbFromCovs()`); `"richards"` has no `gamma`. New output `fireSense_SpreadLink` names the link, and `fireSense_SpreadPowerLaw` and `fireSense_SpreadLinear` are given for all three linear-predictor links, with the link's own parameter names (`fireSenseUtils::spreadLinkParNames`). Fitted ELFs with different such links cannot be blended (an error). Needs fireSenseUtils with the links.

- The ledger's escape definition is checked with `fireSenseUtils::checkEscapeDefinition()` on the whole one-row-per-ELF table (the error names the ELF, not the row number), this run's definition is `fireSenseUtils::escapeDefinitionOf()`, and the random-effect names are `fireSenseUtils::fireSpreadSDTxt` and `yearSpreadSDTxt` (PredictiveEcology/fireSenseUtils#222), instead of local copies. Needs fireSenseUtils with that PR.

- A ledger row with the new fitted parameter `fireSpreadSD` (the per-fire random-effect sd of `fireSense_spreadFit`) no longer stops with "have no covariate": `spreadProbOneELF()` drops every name in `fireSenseUtils::spreadRandomEffectTxt` from the fitted coefficients, so spread probabilities are unchanged. The value is a new output `fireSense_SpreadFireSD` for `fireSense_burn`, shaped like `fireSense_SpreadSD` (one number with one ELF, a blended raster with several; 0 for a row without it). New output `fireSense_EscapeDefinition` (one row per ELF) holds the escape definition recorded in the ledger (`fireSenseUtils::escapeDefinition()`), and each row is checked against this run's new parameters `escapeSizeHa` and `fireCauses` (shared with the other fireSense modules) with `fireSenseUtils::checkEscapeDefinition()`: a mismatch stops. A row without a recorded definition is not checked. Merge this before any fit writes `fireSpreadSD`. Needs fireSenseUtils#221.

- A ledger row's `powerLawShape_spread` (`fireSenseUtils::spreadFitPowerLawShapeTxt`, written by `fireSense_spreadFit`'s new `powerLawShape`) is the curvature exponent of the `"powerLaw"` link, `p = pc - (pc - floor) * exp(-sign(x) * abs(x / gamma)^shape)`, applied in `spreadProbOneELF()` through `fireSenseUtils::spreadProbFromCovs()` (`ledgerPowerLawShape()`). A row without the column, or with `NA`, has shape 1, the original link, so such rows predict exactly as before. `fireSense_SpreadPowerLaw` gains a fourth element or layer, `shape`, for `fireSense_burn`. Needs fireSenseUtils >= 0.2.3.9137.

- New output `fireSense_SpreadLinear` (only for a fit made with `spreadLink = "powerLaw"`, else `NULL`): the linear predictor x of each flammable pixel, the step before `fireSenseUtils::powerLawLink()` makes `fireSense_SpreadPredicted`; with several ELFs it is blended like `fireSense_SpreadPowerLaw`. The link clips a pixel with a low x to p = 0, so `fireSense_burn` could not recover x from p and the year effect never reached those pixels (19-29% of the flammable pixels at ELF 11.3). `fireSense_SpreadPredicted` is unchanged.

- New parameter `paramSetByReplicate` (default `FALSE`, behaviour unchanged). If `TRUE`, the parameter set is chosen by the `run` event within the job, `round((time - .runInitialTime) / .runInterval) + 1` (the numbering `fireSense_summary` uses for hindcast replicates), instead of by `.rep`, so one hindcast job with 20 replicates and 5 fitted sets uses each set four times. The row used is output as `sim$fireSense_SpreadParamSet` (integer; named by ELF with several ELFs), which `fireSense_summary` writes as the `paramSet` column.

- A ledger row from a fit made with `fireSense_spreadFit`'s new `spreadLink = "powerLaw"` is predicted with that link: `spreadProbOneELF()` hands the row's `gamma` (`fireSenseUtils::spreadFitPowerLawGammaTxt`, `ledgerPowerLawGamma()`) to `fireSenseUtils::spreadProbFromCovs()` with `link = "powerLaw"`, so `p = pc - (pc - lowerSpreadProb) * exp(-x / gamma)` with `pc` the row's `maxAsymptote`, exactly as in the fit; with several ELFs each row uses its own. A `"powerLaw"` row without a valid `gamma` stops. Every other row, such as all rows written so far, predicts exactly as before. The year effect of such a fit is added to the linear predictor, not to the logit, so the module also outputs `fireSense_SpreadPowerLaw` (`pc`, `gamma`, `floor`; blended like `fireSense_SpreadSD` with several ELFs) for `fireSense_burn` (its follow-up PR), which recovers `x` from `p`; it is `NULL` for any other link. With mixed links it is `NA` where only a non-power-law ELF predicts. Needs fireSenseUtils >= 0.2.3.9107.

- A ledger row from a fit made with climate outside the `maxAsymptote` ceiling (`fireSense_spreadFit`'s new `spreadLink = "climateOutside"`) is predicted with that link: `spreadProbOneELF()` hands the row's stored climate reference (`fireSenseUtils::spreadFitClimateRefTxt`, `ledgerClimateRef()`) to `fireSenseUtils::spreadProbFromCovs()`, the one function the fit computes its spread probability with, so the climate term is added on the logit after the fuel ceiling exactly as in the fit; with several ELFs each row uses its own. `fireSense_burn` adds the year effect on the same logit, so nothing changes there. A row without a reference, such as every row written so far, predicts exactly as before (`spreadProbFromCovs()` with no reference is `logisticAll()`). Needs fireSenseUtils >= 0.2.3.9106.

- A single fitted ELF now rescales its covariates with the `covMinMax_spread` of its ledger row, as several ELFs already did, not with `sim$covMinMax_spread`, which `fireSense_spreadFit` rebuilds in every run from that run's tables (in a hindcast, from a formula rebuilt for another year, which can lack a fitted term). A row without ranges (an older ledger) still uses `sim$covMinMax_spread`. A fitted covariate with no range now stops, naming it; before, it silently stayed on its raw scale.

- A single fitted ELF now predicts with the covariates its fitted parameters name, as several ELFs already did, not the terms of `sim$fireSense_spreadFormula`. A hindcast whose formula had a term the fit did not (e.g. `treedWetland_agb`) stopped with "not found in data objects". Predictions where the formula matched the fit do not change. A fitted coefficient with no covariate still stops, naming it.

# fireSense_spreadPredict 1.2.0

This release lets fire spread be forecast across study areas that contain several fire regions, each with its own fitted model, with predictions blended smoothly across the boundaries between them. Each simulation replicate now uses one complete set of fitted values instead of an average of all of them, so the spread of results across replicates reflects the uncertainty in the fit. The fitted year-to-year variation in fire spread is passed on to the fire simulation.

Forecasts now follow the same rules as the fit for young forest and for the types of models the fitting module can produce, and the module stops with a clear message when a fitted model and the forecast data do not match, instead of failing with a cryptic error. It is renamed `fireSense_spreadPredict` (lower-case "s"), so projects need to update the name they use for it.

# fireSense_spreadPredict 1.1.4

- `lowerSpreadProb` and `maxFireSpread` default to `fireSenseUtils::spreadProbFloor` (0.13) and `fireSenseUtils::spreadProbCeiling` (0.276; `maxFireSpread` was 0.28), the one floor and ceiling `fireSense_spreadFit` and the objective use. `maxFireSpread` does not change a prediction. Needs the `fireSenseUtils` change in PredictiveEcology/fireSenseUtils (floor to be set once it has a version).

# fireSense_spreadPredict 1.1.3

- A ledger row from a fit with an intercept (a parameter named `fireSenseUtils::spreadInterceptTxt`, from `fireSense_dataPrepFit`'s `spreadIntercept`) is predicted with it: `spreadProbOneELF()` adds a column of 1s for it after rescaling and centring, and subtracts the row's stored covariate centres (`fireSenseUtils::spreadFitCovCentreTxt`, `ledgerCovCentre()`) from the rescaled covariates, as the fit did. With several ELFs each row uses its own. A row without them, such as every row written so far, predicts exactly as before. Needs fireSenseUtils >= 0.2.3.9083 (PredictiveEcology/fireSenseUtils#131).

- Predictions no longer average parameter sets (`spreadProbOneELF()` took `rowMeans()` of one map per row, and `yearSpreadSDOf()` the mean `yearSpreadSD`, which broke the pairing of each set's `yearSpreadSD` with its own coefficients). New parameter `.rep` (integer, default 1): replicate `.rep` uses the one whole parameter set `((rep - 1) %% number of sets) + 1` of each ELF, and `fireSense_SpreadSD` is that set's `yearSpreadSD`. `.rep` is a SpaDES-aware parameter: `SpaDES.project::setupProject()` sets `.globals$.rep` from the experiment's `.rep` (PredictiveEcology/SpaDES.project#190), so nothing needs wiring by hand.

- reqdPkgs now lists `data.table`, which the module calls (`copy`, `data.table`, `setDT`) but did not list. Version 1.1.2.

- `spreadProbOneELF()` (fireSense_spreadPredict.R:266-323 pre-fix) went on when a fitted coefficient had no covariate column, and died in `rowMeans(spreadProbMat)` with "'x' must be an array of at least two dimensions" (a predict-only run whose fit had `nfLCC_100_60` and `nfLCC_40_50_80` but whose covariates had a single `nf`). It now stops at once, naming the coefficients without a covariate and the covariates available, and says the non-forest groups / fuel classes differ from the fit's. Version 1.1.1.
- Renamed from `fireSense_SpreadPredict` to `fireSense_spreadPredict` (module naming convention `<model>_<camelCaseComponent>`); projects must rename the module and its `params` key. Version 1.1.0.

- `spreadProbOneELF()` (fireSense_SpreadPredict.R:282 pre-fix) called
  `fireSenseUtils::spreadProbFromIntegerCovs()` with `mutuallyExclusive = NULL`, so a young pixel's
  fuel biomass and non-forest land-cover columns reached the logistic unchanged instead of being
  zeroed with `youngAge`, as the fit requires. Prediction now derives the same
  `youngAge`-exclusivity rule the fit uses, via the new `fireSenseUtils::youngAgeExclusiveCols()`.
  Requires `fireSenseUtils@development (>= 0.2.3.9048)`. Version 1.0.0.9006.
- New parameter `.studyAreaName` (default `NA`), the name PredictiveEcology modules use for the study area. This module does not use it yet.
- The fitted per-year random effect (`yearSpreadSD`, fireSense_SpreadFit / fireSenseUtils >= 0.2.3.9041) is no
  longer read as a covariate coefficient: with it, a single ELF treated it as a fourth logistic parameter and several
  ELFs stopped with "'yearSpreadSD' not found". It becomes the new output `fireSense_SpreadSD`, which fireSense
  scales one draw per year by: one number with one ELF (the mean over retained parameter sets), a raster blended
  with the spread probabilities' weights with several. 0 when the fit has none.

- Several fitted ELFs in one study area. Every pixel gets a spread probability: each ELF's model (its parameter sets, its `covMinMax_spread` and only the covariates it was fitted with, from its ledger row) predicts its own pixels and those within `ELFblendWidth` (default 20 km) of them, and overlapping predictions are averaged with weights falling linearly from 1 inside an ELF to 0 at `ELFblendWidth` outside it. Each pixel's ELF comes from the new input `rasterToMatchLargeELF` (fireSense_ELFs with a `studyAreaLarge`). One ELF works as before.
- A fit made with fireSense_SpreadFit's `link = "logistic3pUpper"` stores `upperTail1`; prediction uses the upper-tail link for it, chosen by the parameter's name (`fireSenseUtils::logisticAll()`). Needs fireSenseUtils >= 0.2.3.9038.

# fireSense_SpreadPredict 1.0.0

First release from `development` since `master` was last updated (2021-01-27). Full history: https://github.com/PredictiveEcology/fireSense_SpreadPredict/compare/a5b41f9...v1.0.0

## Breaking changes

- Removed input `dataFireSense_SpreadPredict` (RasterLayer, RasterStack).
- Removed output `spreadPredictedProbability` (list).
- Output `fireSense_SpreadPredicted` is now `SpatRaster` (was `RasterLayer, RasterStack`).
- Removed parameters: `data`, `mapping`, `modelObjName`, `typesOfFuel`.

## New features

- New inputs: `covMinMax_spread`, `fireSense_SpreadCovariates`, `flammableRTM`.
- New parameters: `maxFireSpread`, `mutuallyExclusiveCols`.

## Dependencies

- No longer depends on `raster`.
- Now depends on `terra`.

## Testing

- testthat suite and CI (`testthat-module`), including a snapshot of the module's inputs, outputs and parameters in `tests/testthat/test-metadata.R`.
