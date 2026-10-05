# fireSense_spreadPredict (development version)

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
