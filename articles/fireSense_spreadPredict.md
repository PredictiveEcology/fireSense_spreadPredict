---
title: "fireSense_spreadPredict Manual"
subtitle: "v.1.1.3"
date: "Last updated: 2026-10-05"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
bibliography: citations/references_fireSense_spreadPredict.bib
link-citations: true
always_allow_html: true
pkgdown:
  as_is: true
---

# fireSense_spreadPredict Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:fireSense-spreadPredict) *fireSense_spreadPredict*



[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Eliot McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut, cre], Tati Micheletti <tati.micheletti@gmail.com> [aut], Ian Eddy <ian.eddy@nrcan-rncan.gc.ca> [aut], Jean Marchal <jean.d.marchal@gmail.com> [aut], Alex M. Chubaty <achubaty@for-cast.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Each year, predicts a raster of fire spread probabilities from the parameters fitted by *fireSense_spreadFit*, for the spread component of fireSense [@Marchal:2017a; @Marchal:2017b; @Marchal:2019].

1. The covariates in `fireSense_SpreadCovariates` are rescaled to [0, 1] using `covMinMax_spread`, the range of the fitting data.
2. With the one parameter set (row of `studyAreaWithSpreadParams$params[[i]]`) chosen by `.rep`, i.e. row `((rep - 1) %% number of sets) + 1` of each ELF, the spread probability is a 2- or 3-parameter logistic of the linear combination of the covariates, with lower asymptote `lowerSpreadProb`.
3. `fireSense_SpreadPredicted` is that single set's prediction (sets are not averaged), and `fireSense_SpreadSD` its `yearSpreadSD`, on the `flammableRTM` grid.

### Module inputs and parameters

Two objects from *fireSense_spreadFit* are read from the `simList` though they are not declared as inputs: `studyAreaWithSpreadParams` (the fitted parameters) and `fireSense_spreadFormula` (every term must be a column of `fireSense_SpreadCovariates`).
The module stops if `studyAreaWithSpreadParams` has no parameters.
`maxFireSpread` must have the same value in every module that defines it.

Table \@ref(tab:moduleInputs-fireSense-spreadPredict) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-fireSense-spreadPredict)(\#tab:moduleInputs-fireSense-spreadPredict)List of (ref:fireSense-spreadPredict) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> covMinMax_spread </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Minimum and maximum (2 rows) of each covariate in the fitting data, used to rescale the covariates as in `fireSense_spreadFit`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_SpreadCovariates </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> This year's covariates, from `fireSense_dataPrepPredict`. `pixelID` is the cell index of `flammableRTM`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatchLargeELF </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Only with several fitted ELFs: each pixel's ELF (`ELFind`), on the grid of `flammableRTM`, from `fireSense_ELFs` with a `studyAreaLarge`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flammableRTM </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Binary raster, 1 where the pixel is flammable. Template for `fireSense_SpreadPredicted`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-fireSense-spreadPredict))


<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-fireSense-spreadPredict)(\#tab:moduleParams-fireSense-spreadPredict)List of (ref:fireSense-spreadPredict) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> lowerSpreadProb </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.13 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Lower asymptote of the 2- and 3-parameter logistic. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ELFblendWidth </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 20000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> With several ELFs: each ELF's model also predicts this far (m) outside its own pixels, and where predictions overlap they are averaged with weights that fall linearly from 1 inside the ELF to 0 at this distance outside it. 50/50 at a boundary. The default is the buffer fireSenseUtils::makeELFs() puts around ELFs. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> maxFireSpread </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0.28 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Upper limit on `spreadProb` used when fitting. Here it is only checked to be the same in every module that defines it. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .rep </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Which replicate this prediction is. It selects parameter set ((rep - 1) %% number of sets) + 1 of each ELF, so replicates cycle through the fitted sets, each used whole (with its own `yearSpreadSD`), never averaged. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .runInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Time of the first prediction. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .runInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Interval between predictions, in years. `NA` predicts once. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Time of the `save` event, which does nothing. `NA` means never. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant </td>
  </tr>
</tbody>
</table>

### Events

- `init`: checks `maxFireSpread` against the other modules; schedules `run` at `.runInitialTime`, and `save` at `.saveInitialTime` if that is not `NA`.
- `run`: makes the prediction described above; repeats every `.runInterval`.
- `save`: does nothing, and says so in a message.

The module does not plot anything.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-fireSense-spreadPredict)).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-fireSense-spreadPredict)(\#tab:moduleOutputs-fireSense-spreadPredict)List of (ref:fireSense-spreadPredict) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> fireSense_SpreadPredicted </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Spread probability of each flammable pixel, this year. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_SpreadSD </td>
   <td style="text-align:left;"> SpatRaster&amp;#124;numeric </td>
   <td style="text-align:left;"> The fitted sd of the per-year random effect on logit spread probability (`yearSpreadSD` of the parameter set chosen by `.rep`; 0 if the fit has none), for `fireSense_burn`. One number with one fitted ELF; with several, a raster blended across ELFs with the weights of `fireSense_SpreadPredicted`. </td>
  </tr>
</tbody>
</table>

### Links to other modules

Runs after *fireSense_dataPrepPredict* (covariates) and *fireSense_spreadFit* (parameters). `fireSense_SpreadPredicted` is used by *fireSense_burn* to spread fires.
It is normally run as part of the [fireSense](https://github.com/PredictiveEcology/fireSense) module group.

### Getting help

- <https://github.com/PredictiveEcology/fireSense_spreadPredict/issues>

## References

<!-- autogenerated from bibligraphy -->
