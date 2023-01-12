
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IAV\_Figures: An R project for generating main results in “Global agricultural responses to interannual climate and biophysical variability”

## IAV\_Figures

<!-- badges: start -->
<!-- badges: end -->

The goal of this R project (IAV\_Figures) is to document the process of
generating figures provided in the paper: “Global agricultural responses
to interannual climate and biophysical variability” by Zhao\*, Calvin,
Wise, Patel, Snyder, Waldhoff, Hejazi, and Edmonds. The main figures in
the paper can be reproduced using this R project.

\*Corresponding: `Xin Zhao` &lt;`xin.zhao@pnnl.gov`&gt;

## Paper abstract

Most studies assessing climate impacts on agriculture have focused on
average changes in market-mediated responses (e.g., changes in land use,
production, and consumption). However, the response of global
agricultural markets to interannual variability in climate and
biophysical shocks is poorly understood and not well represented in
global economic models. Here we show a strong transmission of
interannual variations in climate-induced biophysical yield shocks to
agriculture markets, which is further magnified by endogenous market
fluctuations generated due to producers’ imperfect expectations of
market and weather conditions. We demonstrate that the volatility of
crop prices and consumption could be significantly underestimated (i.e.,
on average by 55% and 41%, respectively) by assuming perfect foresight,
a standard assumption in the economic equilibrium modeling, compared
with the relatively more realistic adaptive expectations. We also find
heterogeneity in interannual variability across crops and regions, which
is considerably mediated by international trade. Studying interannual
variability provides fundamentally new insights on measuring and
understanding climate impacts on global agriculture, and our framework
lays the foundation for further investigating the full range of climate
impacts on biophysical and human systems.

## Data

In the data folder, `Fig1_2.RData` is included. This is the data
processed based on raw GCAM outputs. These results are used for studying
future climate impacts on agriculture with a focus the interannual
variation transmission from climate and biophysical shocks to economic
responses.

## Uses

All the scripts can be sourced in order in `Run.R` to generate figures.
Note that for ANOVA results, the code `ANOVA.R` was not sourced
in`Run.R` is commented out as it takes hours (e.g., \~8 hour depending
on many factors) to run.

## R package version for replication

The version of R packages used is provided in the following table. Note
that an older version of dplyr (0.8.5) was used initially, but we update
it to dplyr (1.0.4) to use the nest\_by function to accommodate broom
updates.

| R packages required |                  |               |                 |               |
|---------------------|------------------|---------------|-----------------|---------------|
| `scales_1.1.1`      | `broom_0.7.0`    | `egg_0.4.5`   | `ggplot2_3.3.2` | `dplyr_1.0.4` |
| `gridExtra_2.3`     | `extrafont_0.17` | `tidyr_1.1.2` | `cowplot_1.1.0` | `sf_0.9-6`    |

## Mappings to data and figures in paper

| Figure number in paper          | Location                  |
|---------------------------------|---------------------------|
| `Fig. 2`                        | `output/plot/Fig1`        |
| `Fig. 3; Figs. S3-S5`           | `output/plot/Fig2`        |
| `Fig. 4; Figs. S8-S10`          | `output/plot/Fig3`        |
| `Fig. 5; Figs. S11-13; Fig S15` | `output/plot/Fig4`        |
| `Figs. S17-S18`                 | `output/plot/OtherSIFigs` |
| `Table1 & Table S4`             | `output/plot/ANOVA`       |

## Examples of key results (figures in paper)

![Cumulative (top panels) and interannual (bottom panels) climate
impacts on agricultural crop harvested area (a), production (b),
consumption (c), price (d), export (e), import (f), realized yield (g)
and biophysical yield (h) relative to the GCAM reference scenario,
estimated under adaptive expectations. Note that realized yield are
results from the model after considering endogenous yield responses.
Curves and shadows denote average and 10 – 90 percentile ranges of GCAM
results across all crop-region combinations (biomass and fodder crops
not included), respectively. Climate scenarios (two climate models by
two crop models under RCP8.5 and with carbon fertilization),
distinguished by color, include HadGEM2-ES & EPIC (HE), GFDL-ESM2M& EPIC
(GE), HadGEM2-ES & LPJ-GUESS (HL), and GFDL-ESM2M ES & LPJ-GUESS (GL).
The density bars next to plots of cumulative change show heterogeneity
across crop-region values in 2050 for the four climate scenarios (with
corresponding shadow colors), with the crop-region average in each
scenario (solid black lines) and scenario-average (dotted black line)
highlighted. Interannual impact (bottom panels) is calculated as
logarithmic changes of cumulative impact (top panels). The boxplot next
to plots of interannual impact presents the mean values (points), the
median values (line), the first and third quartiles (boxes), and the 10
– 90 percentile ranges (whiskers) of the standard deviations of
interannual impact (i.e., interannual variability) across GCAM
crop-region combinations. See SI Table S3 for summary statistics and SI
Fig. S7 for sensitivity of interannual variability to the coefficient of
expectation. Data source: GCAM simulation
results](output/plot/Fig1/Fig2.png "Climate impacts on global agriculture to mid-century")

**Fig. 2 Climate impacts on global agriculture to mid-century.**
Cumulative (top panels) and interannual bottom panels) climate impacts
on agricultural crop harvested area (a), production (b), consumption
(c), price (d), export (e), import (f), realized yield (g) and
biophysical yield (h) relative to the GCAM reference scenario, estimated
under adaptive expectations.Note that realized yield are results from
the model after considering endogenous yield responses. Curves and
shadows denote average and 10 – 90 percentile ranges of GCAM results
across all crop-region combinations (biomass and fodder crops not
included), respectively. Climate scenarios (two climate models by two
crop models under RCP8.5 and with carbon fertilization), distinguished
by color, include HadGEM2-ES & EPIC (**HE**), GFDL-ESM2M& EPIC (**GE**),
HadGEM2-ES & LPJ-GUESS (**HL**), and GFDL-ESM2M ES & LPJ-GUESS (**GL**).
The density bars next to plots of cumulative change show heterogeneity
across crop-region values in 2050 for the four climate scenarios (with
corresponding shadow colors), with the crop-region average in each
scenario (solid black lines) and scenario-average (dotted black line)
highlighted. Interannual impact (bottom panels) is calculated as
logarithmic changes of cumulative impact (top panels). The boxplot next
to plots of interannual impact presents the mean values (points), the
median values (line), the first and third quartiles (boxes), and the 10
– 90 percentile ranges (whiskers) of the standard deviations of
interannual impact (i.e., interannual variability) across GCAM
crop-region combinations.

![The beta coefficient and correlation coefficient between economic
variables (distinguished by color) and biophysical yield are presented.
Each point denotes a crop in a region and a climate scenario, and only
crop-regions in 10 – 90 percentile ranges of interannual variability in
a climate scenario are presented. Beta coefficients are truncated to
\[-1, 1\] (see SI Fig. S8 for the figure with full ranges of Beta). Note
that the relative interannual variability between economic variables and
biophysical yield (ratio of standard deviations) is equal to the ratio
of beta coefficient to the correlation coefficient. The slope of the
lines represents the average relative interannual variability between
economic variables and biophysical yield. The black dotted line has a
slope of one. The boxplot attached above presents the mean values
(points), the median values (line), the first and third quartiles
(boxes), and the 10 – 90 percentile ranges (whiskers) of the squares of
correlation coefficient, namely coefficient of determination
(R-squared). Data source: GCAM simulation
results](output/plot/Fig3/Fig3_truncate_.png "Interannual economic responses and correlations to biophysical yield shocks")

**Fig. 4 Interannual economic responses and correlations to biophysical
yield shocks.** The beta coefficient and correlation coefficient between
economic variables (distinguished by color) and biophysical yield are
presented. Each point denotes a crop in a region and a climate scenario,
and only crop-regions in 10 – 90 percentile ranges of interannual
variability in a climate scenario are presented. Beta coefficients are
truncated to \[-1, 1\] (see **SI Fig. S8** for the figure with full
ranges of Beta). Note that the relative interannual variability between
economic variables and biophysical yield (ratio of standard deviations)
is equal to the ratio of beta coefficient to the correlation
coefficient. The slope of the lines represents the average relative
interannual variability between economic variables and biophysical
yield. The black dotted line has a slope of one. The boxplot attached
above presents the mean values (points), the median values (line), the
first and third quartiles (boxes), and the 10 – 90 percentile ranges
(whiskers) of the squares of correlation coefficient, namely coefficient
of determination (R-squared). Data source: GCAM simulation results
