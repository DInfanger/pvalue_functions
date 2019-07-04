*P*-value functions
================

  - [Information](#information)
      - [R-package `pvaluefunctions` has been released on
        CRAN\!](#r-package-pvaluefunctions-has-been-released-on-cran)
      - [Overview](#overview)
      - [Installation and recreation of the graphics in the
        paper](#installation-and-recreation-of-the-graphics-in-the-paper)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Information

This repository contains R scripts to reproduce the graphics from [our
paper](https://doi.org/10.1002/sim.8293):

Infanger D, Schmidt-Trucksäss A. (2019): *P* value functions: An
underused method to present research results and to promote quantitative
reasoning. *Statistics in Medicine,* 1-9. doi: 10.1002/sim.8293.

## R-package `pvaluefunctions` has been released on CRAN\!

As the R package
[`pvaluefunctions`](https://cran.r-project.org/package=pvaluefunctions)
has now been released on CRAN, all new information concerning the
package and updates can be found on my [GitHub page for the
package](https://github.com/DInfanger/pvaluefunctions).

This repository is no longer being supported with updates\! To reproduce
the graphics in the paper, see below.

## Overview

A description of the package and some examples of how to use the package
can be found on the official [GitHub page for the
package](https://github.com/DInfanger/pvaluefunctions).

<!-- This repository contains R functions to create graphics of *p*-value functions, confidence distributions, confidence densities, or the [Surprisal value (S-value)](http://www.umsl.edu/~fraundorfp/egsurpri.html) (Greenland 2019). An R-script to reproduce the plots in the publication is also available. -->

## Installation and recreation of the graphics in the paper

Install the package from CRAN using
`install.packages("pvaluefunctions")` and then load it using
`library(pvaluefunctions)`.

<!-- Download the file `confidence_distributions.R` to your computer. You can either `source()` the function in R or open it, select and run everything. After loading the function, it's ready for use. -->

To reproduce the plots from the publication, download the file
`paper_plots.R` and run it *after* loading the package using
`library(pvaluefunctions)`.

<!-- Alternatively, you can source the files directly from the GitHub repository using the [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html) package: -->

<!-- ## Dependencies -->

<!-- The function depends on the following R packages, which need to be installed beforehand: -->

<!-- * ggplot2 -->

<!-- * scales -->

<!-- * zipfR -->

<!-- Use the command `install.packages(c("ggplot2", "scales", "zipfR"))` in R to install those packages. -->

<!-- ### Important information! -->

<!-- The newest version of [ggplot2 (3.1.1)](https://cran.r-project.org/web/packages/ggplot2/index.html) has a [bug](https://github.com/tidyverse/ggplot2/issues/2978) in `sec_axis` that will lead to the secondary y-axis being labelled wrongly. -->

<!-- It is therefore recommended that you install the developmental version of ggplot2 until the bug has been fixed. You can install the developmental version using the following command (after installing the [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html) package): `devtools::install_github("tidyverse/ggplot2")` -->

<!-- ## Usage -->

<!-- There is only one function needed to create the plots: `conf_dist()`. The function has the following arguments: -->

<!-- * `estimate`: Numerical vector containing the estimate(s). -->

<!-- * `n`: Numerical vector containing the sample size(s). Required for correlations, variances, proportions and differences between proportions. Must be equal the number of estimates. -->

<!-- * `df`: Numerical vector containing the degrees of freedom. Required for statistics based on the *t*-distribution (e.g. linear regression) and *t*-tests. Must be equal the number of estimates. -->

<!-- * `stderr`: Numerical vector containing the standard error(s) of the estimate(s). Required for statistics based on the *t*-distribution (e.g. linear regression) and the normal distribution (e.g. logistic regression). Must be equal the number of estimate(s). -->

<!-- * `tstat`: Numerical vector containing the *t*-statistic(s). Required for *t*-tests (means and mean differences). Must be equal the number of estimates.  -->

<!-- * `type`: String indicating the type of the estimate. Must be one of the following: `ttest`, `linreg`, `gammareg`, `general_t`, `logreg`, `poisreg`, `coxreg`, `general_z`, `pearson`, `spearman`, `kendall`, `var`, `prop`, `propdiff`. -->

<!-- * `plot_type`: String indicating the type of plot. Must be one of the following: `cdf` (confidence distribution), `pdf` (confidence density), `p_val` (*p*-value function), `s_val` (Surprisal). For differences between independent proportions, only *p*-value functions and Surprisal value functions are available. -->

<!-- * `n_values` (optional): Integer indicating the number of points that are used to generate the graphics. The higher this number, the higher the computation time and resolution. -->

<!-- * `est_names` (optional): String vector indicating the names of the estimate(s). Must be equal the number of estimates. -->

<!-- * `conf_level` (optional): Numerical vector indicating the confidence level(s). Bust be between 0 and 1. -->

<!-- * `null_values` (optional): Numerical vector indicating the null value(s) in the plot -->

<!-- * `trans` (optional): String indicating the transformation function that will be applied to the estimates and confidence curves. For example: "exp" for an exponential transformation of the log-odds in logistic regression.  -->

<!-- * `alternative`: String indicating if the confidence level(s) are two-sided or one-sided. Must be one of the following: `two_sided`, `one_sided`. -->

<!-- * `log_yaxis`: Logical. Indicating if a portion of the y-axis should be displayed on the logarithmic scale. -->

<!-- * `cut_logyaxis`: Numerical value indicating the threshold below which the y-axis will be displayed logarithmically. Must lie between 0 and 1. -->

<!-- * `xlab` (optional): String indicating the label of the x-axis. -->

<!-- * `xlim` (optional): Optional numerical vector of length 2 indicating the limits of the x-axis on the *untransformed* scale. -->

<!-- * `together`: Logical. Indicating if graphics for multiple estimates should be displayed together or on separate plots. -->

<!-- * `plot_p_limit`: Numerical value indicating the lower limit of the y-axis. Must be greater than 0 for a logarithmic scale (i.e. `log_yaxis = TRUE`). -->

<!-- ### Required arguments for different estimate types -->

<!-- * *t*-tests: `estimate`, `df`, `tstat`. -->

<!-- * Linear regression, Gamma regression, general estimates based on the *t*-distribution: `estimate`, `df`, `stderr`. -->

<!-- * Logistic regression, Poisson regression, Cox regression, general estimates based on the normal distribution: `estimate`, `stderr`. -->

<!-- * Correlation coefficients (Pearson, Spearman, Kendall), proportions, difference between proportions, variances: `estimate`, `n`. -->

<!-- ### Returned values -->

<!-- The main function `conf_dist()` returns five objects in a list: -->

<!-- * **`res_frame`**: A data frame containing the values used to construct the plot. -->

<!-- * **`conf_frame`**: A data frame containing the confidence intervals for the specified confidence levels for all estimates. -->

<!-- * **`counternull_frame`**: A data frame containing the counternull values for the specified null values (see Rosenthal & Rubin (1994) for more information about the counternull). -->

<!-- * **`point_est`**: A data frame containing the point estimates for all estimates. The point estimates correspond to the mean, median or mode of the confidence density (see Xie & Singh (2013) for more information). Estimates are produced using numerical procedures: Increase the number of points `n_values` for higher numerical precision. -->

<!-- * **`plot`**: A [ggplot2](https://ggplot2.tidyverse.org/) plot object. -->

<!-- ## Examples -->

<!-- ### Two-sample *t*-test with unequal variances (Welch-Test) -->

<!-- ### Single coefficient from a linear regression model -->

<!-- #### *P*-value function -->

<!-- #### Confidence distribution -->

<!-- ### Multiple coefficients from a linear regression model -->

<!-- #### *P*-value functions -->

<!-- #### Surprisal values -->

<!-- ### Pearson correlation coefficient (one-sided) -->

<!-- ### Odds ratio from logistic regression -->

<!-- ### Proportion -->

<!-- ## References -->

<!-- Bender R, Berg G, Zeeb H. (2005): Tutorial: using confidence curves in medical research. *Biom J.* 47(2): 237-47. -->

<!-- Fraser  D. A. S. (2019): The *p*-value function and statistical inference. *The American Statistician,* 73:sup1, 135-147. -->

<!-- Greenland S (2019): Valid *P*-Values Behave Exactly as They Should: Some Misleading Criticisms of *P*-Values and Their Resolution with *S*-Values. *The American Statistician,* 73sup1, 106-114. -->

<!-- Poole C. (1987a): Beyond the confidence interval. *Am J Public Health.* 77(2): 195-9. -->

<!-- Poole C. (1987b) Confidence intervals exclude nothing. *Am J Public Health.* 77(4): 492-3. -->

<!-- Rosenthal R, Rubin DB. (1994): The counternull value of an effect size: A new statistic. Psychol Sci. 5(6): 329-34. -->

<!-- Schweder T, Hjort NL. (2016): Confidence, likelihood, probability: statistical inference with confidence distributions. New York, NY: Cambridge University Press. -->

<!-- Xie M, Singh K, Strawderman WE. (2011): Confidence Distributions and a Unifying Framework for Meta-Analysis. *J Am Stat Assoc* 106(493): 320-33. doi: 10.1198/jasa.2011.tm09803. -->

<!-- Xie Mg, Singh K. (2013): Confidence distribution, the frequentist distribution estimator of a parameter: A review. *Internat Statist Rev.* 81(1): 3-39. -->

<!-- ## Contact -->

<!-- [Denis Infanger](https://dsbg.unibas.ch/de/personen/denis-infanger/) -->

<!-- ## Session info -->

<!-- ## License -->

<!-- [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) -->

<!-- [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0). -->
