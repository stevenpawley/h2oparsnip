# h2oparsnip

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/h2oparsnip)](https://CRAN.R-project.org/package=h2oparsnip)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**NOTE the development of h2oparsnip is being integrated within tidymodels as the [agua](https://github.com/tidymodels/agua) package. New issues and feature requests should be made in the agua repository**.

```h2oparsnip``` provides a set of wrappers to bind h2o algorthms with the
'parsnip' package.

This package is early in development. Currently the following h2o algorithms
are implemented:

- h2o.deeplearning engine added to parsnip::mlp model specification
- h2o.gbm engine added to parsnip::boost_tree model specification
- h2o.randomForest engine added to parsnip::rand_forest model specification
- h2o.glm engine added to multinom_reg, logistic_reg and linear_reg model
specifications
- h2o.naiveBayes engine added to naive_Bayes specification
- a new model, automl
- h2o.rulefit engine added to parsnip::rule_fit

## Installation

The package is not yet on CRAN and can be installed with:

``` r
devtools::install_github("stevenpawley/h2oparsnip")
```

## Notes

The package currently is based on the concept of using h2o as a disposable backend, using h2o as a drop-in replacement for the traditionally used 'engines' within the parsnip package. However, performing tasks such as hyperparameter tuning via the 'tune' packge will be less efficient if working on a remote cluster than using h2o directly because data is being sent back and forth.

h2oparsnip also does not provide any management of the h2o cluster. If lots of models are being run then available memory within the cluster may be exhausted. Currently this has to be managed using the commands in the h2o package.

## Basic Usage

An example of using the h2o engine for boosted trees. For boosted trees, hyperparameter tuning of the number of trees is performed using a multi_predict method, i.e. the algorithm fits the maximum number of trees in the parameter grid and can be tuned using fewer trees without retraining.

```
library(tidymodels)
library(h2oparsnip)
library(h2o)

h2o.init()

gbm <- boost_tree(mode = "regression", trees = tune(), min_n = tune()) %>%
  set_engine("h2o")

rec <- iris %>%
  recipe(Petal.Length ~ .)

params <- expand.grid(trees = c(10, 20, 30), min_n = c(1, 5, 10))
resamples = mc_cv(iris, times = 1)
tune_results <- tune_grid(
  object = gbm,
  preprocessor = rec,
  resamples = resamples,
  grid = params,
  metrics = metric_set(rmse)
)
```

## Tuning (alternative)

A problem with using `tune::tune_grid` is that performance is reduced because the data for every tuning hyperparameter iteration and resampling is moved from R to the h2o cluster. To minimize this, the `tune_grid_h2o` function can be used to tune model arguments, as a near drop-in replacement:

```
tune_results <- tune_grid_h2o(
  object = gbm,
  preprocessor = rec,
  resamples = resamples,
  grid = params,
  metrics = metric_set(rmse)
)
```

Currently, `tune_grid_h2o` can only tune model parameters and does not handle recipes with tunable parameters. `tune_grid_h2o` moves the data to the h2o cluster only once, i.e. the complete dataset specified by the `resamples` argument is moved to the cluster, and then the equivalent h2o.frame is split based on the row indices in the resampling object, and the `h2o::h2o.grid` function is used for tuning on the h2o frames. To avoid repeatedly moving predictions back from h2o to R, all metrics are also calculated on the cluster. This restricts the range of metrics to what is available in h2o (`tune_grid_h2o` maps **yardstick** metrics to their h2o equivalents). The available metrics are listed in the `tune_grid_h2o` help documentation. However, hyperparameter tuning using `tune_grid_h2o` should be similarly performant as when using h2o directly.

### Control

Similar to `tune::control_grid`, details of `tune_grid_h2o` can be configured using `tune_grid_h2o(control = control_h2o())`. This allows the predictions and/or models to be saved (the default is that they are removed after tuning to avoid clutter in the cluster).
