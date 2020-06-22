# h2oparsnip

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/h2oparsnip)](https://CRAN.R-project.org/package=h2oparsnip)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test coverage](https://codecov.io/gh/stevenpawley/h2oparsnip/branch/master/graph/badge.svg)](https://codecov.io/gh/stevenpawley/h2oparsnip?branch=master)
<!-- badges: end -->

```h2oparsnip``` provides a set of wrappers to bind h2o algorthms with the
'parsnip' package.

This package is early in development. Currently the following h2o algorithms
are implemented:

- h2o.deeplearning engine added to parsnip::mlp model specification
- h2o.gbm engine added to parsnip::boost_tree model specification
- h2o.randomForest engine added to parsnip::rand_forest model specification
- h2o.glm engine added to multinom_reg, logistic_reg and linear_reg model
specifications
- h2o.naiveBayes engine added to naive_Bayes specification (requires the discrim package)
- a new model, automl

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

An example using the mlp model. This also uses a multi_predict method to tune the number of epochs. In contrast to boost_tree which fits the boosted tree model using the maximum number of trees in the parameter grid, mlp uses h2o's checkpoints to perform warm starts, i.e. it can continue training the model with more epochs. This is still faster than restarting training from scratch when hyperparameter tuning for the number of epochs.

```
mlp_mod <- mlp(mode = "classification", epochs = tune(), hidden_units = tune()) %>%
  set_engine("h2o")
rec <- iris %>%
  recipe(Species ~ .)
grid <- expand.grid(epochs = c(1, 5, 10), hidden_units = c(25, 50, 100))
resamples = mc_cv(iris, times = 1)

tune_results <- tune_grid(
  object = mlp_mod,
  preprocessor = rec,
  resamples = resamples,
  grid = grid,
  metrics = metric_set(accuracy)
)
```
