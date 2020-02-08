# h2oparsnip

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/h2oparsnip)](https://CRAN.R-project.org/package=h2oparsnip)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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

## Installation

The package is not yet on CRAN and can be installed with:

``` r
devtools::install_github("stevenpawley/h2oparsnip")
```
