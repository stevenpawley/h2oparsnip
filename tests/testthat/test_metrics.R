library(testthat)
library(rsample)
library(h2o)
library(h2oparsnip)
library(modeldata)
library(recipes)
library(dials)
library(yardstick)
library(tune)


test_that("test regression metrics", {
  skip_on_cran()
  h2o.init(nthreads = 1)
  data(ames)

  spec <- rand_forest(mode = "regression", mtry = tune(), min_n = tune()) %>%
    set_engine("h2o")

  rec <- ames %>%
    recipe(Sale_Price ~ .)

  params <- list(mtry = mtry(c(5, 10)), min_n = min_n(c(1, 10))) %>%
    grid_regular()

  resamples <- bootstraps(ames, times = 1)

  regression_metrics <- metric_set(rmse, rsq)

  res <-
    tune_grid_h2o(
      object = spec,
      preprocessor = rec,
      resamples = resamples,
      grid = params,
      metrics = regression_metrics,
      control = control_h2o(save_pred = FALSE, save_models = FALSE)
    )
  expect_equal(unique(collect_metrics(res) %>% pull(.metric)), c("rmse", "rsq"))
  h2o.shutdown(prompt = FALSE)
})


test_that("test classification metrics multiclass", {
  skip_on_cran()
  h2o.init(nthreads = 1)
  data(iris)

  spec <- rand_forest(mode = "classification", min_n = tune()) %>%
    set_engine("h2o")

  rec <- iris %>%
    recipe(Species ~ .)

  params <- list(min_n = min_n(c(1, 10))) %>%
    grid_regular()

  resamples <- bootstraps(iris, times = 1)

  clf_metrics <- metric_set(accuracy, mn_log_loss)

  res <-
    tune_grid_h2o(
      object = spec,
      preprocessor = rec,
      resamples = resamples,
      grid = params,
      metrics = clf_metrics,
      control = control_h2o(save_pred = FALSE, save_models = FALSE)
    )
  expect_equal(unique(collect_metrics(res) %>% pull(.metric)), c("accuracy", "mn_log_loss"))

  h2o.shutdown(prompt = FALSE)
})


test_that("test classification metrics binary", {
  skip_on_cran()
  h2o.init(nthreads = 1)
  data(two_class_dat)

  spec <- rand_forest(mode = "classification", min_n = tune()) %>%
    set_engine("h2o")

  rec <- two_class_dat %>%
    recipe(Class ~ .)

  params <- list(min_n = min_n(c(1, 10))) %>%
    grid_regular()

  resamples <- bootstraps(two_class_dat, times = 1)

  clf_metrics <- metric_set(roc_auc, pr_auc)

  res <-
    tune_grid_h2o(
      object = spec,
      preprocessor = rec,
      resamples = resamples,
      grid = params,
      metrics = clf_metrics,
      control = control_h2o(save_pred = FALSE, save_models = FALSE)
    )
  expect_equal(
    unique(collect_metrics(res) %>% pull(.metric)),
    c("pr_auc", "roc_auc")
  )

  h2o.shutdown(prompt = FALSE)
})
