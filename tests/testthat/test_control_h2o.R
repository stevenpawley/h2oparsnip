library(testthat)
library(rsample)
library(h2o)
library(h2oparsnip)
library(modeldata)
library(recipes)
library(dials)
library(yardstick)
library(tune)


test_that("test for control_h2o options", {
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

  # test defaults
  res <-
    tune_grid_h2o(
      object = spec,
      preprocessor = rec,
      resamples = resamples,
      grid = params,
      metrics = metric_set(rmse),
      control = control_h2o(save_pred = FALSE)
    )
  expect_s3_class(res, "tune_results")
  expect_equal(names(res), c("splits", "id", ".metrics", ".notes"))

  # test for returning predictions for numeric variable
  res <-
    tune_grid_h2o(
      object = spec,
      preprocessor = rec,
      resamples = resamples,
      grid = params,
      metrics = metric_set(rmse),
      control = control_h2o(save_pred = TRUE)
    )
  expect_s3_class(res, "tune_results")
  expect_equal(names(res), c("splits", "id", ".metrics", ".predictions", ".notes"))
  expect_equal(names(res$.predictions[[1]])[1], ".pred")

  # test for storing models
  res <-
    tune_grid_h2o(
      object = spec,
      preprocessor = rec,
      resamples = resamples,
      grid = params,
      metrics = metric_set(rmse),
      control = control_h2o(save_pred = TRUE, save_models = TRUE)
    )
  expect_s3_class(res, "tune_results")
  expect_equal(names(res), c("splits", "id", ".metrics", ".predictions", ".models", ".notes"))

  h2o.shutdown(prompt = FALSE)
})
