library(testthat)
library(parsnip)
library(tibble)
library(magrittr)
library(h2o)


test_that("mlp h2o formula method", {
  skip_on_cran()

  h2o.init(nthreads = 1)
  iris_df <- as_tibble(iris)

  # classfication
  h2o_clf_fitted <-
    h2o.deeplearning(
      x = 1:4,
      y = 5,
      training_frame = as.h2o(iris_df),
      hidden = 100,
      activation = "Rectifier",
      seed = 1234,
      reproducible = TRUE
    )
  h2o_clf_preds <- predict(h2o_clf_fitted, as.h2o(iris_df))
  h2o_clf_preds <- as_tibble(h2o_clf_preds)

  mlp_clf <- mlp(mode = "classification", hidden_units = 100) %>%
    set_engine("h2o", seed = 1234, reproducible = TRUE)

  fitted_clf <- mlp_clf %>% fit(Species ~ ., iris_df)

  clf_preds <- predict(fitted_clf, iris_df)
  clf_probs <- predict(fitted_clf, iris_df, type = "prob")

  expect_equal(clf_preds[[1]], h2o_clf_preds$predict)
  expect_equal(clf_probs[[1]], h2o_clf_preds$setosa)
  expect_equal(clf_probs[[2]], h2o_clf_preds$versicolor)
  expect_equal(clf_probs[[3]], h2o_clf_preds$virginica)

  # regression
  h2o_regr_fitted <-
    h2o::h2o.deeplearning(
      x = 2:5,
      y = 1,
      training_frame = as.h2o(iris_df),
      hidden = 100,
      activation = "Rectifier",
      seed = 1234,
      reproducible = TRUE
    )
  h2o_regr_preds <- predict(h2o_regr_fitted, as.h2o(iris_df))
  h2o_regr_preds <- as_tibble(h2o_regr_preds)

  mlp_regr <- mlp(mode = "regression", hidden_units = 100) %>%
    set_engine("h2o", seed = 1234, reproducible = TRUE)

  fitted_regr <- mlp_regr %>% fit(Sepal.Length ~ ., iris_df)
  regr_preds <- predict(fitted_regr, iris_df)

  expect_equal(h2o_regr_preds$predict, regr_preds$.pred)
})


test_that("mlp h2o non-formula method", {
  skip_on_cran()

  h2o.init(nthreads = 1)
  iris_df <- as_tibble(iris)

  h2o_clf_fitted <-
    h2o::h2o.deeplearning(
      x = 1:4,
      y = 5,
      training_frame = as.h2o(iris_df),
      hidden = 100,
      activation = "Rectifier",
      seed = 1234,
      reproducible = TRUE
    )
  h2o_clf_preds <- predict(h2o_clf_fitted, as.h2o(iris_df))
  h2o_clf_preds <- as_tibble(h2o_clf_preds)

  mlp_clf <- mlp(mode = "classification", hidden_units = 100) %>%
    set_engine("h2o", seed = 1234, reproducible = TRUE)

  fitted_clf <- mlp_clf %>% fit_xy(iris_df[, -5], iris_df$Species)

  clf_preds <- predict(fitted_clf, iris_df)
  clf_probs <- predict(fitted_clf, iris_df, type = "prob")

  expect_equal(clf_preds[[1]], h2o_clf_preds$predict)
  expect_equal(clf_probs[[1]], h2o_clf_preds$setosa)
  expect_equal(clf_probs[[2]], h2o_clf_preds$versicolor)
  expect_equal(clf_probs[[3]], h2o_clf_preds$virginica)
})


test_that("mlp h2o automatic use of activation function with dropout", {
  skip_on_cran()

  h2o.init(nthreads = 1)
  iris_df <- as_tibble(iris)

  # rectifier with dropout
  h2o_clf_fitted <-
    h2o::h2o.deeplearning(
      x = 1:4,
      y = 5,
      training_frame = as.h2o(iris_df),
      hidden = 100,
      activation = "RectifierWithDropout",
      hidden_dropout_ratios = 0.2,
      seed = 1234,
      reproducible = TRUE
    )
  h2o_clf_preds <- predict(h2o_clf_fitted, as.h2o(iris_df))
  h2o_clf_preds <- as_tibble(h2o_clf_preds)

  mlp_clf <-
    mlp(
      mode = "classification",
      hidden_units = 100,
      activation = "relu",
      dropout = 0.2
    ) %>%
    set_engine("h2o", seed = 1234, reproducible = TRUE)

  fitted_clf <- mlp_clf %>% fit_xy(iris_df[, -5], iris_df$Species)
  clf_preds <- predict(fitted_clf, iris_df)

  expect_equal(clf_preds[[1]], h2o_clf_preds$predict)
})


test_that("mlp h2o multi_predict", {
  mlp_clf <-
    mlp(mode = "classification", hidden_units = 100, epochs = 100) %>%
    set_engine("h2o", seed = 1234, reproducible = TRUE)

  fitted_clf <- mlp_clf %>%
    fit(Species ~ ., iris_df)

  expect_true(parsnip::has_multi_predict(fitted_clf))
  expect_equal(parsnip::multi_predict_args(fitted_clf), "epochs")

  expect_equal(
    nrow(multi_predict(fitted_clf, iris_df, type = "class", epochs = 200)),
    150
  )
})


test_that("mlp h2o early stopping", {
  mlp_clf <-
    mlp(mode = "classification", hidden_units = 100, epochs = 100) %>%
    set_engine("h2o",
      seed = 1234, reproducible = TRUE, stopping_rounds = 10,
      validation = 0.1
    )

  fitted_clf <- mlp_clf %>%
    fit(Species ~ ., iris_df)
})
