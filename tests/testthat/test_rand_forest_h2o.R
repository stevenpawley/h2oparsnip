library(testthat)
library(parsnip)
library(tibble)
library(magrittr)
library(h2o)


test_that("boost_tree h2o formula method", {
  skip_on_cran()
  h2o.init(nthreads = 1)
  iris_df <- as_tibble(iris)

  # classfication
  h2o_clf_fitted <-
    h2o.randomForest(
      x = 1:4,
      y = 5,
      training_frame = as.h2o(iris_df),
      ntrees = 50,
      min_rows = 1,
      mtries = -1,
      seed = 1234,
    )
  h2o_clf_preds <- predict(h2o_clf_fitted, as.h2o(iris_df))
  h2o_clf_preds <- as_tibble(h2o_clf_preds)

  clf <-
    rand_forest(
      mode = "classification",
      trees = 50,
      min_n = 1,
      mtry = -1
    ) %>%
    set_engine("h2o", seed = 1234)

  fitted_clf <- clf %>% fit(Species ~ ., iris_df)

  clf_preds <- predict(fitted_clf, iris_df)
  clf_probs <- predict(fitted_clf, iris_df, type = "prob")

  expect_equal(clf_preds[[1]], h2o_clf_preds$predict)
  expect_equal(clf_probs[[1]], h2o_clf_preds$setosa)
  expect_equal(clf_probs[[2]], h2o_clf_preds$versicolor)
  expect_equal(clf_probs[[3]], h2o_clf_preds$virginica)

  # regression
  h2o_regr_fitted <-
    h2o.randomForest(
      x = 2:5,
      y = 1,
      training_frame = as.h2o(iris_df),
      ntrees = 50,
      min_rows = 1,
      mtries = -1,
      seed = 1234
    )
  h2o_regr_preds <- predict(h2o_regr_fitted, as.h2o(iris_df))
  h2o_regr_preds <- as_tibble(h2o_regr_preds)

  regr <- rand_forest(
    mode = "regression",
    trees = 50,
    min_n = 1,
    mtry = -1
  ) %>%
    set_engine("h2o", seed = 1234)

  fitted_regr <- regr %>% fit(Sepal.Length ~ ., iris_df)
  regr_preds <- predict(fitted_regr, iris_df)

  expect_equal(h2o_regr_preds$predict, regr_preds$.pred)
})
