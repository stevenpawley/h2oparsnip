library(testthat)
library(parsnip)
library(tibble)
library(magrittr)
library(h2o)

h2o.init(nthreads = 1)
iris_df <- as_tibble(iris)

test_that('boost_tree h2o formula method', {

  # classfication
  h2o_clf_fitted <-
    h2o.gbm(
      x = 1:4,
      y = 5,
      training_frame = as.h2o(iris_df),
      ntrees = 50,
      max_depth = 10,
      min_rows = 1,
      learn_rate = 0.1,
      sample_rate = 0.8,
      col_sample_rate = 0.3,
      seed = 1234
    )
  h2o_clf_preds <- predict(h2o_clf_fitted, as.h2o(iris_df))
  h2o_clf_preds <- as_tibble(h2o_clf_preds)

  clf <-
    boost_tree(
      mode = "classification",
      trees = 50,
      tree_depth = 10,
      min_n = 1,
      learn_rate = 0.1,
      sample_size = 0.8,
      mtry = 0.3
    ) %>%
    set_engine("h2o", seed = 1234)

  fitted_clf <- clf %>% fit(Species ~., iris_df)

  clf_preds <- predict(fitted_clf, iris_df)
  clf_probs <- predict(fitted_clf, iris_df, type = "prob")

  expect_equal(clf_preds[[1]], h2o_clf_preds$predict)
  expect_equal(clf_probs[[1]], h2o_clf_preds$setosa)
  expect_equal(clf_probs[[2]], h2o_clf_preds$versicolor)
  expect_equal(clf_probs[[3]], h2o_clf_preds$virginica)

  # regression
  h2o_regr_fitted <-
    h2o.gbm(
      x = 2:5,
      y = 1,
      training_frame = as.h2o(iris_df),
      ntrees = 50,
      max_depth = 10,
      min_rows = 1,
      learn_rate = 0.1,
      sample_rate = 0.8,
      col_sample_rate = 0.3,
      seed = 1234
    )
  h2o_regr_preds <- predict(h2o_regr_fitted, as.h2o(iris_df))
  h2o_regr_preds <- as_tibble(h2o_regr_preds)

  regr <- boost_tree(
    mode = "regression",
    trees = 50,
    tree_depth = 10,
    min_n = 1,
    learn_rate = 0.1,
    sample_size = 0.8,
    mtry = 0.3
  ) %>%
    set_engine("h2o", seed = 1234)

  fitted_regr <- regr %>% fit(Sepal.Length ~ ., iris_df)
  regr_preds <- predict(fitted_regr, iris_df)

  expect_equal(h2o_regr_preds$predict, regr_preds$.pred)
})

