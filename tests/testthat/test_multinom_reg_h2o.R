library(testthat)
library(parsnip)
library(tibble)
library(magrittr)
library(h2o)

h2o.init(nthreads = 1)
iris_df <- as_tibble(iris)

test_that('multinom_reg h2o formula method', {

  # classfication
  h2o_clf_fitted <-
    h2o.glm(
      x = 1:4,
      y = 5,
      training_frame = as.h2o(iris_df),
      family = "multinomial",
      seed = 1234,
    )
  h2o_clf_preds <- predict(h2o_clf_fitted, as.h2o(iris_df))
  h2o_clf_preds <- as_tibble(h2o_clf_preds)

  clf <- multinom_reg(mode = "classification") %>%
    set_engine("h2o", seed = 1234)

  fitted_clf <- clf %>% fit(Species ~., iris_df)

  clf_preds <- predict(fitted_clf, iris_df)
  clf_probs <- predict(fitted_clf, iris_df, type = "prob")

  expect_equal(clf_preds[[1]], h2o_clf_preds$predict)
  expect_equal(clf_probs[[1]], h2o_clf_preds$setosa)
  expect_equal(clf_probs[[2]], h2o_clf_preds$versicolor)
  expect_equal(clf_probs[[3]], h2o_clf_preds$virginica)

})
