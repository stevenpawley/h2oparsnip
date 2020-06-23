library(testthat)
library(parsnip)
library(tibble)
library(magrittr)
library(h2o)


test_that('logistic_reg h2o formula method', {

  skip_on_cran()

  h2o.init(nthreads = 1)
  iris_df <- as_tibble(iris)
  iris_df <- iris_df[which(iris_df$Species %in% c("setosa", "versicolor")), ]
  iris_df$Species <- droplevels(iris_df$Species)

  # classfication
  h2o_clf_fitted <-
    h2o.glm(
      x = 1:4,
      y = 5,
      training_frame = as.h2o(iris_df),
      family = "binomial",
      seed = 1234,
    )
  h2o_clf_preds <- predict(h2o_clf_fitted, as.h2o(iris_df))
  h2o_clf_preds <- as_tibble(h2o_clf_preds)

  clf <- logistic_reg(mode = "classification") %>%
    set_engine("h2o", seed = 1234)

  fitted_clf <- clf %>% fit(Species ~., iris_df)

  clf_preds <- predict(fitted_clf, iris_df)
  clf_probs <- predict(fitted_clf, iris_df, type = "prob")

  expect_equal(clf_preds[[1]], h2o_clf_preds$predict)
  expect_equal(clf_probs[[1]], h2o_clf_preds$setosa)
  expect_equal(clf_probs[[2]], h2o_clf_preds$versicolor)
})
