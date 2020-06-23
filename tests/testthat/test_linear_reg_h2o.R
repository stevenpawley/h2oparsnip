library(testthat)
library(parsnip)
library(tibble)
library(magrittr)
library(h2o)

test_that('linear_reg h2o formula method', {

  skip_on_cran()

  h2o.init(nthreads = 1)
  iris_df <- as_tibble(iris)

  # classfication
  h2o_regr_fitted <-
    h2o.glm(
      x = 2:4,
      y = 1,
      training_frame = as.h2o(iris_df),
      family = "gaussian",
      seed = 1234,
    )
  h2o_regr_preds <- predict(h2o_regr_fitted, as.h2o(iris_df))
  h2o_regr_preds <- as_tibble(h2o_regr_preds)

  regr <- linear_reg(mode = "regression") %>%
    set_engine("h2o", seed = 1234)

  fitted_regr <- regr %>%
    fit(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris_df)

  regr_preds <- predict(fitted_regr, iris_df)

  expect_equal(regr_preds[[1]], h2o_regr_preds$predict)
})
