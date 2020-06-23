library(testthat)
library(parsnip)
library(tibble)
library(magrittr)
library(h2o)


test_that("model persistence, model spec", {

  skip_on_cran()

  h2o.init(nthreads = 1)
  iris_df <- as_tibble(iris)

  clf <-
    mlp(mode = "classification") %>%
    set_engine("h2o", seed = 1234)

  fitted_clf <- clf %>% fit(Species ~., iris_df)
  preds <- predict(fitted_clf, iris_df)

  # save model
  model_file <- tempfile(fileext = ".mod")
  parsnip_file <- tempfile(fileext = ".rds")

  saveRDS(fitted_clf, parsnip_file)
  write_h2o(fitted_clf, model_file)

  # remove model
  h2o.removeAll()
  remove(fitted_clf)

  # restore model
  restored_clf <- readRDS(parsnip_file)
  restored_clf <- read_h2o(restored_clf, model_file)
  preds_post <- predict(restored_clf, iris_df)

  # compare
  expect_equal(preds, preds_post)
})
