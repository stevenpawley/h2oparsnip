library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(h2o)
library(magrittr)
library(parsnip)
#library(parsnipExtra)

h2o.init()
iris_df <- as_tibble(iris)
as.h2o(iris_df)

test_that('mlp h2o engine execution', {
  mlp_model <- mlp(mode = "classification", hidden_units = 100) %>% set_engine("h2o")
  fitted <- mlp_model %>% fit(Species ~., iris_df)
  predict(fitted, iris_df, type = "prob")

  print(mlp_model)
  update(mlp_model)
})

