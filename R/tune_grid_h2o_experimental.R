# library(tidymodels)
# library(h2oparsnip)
# library(h2o)
# library(yardstick)
# h2o.init()
#
# object <-
#   parsnip::rand_forest(mode = "classification", min_n = tune(), mtry = .cols()) %>%
#   set_engine("h2o", seed = 1234)
# rec_obj <- iris %>% recipe(Species ~.)
# object <- workflow() %>%
#   add_recipe(rec_obj) %>%
#   add_model(object)
# grid <- expand.grid(min_n = c(1, 5, 10))
# resamples <- vfold_cv(iris, v = 2)
# res <- tune_grid_h2o(object, resamples = resamples, grid = grid, metric = "accuracy")
# res$.metrics
# select_best(res, metric = "accuracy")
#
# object <-
#   parsnip::multinom_reg(mode = "classification", penalty = tune(), mixture = tune()) %>%
#   set_engine("h2o", seed = 1234)
# rec_obj <- iris %>% recipe(Species ~.)
# object <- workflow() %>%
#   add_recipe(rec_obj) %>%
#   add_model(object)
# grid <- expand.grid(penalty = c(0.1, 1), mixture = c(0.1, 0.3, 0.5, 0.7, 1.0))
#
# resamples <- vfold_cv(iris, v = 2)
# res <- tune_grid_h2o(object, resamples = resamples, grid = grid, metric = "mn_log_loss")
# res$.metrics
# collect_metrics(res)
# attributes(res)
# select_best(res, metric = "mn_log_loss")
