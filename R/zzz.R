.onLoad <- function(libname, pkgname) {
  add_mlp_h2o()
  add_boost_tree_h2o()
  add_rand_forest_h2o()
  add_multinom_reg_h2o()
  add_logistic_reg_h2o()
  add_linear_reg_h2o()
}

