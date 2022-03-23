.onLoad <- function(libname, pkgname) {

  # if (any(loadedNamespaces() == "discrim")) {
  #   add_naive_Bayes_h2o()
  # }
  #
  # if (any(loadedNamespaces() == "rules")) {
  #   add_rule_fit_h2o()
  # }

  add_naive_Bayes_h2o()
  add_rule_fit_h2o()

  add_mlp_h2o()
  add_boost_tree_h2o()
  add_rand_forest_h2o()
  add_multinom_reg_h2o()
  add_logistic_reg_h2o()
  add_linear_reg_h2o()
  add_automl()
}

