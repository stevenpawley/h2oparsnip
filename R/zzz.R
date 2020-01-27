.onLoad <- function(libname, pkgname){
  add_fnn_engine()
  add_h2o_engine()
  add_liquidSVM_engine()

  add_kriging_rk()
}

