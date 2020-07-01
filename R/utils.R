generate_random_id <- function(prefix) {
  paste(prefix, as.integer(stats::runif(1, 0, 1e9)), sep = "_")
}
