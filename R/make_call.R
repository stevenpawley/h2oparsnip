make_h2o_call <- function(.fn, args, others) {

  # remove args with NULLs
  args <- args[lengths(args) != 0]

  # create unevaluated model call
  model_call <- rlang::call2(.fn = .fn, !!!args, .ns = "h2o")

  # add others if not NULL
  if (length(others) > 0) {
    model_call <- rlang::call_standardise(model_call)
    model_call <- rlang::call_modify(model_call, !!!others)
  }

  rlang::eval_tidy(model_call)
}
