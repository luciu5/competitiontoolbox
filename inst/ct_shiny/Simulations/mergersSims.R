mergersSims <- function(supply, demand = "logit", indata, mktElast = NA_real_,
                        type = c("Horizontal", "Vertical")) {
  type <- match.arg(type)
  spec <- model_spec(type, supply, demand)
  run_model(spec, indata, list(mktElast = mktElast))
}
