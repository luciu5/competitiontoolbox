tradeSims <- function(supply, demand, indata, mktElast, type = c("Tariffs", "Quotas")) {
  type <- match.arg(type)
  spec <- model_spec(type, supply, demand)
  run_model(spec, indata, list(mktElast = mktElast))
}
