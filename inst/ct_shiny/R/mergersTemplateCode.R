mergersTemplateCode <- function(type){
  type <- match.arg(type, c("Horizontal", "Vertical"))

  if (identical(type, "Horizontal")) {
    indata <- values[["inputData"]]
    indata <- indata[!is.na(indata$Name) & indata$Name != "", , drop = FALSE]
    if (!nrow(indata)) return(character())

    ## Keep the generated script independent of the Shiny environment.  The
    ## app's display labels contain newlines, so use stable, newline-free
    ## column names in the standalone data frame.
    cnames <- gsub("\n", "", colnames(indata), fixed = TRUE)
    colnames(indata) <- cnames
    price_col <- grep("^Prices", cnames, ignore.case = TRUE)[1L]
    output_col <- grep("Quantities|Revenues", cnames, ignore.case = TRUE)[1L]
    margin_col <- grep("^Margins", cnames, ignore.case = TRUE)[1L]
    cost_col <- grep("Cost Changes", cnames, ignore.case = TRUE)[1L]

    spec <- model_spec("Horizontal", supply(), demand())
    this_elast <- if (grepl("elast", input$calcElast, ignore.case = TRUE)) {
      as.character(input$enterElast)
    } else {
      "NA_real_"
    }

    ref <- function(name) paste0("simdata$`", name, "`")
    data_code <- paste(capture.output(dput(indata)), collapse = "\n")
    data_code <- paste0("simdata <- ", data_code)

    price_ref <- ref(cnames[price_col])
    output_ref <- ref(cnames[output_col])
    margin_ref <- ref(cnames[margin_col])
    cost_ref <- if (is.na(cost_col)) {
      "rep(0, length(output))"
    } else {
      ref(cnames[cost_col])
    }
    pre_owner_ref <- ref("Pre-merger Owner")
    post_owner_ref <- ref("Post-merger Owner")

    lines <- c(
      "library(antitrust)",
      "",
      "## Load data",
      data_code,
      "",
      paste0("prices <- as.numeric(", price_ref, ")"),
      paste0("output <- as.numeric(", output_ref, ")"),
      paste0("margins <- as.numeric(", margin_ref, ")"),
      paste0("mcDelta <- as.numeric(", cost_ref, ")"),
      paste0("labels <- as.character(", ref("Name"), ")"),
      paste0("ownerPre <- 1 * outer(", pre_owner_ref, ", ", pre_owner_ref, ", \"==\")"),
      paste0("ownerPost <- 1 * outer(", post_owner_ref, ", ", post_owner_ref, ", \"==\")"),
      "missPrices <- anyNA(prices)",
      "sharesQuantity <- output / sum(output, na.rm = TRUE)",
      "shares <- sharesQuantity",
      "insideSize <- sum(output, na.rm = TRUE)",
      "if (!missPrices) {",
      if (spec$demand_id %in% c("ces", "aids")) {
        c(
          "  sharesRevenue <- prices * sharesQuantity / sum(prices * sharesQuantity, na.rm = TRUE)",
          "  shares <- sharesRevenue"
        )
      } else {
        NULL
      },
      if (spec$demand_id %in% c("ces", "aids")) {
        "  insideSize <- sum(prices * output, na.rm = TRUE)"
      } else {
        NULL
      },
      if (identical(spec$supply, "2nd Score Auction")) {
        "  margins <- margins * prices"
      } else {
        NULL
      },
      "}",
      paste0("mktElast <- ", this_elast),
      paste0(
        "upstreamSpec <- antitrust::model_spec(demand = ",
        shQuote(spec$demand_id), ", conduct = ", shQuote(spec$conduct),
        ", variant = ", shQuote(spec$variant), ")"
      )
    )

    if (identical(spec$conduct, "cournot") &&
        spec$demand_id %in% c("linear", "loglin")) {
      lines <- c(lines,
        "prices <- prices[which(!is.na(prices))[1L]]",
        "quantities <- as.matrix(output)",
        "margins <- as.matrix(margins)",
        paste0("labels <- list(as.character(", ref("Name"), "), as.character(",
               ref("Name"), "[1L]))")
      )
    } else if (spec$demand_id %in% c("linear", "loglin")) {
      lines <- c(lines, "quantities <- output")
    }

    homogeneous_cournot <- identical(spec$conduct, "cournot") &&
      spec$demand_id %in% c("linear", "loglin")
    revenue_code <- if (spec$demand_id %in% c("ces", "aids")) {
      "missPrices"
    } else {
      "FALSE"
    }
    market_summary_code <- if (spec$demand_id %in% c("ces", "aids")) {
      c(
        "if (missPrices) {",
        "  details <- NULL",
        "  capture.output(details <- antitrust::summary(simres, revenue = TRUE, levels = FALSE, market = FALSE))",
        "  is_party <- details$isParty == \"*\"",
        "  weighted_change <- function(select) {",
        "    if (!any(select)) return(NA_real_)",
        "    stats::weighted.mean(details$priceDelta[select], details$sharesPost[select], na.rm = TRUE)",
        "  }",
        "  market_summary <- data.frame(",
        "    `Pre-Merger HHI` = as.integer(antitrust::HHI(sharesQuantity, owner = ownerPre)),",
        "    `HHI Change` = as.integer(antitrust::HHI(sharesQuantity, owner = ownerPost) - antitrust::HHI(sharesQuantity, owner = ownerPre)),",
        "    `Industry Price Change (%)` = weighted_change(rep(TRUE, nrow(details))),",
        "    `Merging Party Price Change (%)` = weighted_change(is_party),",
        "    check.names = FALSE",
        "  )",
        "  print(market_summary, row.names = FALSE)",
        "} else {",
        "  antitrust::summary(simres, revenue = FALSE, levels = FALSE, market = TRUE)",
        "}"
      )
    } else {
      "antitrust::summary(simres, revenue = FALSE, levels = FALSE, market = TRUE)"
    }
    fit_args <- c(
      "  demand = upstreamSpec",
      "  prices = prices",
      if (!homogeneous_cournot) "  shares = shares" else NULL,
      "  margins = margins",
      if (spec$demand_id %in% c("linear", "loglin")) "  quantities = quantities" else NULL,
      "  ownerPre = ownerPre",
      if (identical(spec$calibration, "known") || identical(spec$conduct, "cournot")) {
        "  mktElast = mktElast"
      } else {
        NULL
      },
      if (!homogeneous_cournot) "  insideSize = insideSize" else NULL,
      "  labels = labels"
    )
    lines <- c(lines,
      "",
      "## Calibrate the observed market and simulate the merger",
      "fit <- antitrust::calibrate(",
      paste0(fit_args, collapse = ",\n"),
      ")",
      "simres <- antitrust::simulate(fit, ownerPost = ownerPost, mcDelta = mcDelta)",
      "",
      "## Summary tab results",
      market_summary_code,
      "",
      "## Details tab results",
      paste0("antitrust::summary(simres, revenue = ", revenue_code,
             ", levels = FALSE, market = FALSE)"),
      "",
      "## Elasticities tab results (pre-merger only)",
      "antitrust::elast(simres, preMerger = TRUE, market = TRUE)",
      "antitrust::elast(simres, preMerger = TRUE, market = FALSE)",
      "",
      "## Diagnostics tab results",
      "antitrust::calcDiagnostics(simres)"
    )
    return(lines)
  }

  indata <- valuesVertical[["inputData"]]
  indata <- indata[!is.na(indata$Name) & indata$Name != "", , drop = FALSE]
  if (!nrow(indata)) return(character())

  cnames <- gsub("\n", "", colnames(indata), fixed = TRUE)
  colnames(indata) <- cnames
  spec <- model_spec("Vertical", supply(), demand())
  ref <- function(name) paste0("simdata$`", name, "`")
  data_code <- paste(capture.output(dput(indata)), collapse = "\n")
  data_code <- paste0("simdata <- ", data_code)

  lines <- c(
    "library(vertical)",
    "",
    "## Load data",
    data_code,
    "",
    paste0("pricesDown <- as.numeric(", ref("pricesDown"), ")"),
    paste0("sharesDown <- as.numeric(", ref("sharesDown"), ")"),
    paste0("marginsDown <- as.numeric(", ref("marginsDown"), ")"),
    paste0("pricesUp <- as.numeric(", ref("pricesUp"), ")"),
    paste0("marginsUp <- as.numeric(", ref("marginsUp"), ")"),
    paste0("ownerPreDown <- as.character(", ref("ownerPreDown"), ")"),
    paste0("ownerPostDown <- as.character(", ref("ownerPostDown"), ")"),
    paste0("ownerPreUp <- as.character(", ref("ownerPreUp"), ")"),
    paste0("ownerPostUp <- as.character(", ref("ownerPostUp"), ")"),
    paste0("labels <- as.character(", ref("Name"), ")"),
    paste0(
      "verticalSpec <- vertical::model_spec(demand = ",
      shQuote(spec$demand_id), ", conduct = ", shQuote(spec$conduct),
      ", variant = ", shQuote(spec$variant), ")"
    ),
    "",
    "## Calibrate the observed supply chain and simulate the merger",
    "fit <- vertical::calibrate(",
    "  demand = verticalSpec,",
    "  prices = pricesDown,",
    "  shares = sharesDown,",
    "  margins = marginsDown,",
    "  ownerPre = ownerPreDown,",
    "  pricesUp = pricesUp,",
    "  marginsUp = marginsUp,",
    "  ownerPreUp = ownerPreUp,",
    "  priceOutside = 0,",
    "  labels = labels",
    ")",
    "simres_vert <- vertical::simulate(",
    "  fit,",
    "  ownerPost = list(up = ownerPostUp, down = ownerPostDown)",
    ")",
    "",
    "## Summary tab results",
    "vertical::summary(simres_vert, market = TRUE)",
    "",
    "## Details tab results",
    "vertical::summary(simres_vert, market = FALSE)"
  )
  lines
}
