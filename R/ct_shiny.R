
#'A Shiny Interface to the trade and antitrust Packages
#'
#' Launch a shiny interface to simulate the effects of tariffs and mergers
#'

#' @details
#'
#' \code{ct_shiny} launches a shiny interface for the \code{antitrust} and \code{trade} package.
#' The shiny interface provides users with the ability to calibrate model parameters and simulate
#' tariff effects using many of the supply and demand models included in the \code{trade} package. It
#' also provides users with the ability to calibrate different consumer demand systems and simulate the
#' effects of mergers under different competitive regimes included in the \code{antitrust} package.
#'
#'
#' @export


ct_shiny <- function() {
  #requireNamespace("rhandsontable")
  shiny::runApp(system.file('ct_shiny', package='competitiontoolbox'))
}




#'A Link to the Shiny Interface to the trade and antitrust Packages
#'
#' Launch a shiny interface to simulate the effects of tariffs and mergers
#'

#' @details
#'
#' \code{trade_shiny} calls \code{ct_shiny}, which is a shiny interface for the \code{antitrust}
#' and \code{trade} package. See \code{ct_shiny} for further details.
#'
#' @export


trade_shiny <- function() {
  ct_shiny()
}


#'A Link to the Shiny Interface to the trade and antitrust Packages
#'
#' Launch a shiny interface to simulate the effects of tariffs and mergers
#'

#' @details
#'
#' \code{antitrust_shiny} calls \code{ct_shiny}, which is a shiny interface for the \code{antitrust}
#' and \code{trade} package. See \code{ct_shiny} for further details.
#'
#' @export


antitrust_shiny <- function() {
  ct_shiny()
}




# _______________
# trade_shiny + antitrust_shiny are just going to be placeholders that call the
# competition toolbox (within their own packages, the shiny calls will be replaced
# with stop functions and calls to download this package). Will need to deal with
# masking issues.
