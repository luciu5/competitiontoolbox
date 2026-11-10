

#'A Shiny Interface to the antitrust, trade, and vertical Packages
#'
#' Launch a shiny interface to simulate the effects of tariffs and mergers
#'
#' @import bslib shiny rhandsontable
#' @importClassesFrom antitrust AntitrustFit
#' @importClassesFrom trade TradeFit
#' @importClassesFrom vertical VerticalFit
#' @details
#'
#' \code{ct_shiny} launches a shiny interface for the \code{antitrust}, \code{trade}, and
#' \code{vertical} packages.
#' The shiny interface provides users with the ability to calibrate model parameters and simulate
#' tariff and quota effects using many of the supply and demand models included in the \code{trade}
#' package. It also provides users with the ability to calibrate different consumer demand systems
#' and simulate the effects of horizontal mergers under different competitive regimes included in the
#' \code{antitrust} package. The \code{vertical} package owns the vertical merger workflow, including
#' its model calibration and simulation.
#' Copy the R Code tab to a local script to reproduce a scenario outside the app.
#' See \code{vignette("gui-to-r-merger", package = "competitiontoolbox")}
#' for a complete walkthrough.
#' @examples
#' if(interactive()){ct_shiny()}
#'
#' @author Charles Taragin, Paulette Wolak
#' @export


ct_shiny <- function() {
  shiny::runApp(system.file('ct_shiny', package='competitiontoolbox'))
}




#'A Link to the Shiny Interface to the antitrust, trade, and vertical Packages
#'
#' Launch a shiny interface to simulate the effects of tariffs and mergers
#'

#' @details
#'
#' \code{trade_shiny} calls \code{ct_shiny}, which is a shiny interface for the \code{antitrust},
#' \code{trade}, and \code{vertical} packages. See \code{\link{ct_shiny}} for further details.
#'
#'
#' @export


trade_shiny <- function() {
  cruft <- ggplot2::is.ggplot(NA)
  ct_shiny()
}


#'A Link to the Shiny Interface to the antitrust, trade, and vertical Packages
#'
#' Launch a shiny interface to simulate the effects of tariffs and mergers
#'

#' @details
#'
#' \code{antitrust_shiny} calls \code{ct_shiny}, which is a shiny interface for the \code{antitrust},
#' \code{trade}, and \code{vertical} packages. See \code{\link{ct_shiny}} for further details.
#'
#'
#' @export


antitrust_shiny <- function() {
  ct_shiny()
}



