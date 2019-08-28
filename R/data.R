
#' Box Plot Statistics for "Indices" Tab
#'
#' A dataset the summary statistics necessary to make boxplots according to supply, demand,
#' and percent of outside share for horizontal mergers in  order to examine the
#' relationship between industry price changes and commmonly used merger indices.
#'
#' @format A data frame with 2,303 rows and 10 variables:
#' \describe{
#'   \item{Cut_type}{Firm Count, HHI, Delta HHI, UPP, CMCR, Harm 2nd, Party Gap}
#'   \item{Cut_value}{Axis units depending on Cut_type}
#'   \item{shareOutThresh}{Outside share threshold in percent (20-70)}
#'   \item{Supply}{Pooled, betrand, cournot, auction }
#'   \item{Demand}{Pooled, log, logit, aids, ces, linear}
#'   \item{high_wisk}{Maximum}
#'   \item{low_wisk}{Minimum}
#'   \item{pct25}{25% percentile boxplot line}
#'   \item{pct50}{50% percentile boxplot line}
#'   \item{pct75}{75% percentile boxplot line}
#' }
"indicboxdata"


#' Data on Number of Monte Carlo Simulations Performed in "Indices" Tab
#'
#' A dataset containing the information necessary to calculate the number of merger
#' simulations used to generate the plots for the "Indices" tab of "Numerical Simulations"
#'
#' @format A data frame with 35 rows and 3 variables:
#' \describe{
#'   \item{Cut_type}{Firm Count, HHI, Delta HHI, UPP, CMCR, Harm 2nd, Party Gap}
#'   \item{Cnt}{Number of horizontal merger simulations (25,890-184,254)}
#'   \item{shareOutThresh}{Outside share threshold in percent (20-70)}
#' }
"indicboxmktCnt"


#' Box Plot Statistics for "Summary" Tab
#'
#' A dataset the summary statistics necessary to make boxplots according to supply, demand,
#' and percent of outside share for horizontal mergers in  order to examine the
#' relationship between industry price changes and commmonly used merger indices.
#'
#' @format A data frame with 180 rows and 10 variables:
#' \describe{
#'   \item{Demand}{log, logit, aids, ces, linear}
#'   \item{Model}{cournot:log, cournot: linear, bertrand:aids, bertrand:logit, bertrand:ces, auction:logit}
#'   \item{Outcome}{Post-Merger indice of interest (Industry Price Change (%), Merging Party Price Change (%),
#'    Consumer Harm ($), Producer Benefit ($), Net Harm ($)}
#'   \item{Supply}{betrand, cournot, auction}
#'   \item{high_wisk}{Maximum}
#'   \item{low_wisk}{Minimum}
#'   \item{pct25}{25% percentile boxplot line}
#'   \item{pct50}{50% percentile boxplot line}
#'   \item{pct75}{75% percentile boxplot line}
#'   \item{shareOutThresh}{Outside share threshold in percent (20-70)}
#' }
"sumboxdata"


#' Data on Number of Monte Carlo Simulations Performed in "Summary"
#'
#' A dataset containing the information necessary to calculate the number of merger
#' simulations used to generate the plots for the "Summary" tab of "Numerical Simulations"
#'
#' @format A data frame with 151 rows and 6 variables:
#' \describe{
#'   \item{Supply}{betrand, cournot, auction}
#'   \item{Demand}{log, logit, aids, ces, linear}
#'   \item{Outcome}{Post-Merger indice of interest (Industry Price Change (%), Merging Party Price Change (%),
#'    Consumer Harm ($), Producer Benefit ($), Net Harm ($)}
#'   \item{Cnt}{Number of horizontal merger simulations}
#'   \item{shareOutThresh}{Outside share threshold in percent (20-70)}
#'   \item{Model}{cournot:log, cournot: linear, bertrand:aids, bertrand:logit, bertrand:ces, auction:logit}
#' }
"sumboxmktCnt"
