
#' Box Plot Statistics for "Indices" Tab
#'
#' A dataset containing the summary statistics necessary to make boxplots according to supply, demand,
#' and percent of outside share for horizontal mergers. This allows for examination of the
#' relationship between industry price changes and commonly used merger indices.
#'
#' @format A data frame with 2,303 rows and 10 variables
#' \describe{
#'   \item{Cut_type}{Firm Count, HHI, Delta HHI, UPP, CMCR, Harm 2nd, Party Gap}
#'   \item{Cut_value}{axis units depending on Cut_type}
#'   \item{shareOutThresh}{outside share threshold in percent (20--70)}
#'   \item{Supply}{pooled, bertrand, cournot, auction}
#'   \item{Demand}{pooled, log, logit, aids, ces, linear}
#'   \item{high_wisk}{maximum}
#'   \item{low_wisk}{minimum}
#'   \item{pct25}{25th percentile boxplot line}
#'   \item{pct50}{50th percentile boxplot line}
#'   \item{pct75}{75th percentile boxplot line}
#' }
#' @references \href{https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers}{Taragin and Loudermilk 2019}
"indicboxdata"


#' Number of Monte Carlo Simulations Performed in "Indices" Tab
#'
#' A dataset containing the information necessary to calculate the number of merger
#' simulations used to generate the plots for the "Indices" tab of "Numerical Simulations" for
#' Horizontal Mergers based on the index of interest.
#'
#' @format A data frame with 35 rows and 3 variables
#' \describe{
#'   \item{Cut_type}{Firm Count, HHI, Delta HHI, UPP, CMCR, Harm 2nd, Party Gap}
#'   \item{Cnt}{number of horizontal merger simulations (25,890 -- 184,254)}
#'   \item{shareOutThresh}{outside share threshold in percent (20--70)}
#' }
#' @references \href{https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers}{Taragin and Loudermilk 2019}
"indicboxmktCnt"


#' Box Plot Statistics for "Summary" Tab for Horizontal Mergers
#'
#' A dataset containing the summary statistics necessary to make boxplots according to supply, demand,
#' and percent of outside share for horizontal mergers so as to examine the
#' distribution of outcomes.
#'
#' @format A data frame with 210 rows and 10 variables
#' \describe{
#'   \item{Demand}{log, logit, aids, ces, linear}
#'   \item{Model}{cournot:log, cournot: linear, bertrand:aids, bertrand:logit, bertrand:ces, auction:logit}
#'   \item{Outcome}{post-Merger index of interest (Industry Price Change (\%), Merging Party Price Change (\%), Consumer Harm (\$), Producer Benefit (\$), Net Harm (\$)}
#'   \item{Supply}{bertrand, cournot, auction}
#'   \item{high_wisk}{maximum}
#'   \item{low_wisk}{minimum}
#'   \item{pct25}{25th percentile boxplot line}
#'   \item{pct50}{50th percentile boxplot line}
#'   \item{pct75}{75th percentile boxplot line}
#'   \item{shareOutThresh}{outside share threshold in percent (20--70) }
#' }
#' @references \href{https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers}{Taragin and Loudermilk 2019}
"sumboxdata"


#' Number of Monte Carlo Simulations Performed in "Summary" Tab for Horizontal Mergers
#'
#' A dataset containing the information necessary to calculate the number of merger
#' simulations used to generate the plots for the Summary tab of Numerical Simulations for Horizontal Mergers.
#'
#' @format A data frame with 30 rows and 3 variables
#' \describe{
#'   \item{Outcome}{post-Merger indice of interest (Industry Price Change (\%), Merging Party Price Change (\%), Consumer Harm (\$), Producer Benefit (\$), Net Harm (\$)}
#'   \item{Cnt}{number of horizontal merger simulations}
#'   \item{shareOutThresh}{outside share threshold in percent (20--70)}
#' }
#' @references \href{https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers}{Taragin and Loudermilk 2019}
"sumboxmktCnt"


#' Box Plot Statistics for "Summary" Tab for Tariffs
#'
#' A dataset containing the summary statistics necessary to make boxplots according to supply, demand,
#' and tariff percentage for tariffs so as to examine the
#' distribution of outcomes.
#'
#' @format A data frame with 162 rows and 10 variables
#' \describe{
#'   \item{Demand}{Linear, CES, Logit}
#'   \item{Model}{Cournot:Linear, Bertrand:CES, Bertrand:Logit, Auction2nd:Logit, Bargaining:Logit, Monopolistic Competition:CES, Monopolistic Competition:Logit}
#'   \item{Outcome}{Consumer Harm, Domestic Firm Benefit, Foreign Firm Harm, Industry Price Change, Net Domestic Harm, Net Total Harm, Domestic Firm Price Change, Foreign Firm Price Change}
#'   \item{Supply}{Cournot, Bertrand, Auction2nd, Bargaining, Monopolistic Competition}
#'   \item{high_wisk}{maximum}
#'   \item{low_wisk}{minimum}
#'   \item{pct25}{25th percentile boxplot line}
#'   \item{pct50}{50th percentile boxplot line}
#'   \item{pct75}{75th percentile boxplot line}
#'   \item{tariffThresh}{tariff threshold in percent (10--30)}
#' }
#' @references \href{https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers}{Taragin and Loudermilk 2019}
"sumboxdata_trade"


#' Number of Monte Carlo Simulations Performed in "Summary" Tab for Tariffs
#'
#' A dataset containing the information necessary to calculate the number of tariffs
#' used to generate the plots for the Summary tab of Numerical Simulations for Tariffs.
#'
#' @format A data frame with 24 rows and 3 variables
#' \describe{
#'   \item{Outcome}{Consumer Harm, Domestic Firm Benefit, Foreign Firm Harm, Industry Price Change, Net Domestic Harm, Net Total Harm, Domestic Firm Price Change, Foreign Firm Price Change}
#'   \item{Cnt}{number of tariffs simulated}
#'   \item{tariffThresh}{tariff threshold in percent (10--30)}
#' }
#' @references \href{https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers}{Taragin and Loudermilk 2019}
"sumboxmktCnt_trade"
