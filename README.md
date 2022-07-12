# competitiontoolbox

`competitiontoolbox` is an R package that runs the <em>competitiontoolbox</em> RShiny web application, which is a browser-based user interface for functionality embedded in the [antitrust](https://github.com/cran/antitrust) and [trade](https://github.com/cran/trade) R packages.
<br>
<br>
An accessible public instance of the `competitiontoolbox` web interface is located [**here**](https://daag.shinyapps.io/ct_shiny/).
<br>
<br>

## Why `competitiontoolbox`?

`competitiontoolbox` provides researchers and practicioners with an easy-to-use interactive environment that introduces a variety of merger simulation models encountered in antitrust frameworks. The app provides a useful launching point for users interested in conducting their own merger simulation analyses.

## Features

The app allows users to

* simulate horizontal and vertical mergers, tariffs, and quotas assuming various competitive environments and market demand forms
* input specific market characteristics such as product prices, margins, output, and assumed market elasticity
* numerically simulate thousands of horizontal and supply chain mergers to view distributions of market outcomes
* copy auto-generated R code that runs the inputted simulation

For more details concerning the models used in the app, consult the antitrust package [vignette](https://CRAN.R-project.org/package=antitrust).<br>For more details concerning the numerical simulations included in the app, consult [Taragin and Loudermilk (2019)](https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers) and [Sheu and Taragin (2020)](https://www.researchgate.net/publication/330564874_Simulating_Mergers_in_a_Vertical_Supply_Chain_with_Bargaining).

## Usage
```r
library(competitiontoolbox)
ct_shiny()
```
This will open the `competitiontoolbox` web interface and the user will be greeted by the **Introduction** tab. Users may proceed to the **Mergers**, **Trade**, **Numerical Simulations**, or **Other Resources** tabs. All tabs are self-contained and provide directions on how to proceed.

## Dependencies
`antitrust`<br>
`trade`<br>
`shiny`<br>
`rhandsontable`
