This file describe the directories present in inst/ct_shiny

Details - mergersNoPurch.R and tradeNoPurch.R calculates the no-purchase shares
for merger simulations and tariffs/quotas, respectively. These shares are displayed
in the Details tabs.

Diagnostics - isOverID.R outputs a note which indicates whether the merger simulation
or tariff/quota run is just- or over-identified based on the inputs. mergersDiag.R
calculates percent differences in inputted and fitted simulation values for prices,
margins, shares, and market elasticity. Several parameters are also outputted, including
the bargaining parameter for supply chain simulations. tradeDiag.R is analagous for 
tariffs/quotas. These figures are all displayed in the Diagnostic tabs.

Inputs - reactiveInputs.R creates reactive values for supply, demand, and market
elasticity so that they are dynamically updated by user choices made on the app. mergersInputs.R
and tradeInputs.R generate the default example dataframe of inputs used for the
horizontal/vertical merger simulations and tariff/quotas, respectively. These default
inputs can be edited by direct user input into the displayed Inputs table on the app.

Output - mergersOutput.R, numericalSimsOutput.R, and tradeOutput.R is the RShiny
code responsible for actually all tables, graphs, and the Summary, Details,
Elasticities, Diagnostics, R Code, and Messages tabs to the user interface (ui.R).

R Code - mergersTemplateCode.R and tradeTemplateCode.R dynamically generates printable
text that displays what a user ought to run in R console in order to generate the same merger
simulation output as what the user manually entered into the app. This code output
is displayed in the R Code tab.

rsconnect/shinyapps.io/paulette2 - files that assist in generating the RShiny app

Simulations - mergersSims.R is where the code that actually runs the horizontal
and vertical merger simulations is stored. It contains extensive conditional logic 
based on the various competitive environments and demand systems that users can select.
The functions that run the merger simulation on the back end are called from the
antitrust package. tradeSims.R is analagous for tariffs/quotas.

Summary - Generates the formatted summary tables that displays summary() calls on
merger simulation objects. Displayed in the Summary tab.

www - Contains a default logo.png and then (for now) eight separate charts depicting
aggregate numerical simulation statistics.






