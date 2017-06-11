# FatInjUSA

The goal of FatInjUSA is to give insight of the number and location of fatal injuries within the USA.

## Installation

You can install FatInjUSA from github with:


``` r
# install.packages("devtools")
devtools::install_github("FerrieHeskes/Deel3-week2")
```

## Examples

``` r
## basic example code
# To get a map on which the fatal injuries per state (state 10) per year (year 2013) are plotted
fars_map_state(10,2013)

# To get an overview per month per year (years 2013 and 2014) of the total fatal injuries within the USA 
fars_summarize_years(c(2013,2014))
```
