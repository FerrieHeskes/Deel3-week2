# FatInjUSA

The goal of FatInjUSA is to give insight of the number and location of fatal injuries within the USA.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## To summarize the total number of injuries per month and year within the USA
getwd()
dir()
fars_summarize_years(c(2013,2014))

## To show the location of the fatal injuries per year per state
fars_map_state(10,2013)
```
