% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{fars_map_state}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An integer or character string denoting the state number}

\item{year}{An integer or character string denoting a year}
}
\value{
This function shows a graphical representation of the state and the locations within a state where fatal injuries occured
}
\description{
This function gives for a certain year a graphical display of the location of fatal injuries within a particular state based on the data 
of fatal injuries in motor vehicle traffic crashes in the USA.
}
\note{
This function will give a message "invalid STATE number: <state.num>" when the statenumber cannot be found within the data file based on the input of the year and will stop processing
This function will give a message "no accidents to plot" when no fatal injuries took place in a certain state in a certain year
}
\examples{
\dontrun{
fars_map_state(10,2013)
fars_map_state("1",2013)
fars_map_state("1","2013")
}
}
