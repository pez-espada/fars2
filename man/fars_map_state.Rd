% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars2.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Function `fars_map_state`: Plot a map with geolocated observations (cars accidents)}
\usage{
fars_map_state(state_num, year)
}
\description{
This functions takes a valid state_number and a valid year
and produces a map with geolocated observations (accidents)
plotted as dots on the map.
}
\details{
@param state_num valid state_number (between 1 and 51)
 @param year valid year

@return A map with geolocated accidents as dots on the map

@examples
 fars_map_state(51, 2015)
}
