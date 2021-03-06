% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars2.R
\name{fars_read}
\alias{fars_read}
\title{Function `fars_read`: Read file into a dplyr's data frame tbl}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string giving the file's name the function
will read}
}
\value{
This function returns a error message either if the file's name
does not exits or a data frame table representing the data of the file
}
\description{
This function reads a file by firstly  checking that a file with that name
exists and if it does, it converts it into a data frame table
You must use system.file("extdata", "filename", package = "fars2") as the
argument of the function to load the package internal data set
}
\examples{
fars_read(system.file("extdata", "accident_2015.csv.bz2", package = "fars2"))

}
