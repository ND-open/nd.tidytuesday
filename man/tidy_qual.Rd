% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_qual.R
\name{tidy_qual}
\alias{tidy_qual}
\title{Quality function for dataset using tifyverse functions}
\usage{
tidy_qual(df)
}
\arguments{
\item{df}{a dataframe}
}
\value{
a data frame with the number of NA, NaN and infinite values per variables.
}
\description{
Quality function for dataset using tifyverse functions
}
\examples{
tidy_qual(iris)
}
