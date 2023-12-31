% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_tolerance.R
\name{set_tolerance}
\alias{set_tolerance}
\title{Set Tolerance Level for Comparisons}
\usage{
set_tolerance(tolerance = 0)
}
\arguments{
\item{tolerance}{A non-negative numeric value specifying the tolerance level.}
}
\value{
None; this function sets an option and does not return a value.
}
\description{
Defines the tolerance level for numeric comparisons.
This is useful for dealing with floating-point arithmetic issues.
}
\examples{
set_tolerance(0.001)
}
