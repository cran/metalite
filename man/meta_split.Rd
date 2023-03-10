% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_split.R
\name{meta_split}
\alias{meta_split}
\title{Split metadata into groups}
\usage{
meta_split(meta, by)
}
\arguments{
\item{meta}{A \code{meta_adam} object.}

\item{by}{A character variable name both in
population level and observation level data of a metadata object.}
}
\value{
A metadata object split by the input variable.
}
\description{
Split metadata into groups
}
\examples{
meta_example() |> meta_split("RACE")
}
