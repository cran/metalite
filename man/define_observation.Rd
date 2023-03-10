% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define.R
\name{define_observation}
\alias{define_observation}
\title{Define analysis observation meta information for ADaM dataset}
\usage{
define_observation(
  meta,
  name,
  id = "USUBJID",
  group = NULL,
  var = NULL,
  subset = NULL,
  label = NULL,
  ...
)
}
\arguments{
\item{meta}{A \code{meta_adam} object.}

\item{name}{A character value of term name.
The term name is used as key to link information.}

\item{id}{A character value of subject identifier variable name
in an ADaM dataset.}

\item{group}{A character vector of group variable names in an ADaM dataset.}

\item{var}{A character vector of useful variable names in an ADaM dataset.}

\item{subset}{An expression to identify analysis records.
See \code{\link[base:subset]{base::subset()}}.}

\item{label}{A character value of analysis label.}

\item{...}{Additional variables.}
}
\value{
A metadata object with observation defined.
}
\description{
Define analysis observation meta information for ADaM dataset
}
\examples{
plan <- plan(
  analysis = "ae_summary", population = "apat",
  observation = c("wk12", "wk24"), parameter = "any;rel;ser"
)

meta_adam(
  population = r2rtf::r2rtf_adsl,
  observation = r2rtf::r2rtf_adae
) |>
  define_plan(plan = plan) |>
  define_observation(
    name = "wk12",
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "Weeks 0 to 12"
  )
}
