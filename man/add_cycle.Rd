% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_model_utils.R
\name{add_cycle}
\alias{add_cycle}
\title{Add a cycle to a state space model}
\usage{
add_cycle(sys, p, name, lags = NULL)
}
\arguments{
\item{sys}{list with system matrices}

\item{p}{integer with autoregressive order, \code{p <= 2}}

\item{name}{name ob observation equation}

\item{lags}{(optional) number of lags added to state equation, e.g. since
other equations load on them}
}
\value{
The input list \code{sys} with updated matrices.
}
\description{
Add a cycle to a state space model
}
\keyword{internal}
