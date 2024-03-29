% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayes_eval.R
\name{results_state}
\alias{results_state}
\title{MCMC summary statistics for states}
\usage{
results_state(model, HPDIprob, state)
}
\arguments{
\item{model}{The return object of the function \code{fitSSM} from the package \code{KFAS}}

\item{HPDIprob}{probability of highest posterior density interval, the
default is \code{HPDIprob = 0.68}}

\item{state}{An array with the smoothed state}
}
\value{
List of time series.
}
\description{
Computes MCMC summary statistics for each state.
}
\keyword{internal}
