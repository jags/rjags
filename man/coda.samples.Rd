\name{coda.samples}
\alias{coda.samples}
\title{Generate posterior samples in mcmc.list format}
\description{
  This is a wrapper function for \code{jags.samples} which sets a trace
  monitor for all requested nodes, updates the model, and coerces the
  output to a single \code{mcmc.list} object.
}
\usage{
coda.samples(model, variable.names, n.iter, thin = 1)
}
\arguments{
  \item{model}{a jags model object}
  \item{variable.names}{a character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
}
\value{
  An \code{mcmc.list} object.
}
\note{
  If you monitor a subset of a node array, such as \code{Y[26,2:5]} then
  the corresponding variables in the \code{mcmc.list} object returned by
  \code{coda.samples} will have double subscripts, e.g. \code{Y[26,3]}
  will appear as \code{Y[26,2:5][2]}.
}
\author{Martyn Plummer}
\seealso{\code{\link{jags.samples}}, \code{\link[coda]{mcmc.list}}}
\keyword{models}
