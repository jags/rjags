\name{jags.object}
\alias{update.jags}
\alias{coef.jags}
\title{Functions for manipulating jags model objects}
\description{
  A \code{jags} object represents a Bayesian graphical model
  described using the BUGS language. 
}
\usage{
\method{update}{jags}(object, n.iter=1, by, progress.bar, \ldots)
\method{coef}{jags}(object, chain=1, \ldots)
}
\arguments{
  \item{object}{a \code{jags} model object}
  \item{n.iter}{number of iterations to update the model}
  \item{by}{update frequency for progress bar.  See Details}
  \item{chain}{chain number to query}
  \item{progress.bar}{type of progress bar. Possible values are
    \code{"text"}, \code{"gui"}, and \code{"none"}. See Details}
  \item{\ldots}{additional arguments to the call (ignored)}
}
\value{
  These are methods for generic functions applied to \code{jags}
  model objects.

  The \code{update} function updates all samplers associated with the
  model by the given number of iterations. This function modifies the
  original object and invisibly returns \code{NULL}.

  The \code{coef} function returns a list with an entry for each Node
  array that contains an unobserved Node. Elements corresponding to
  observed Nodes or deterministic Nodes are given missing values.
}
\details{
  
  Since MCMC calculations are typically long, a progress bar is
  displayed during the call to \code{update}. The progress bar is
  refreshed every \code{by} iterations. It is suppressed if either
  \code{type="none"} or R is not being run interactively.
   
  Type \code{"text"} is a text progress bar displayed on the R
  console. Type \code{"gui"} is a graphical progress bar, which also
  displays the current iteration number.  The default progress bar type
  is taken from the option \code{jags.pb}, or is type \code{"text"} if
  this is option not set.
  
}
\author{Martyn Plummer}
\keyword{models}
