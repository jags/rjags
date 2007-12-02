\name{model.samples}
\alias{model.samples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate posterior samples}
\description{
  Function to extract random samples from the posterior distribution
  of the parameters of a \code{jags} model. 
}
\usage{
model.samples(model, variable.names, n.iter, thin = 1, type="trace")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{a jags model object}
  \item{variable.names}{a character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
  \item{type}{type of monitor}
}
\details{
  The \code{model.samples} function creates monitors for the given
  variables, runs the model for \code{n.iter} iterations and returns
  the monitored samples.
}
\value{
  
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{Martyn Plummer}
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{\code{\link{jags.model}}}
\keyword{models}
