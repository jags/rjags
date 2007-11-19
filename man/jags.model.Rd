\name{jags.model}
\alias{jags.model}
\title{Create a JAGS model object}
\description{
  \code{jags.model} is used to create an object representing a
  Bayesian graphical model, specified with a BUGS-language description
  of the prior distribution, and a set of data.
}
\usage{jags.model(file, data=sys.frame(sys.parent()), inits=NULL, nchain = 1)}
\arguments{
  \item{file}{a file containing a description of the model in the
    JAGS dialect of the BUGS language}
  \item{data}{a list or environment containing the data. Any numeric
    objects in \code{data} corresponding to node arrays used in
    \code{file} are taken to represent the values of observed nodes
    in the model}
  \item{inits}{an optional list containing initial values.  If supplied,
    this must be a list of length equal to \code{nchain} in which each
    element is itself a list of initial values. It is an error to supply
    an initial value for an observed node.}
  \item{nchain}{the number of parallel chains for the model}
}
\value{
  \code{jags.model} returns an object inheriting from class \code{jags}
  which can be used to generate dependent samples from the posterior
  distribution of the parameters

  An object of class \code{jags} 
}
