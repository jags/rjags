\name{jags.module}
\alias{load.module}
\alias{unload.module}
\title{Dynamically load JAGS modules}
\description{
  A JAGS module is a dynamically loaded library that extends the
  functionality of JAGS. These functions load and unload JAGS modules.
}
\usage{
load.module(name, path)
unload.module(name)
}
\arguments{
  \item{name}{name of the load module to be loaded}
  \item{path}{file path to the location of the module. If omitted,
    the option \code{jags.moddir} is used to locate the modules}
}
\author{Martyn Plummer}
\keyword{interface}

