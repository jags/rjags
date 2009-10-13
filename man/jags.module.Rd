\name{jags.module}
\alias{load.modules}
\alias{unload.modules}
\title{Dynamically load JAGS modules}
\description{
  A JAGS module is a dynamically loaded library that extends the
  functionality of JAGS. These functions load and unload JAGS modules.
}
\usage{
load.modules(names, path)
unload.modules(names)
}
\arguments{
  \item{names}{a vector of names of the load modules to be loaded}
  \item{path}{the file path to the location of the modules. If omitted,
    the option \code{jags.moddir} is used to locate the modules}
}
\author{Martyn Plummer}
\keyword{interface}

