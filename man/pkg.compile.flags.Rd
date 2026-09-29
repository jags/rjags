\name{pkg.compile.flags}
\alias{pkg.compile.flags}
\title{Get package compile flags}
\description{
  Function to check binary compatibility for R packages implementing JAGS
  modules, as well as retrieving the necessary compile flags to link to the
  same version of JAGS that rjags itself uses.
}
\usage{
pkg.compile.flags(type = c("BINCOMPAT", "CPPFLAGS", "CXXFLAGS", "LIBS"), print=type!="BINCOMPAT")
}
\arguments{
  \item{type}{the compile flag to display (see value)}
  \item{print}{by default the value is printed using cat (for easier use in
  Makevars files) except when the type is BINCOMPAT}
}
\details{
  This function is provided for authors of R packages implementing JAGS modules
  for use within the Makevars(.win/ucrt) files in order to ensure that these
  packages are binary compatible with rjags. For more information see the
  JAGS forum.
}
\value{
  The function prints and returns (invisibly) one of the following character strings,
  depending on the type requested:

  \describe{
    \item{BINCOMPAT}{A list of values that can be tested at run time to verify binary compatibility (composed of the JAGS major version and compile flags below).}
    \item{CPPFLAGS}{The required PKG_CPPFLAGS.}
    \item{CXXFLAGS}{The required PKG_CXXFLAGS (may be empty).}
    \item{LIBS}{The required PKG_LIBS.}
  }
}
\author{Matt Denwood}
\keyword{utilities}
