\name{jags.samples}
\alias{jags.samples}
\title{Generate posterior samples}
\description{
  Function to extract random samples from the posterior distribution
  of the parameters of a \code{jags} model. 
}
\usage{
jags.samples(model, variable.names, n.iter, thin = 1,
             stat="value", summary = "trace", force.list=FALSE, ...)
}
\arguments{
  \item{model}{A jags model object}
  \item{variable.names}{A character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
  \item{stat}{character vector of statistics to be monitored}
  \item{summary}{character vector of summaries}
  \item{force.list}{option to consistently return a named list of monitor
    types even if a single monitor type is requested}
  \item{...}{optional arguments passed to the update method for jags
    model objects}
}
\details{
  The \code{jags.samples} function creates monitors for the given
  variables, runs the model for \code{n.iter} iterations and returns
  the monitored samples.
}
\value{
  A recursive list of \code{mcarray} objects is returned.  The top level
  denotes the requested stat, the second level the summary, and the
  third level the monitored variable. Individual \code{mcarray} objects can be
  extrected with \code{samples$stat$summary$variable}. If only a single
  stat/summary/variable combination is monitored then a single
  \code{mcarray} object is returned directly unless the option
  \code{force.list=TRUE} is used.
}
\examples{
  data(LINE)
  LINE$recompile()
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
  n.iter=1000)
  LINE.samples
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
  force.list=TRUE, n.iter=1000)
  LINE.samples
  LINE.samples <- jags.samples(LINE, "alpha",
  n.iter=1000, summary=c("trace","mean"))
  LINE.samples$trace
  LINE.samples$mean
}
\author{Martyn Plummer}
\seealso{\code{\link{jags.model}}, \code{\link{coda.samples}}}
\keyword{models}
