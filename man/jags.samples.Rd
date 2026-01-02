\name{jags.samples}
\alias{jags.samples}
\title{Generate posterior samples}
\description{
  Function to extract random samples from the posterior distribution
  of the parameters of a \code{jags} model. 
}
\usage{
jags.samples(model, variable.names, n.iter, thin = 1,
             stat="value", summary = "trace", simplify=TRUE,
             force.list=FALSE, ...)
}
\arguments{
  \item{model}{A jags model object}
  \item{variable.names}{A character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
  \item{stat}{character vector of statistics to be monitored}
  \item{summary}{character vector of summaries}
  \item{simplify}{logical flag. If TRUE then the return value will
    be recursively simplified. See Details below.}
  \item{force.list}{logical flag. If TRUE then the return value will
    never be simplified beyond the first two levels. See Details below.}
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
  third level the monitored variable. Individual \code{mcarray} objects
  can be extrected with \code{samples$stat$summary$variable}
  substituting appropriate names for \code{stat}, \code{summary}, and
  \code{variable}.

  If \code{simplify=TRUE} then the list will be recursively simplified
  so that any top level with only one element is suppressed. This is
  most useful when only a single stat/summary/variable combination is
  monitored. In this case \code{simplify=TRUE} returns a single
  \code{mcarray} object. This behaviour can be modified by specifying
  \code{force.list=TRUE}, in which case the return value will never be
  be simplified at the variable level. If only a single
  stat/summary/variable combination is monitored then the return value
  will be a list containing a single \code{mcarray} object.

}
\examples{
  data(LINE)
  LINE$recompile()
  ## With simplify=FALSE the output is a recursive list of depth 3
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
                               n.iter=1000, simplify=FALSE)
  ## Access the value trace monitor for node alpha
  LINE.samples$value$trace$alpha

  ## With simplify=TRUE, the top 2 levels of the list are removed
  ## because all monitors are of stat type "value" and summary type "trace"
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
                               n.iter=1000, simplify=TRUE)
  ## Simpler access to the value trace monitor for alpha
  LINE.samples$alpha

  ## If we have monitors of the same stat type but different summary
  ## types then simplify will only remove the top level of the list
  LINE.samples <- jags.samples(LINE, "alpha",  n.iter=1000,
                               summary=c("mean","var"),
                               simplify=TRUE)
  LINE.samples$mean$alpha
}
\author{Martyn Plummer}
\seealso{\code{\link{jags.model}}, \code{\link{coda.samples}}}
\keyword{models}
