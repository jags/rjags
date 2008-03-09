.onLoad <- function(lib, pkg)
{
  ## Load the rjags wrapper ...
  library.dynam("rjags", pkg, lib, local=FALSE)

  ## ... and the modules
  moddir <- .Call("compile_time_moddir", PACKAGE="rjags")
  if (is.null(getOption("jags.moddir"))) {
      if (is.null(moddir)) {
          stop("Unable to locate module directory")
      }
      options("jags.moddir" = moddir)
  }
  jags.module(c("basemod","bugs"))

  .Call("init_jags_console", PACKAGE="rjags")
}
