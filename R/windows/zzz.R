.onLoad <- function(lib, pkg)
{
  ## Load the rjags wrapper ...
  library.dynam("rjags", pkg, lib, local=FALSE)

  if (!require("SWinRegistry")) {
      stop("On Windows, rjags requires package SWinRegistry from ",
           "http://www.omegahat.org")
  }

  if (is.null(resolveKey("SOFTWARE\\JAGS-1.0.0\\Install_Dir",
                         top="HKEY_LOCAL_MACHINE")))
    {
        stop("Failed to locate JAGS 1.0.0 installation")
    }
  
  jags.home <- getRegistryValue("SOFTWARE\\JAGS-1.0.0",
                                "Install_Dir",
                                top="HKEY_LOCAL_MACHINE")

  bindir <- file.path(jags.home, "bin")
  path <- Sys.getenv("PATH")
  split.path <- strsplit(path, Platform$path.sep)$PATH
  if (!any(split.path == jags.bindir)) {
      path <- paste(path, jags.bindir, sep=.Platform$path.sep)
      Sys.setenv("PATH"=path)
  }
      
  ## The user may overwrite the default module directory with the
  ## option jags.moddir
  
  if (is.null(getOption("jags.moddir"))) {
      options("jags.moddir" = file.path(jags.home, "modules"))
  }
  jags.module(c("basemod","bugs"))

  .Call("init_jags_console", PACKAGE="rjags")
}
