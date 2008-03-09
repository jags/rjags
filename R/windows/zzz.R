.onLoad <- function(lib, pkg)
{

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
  split.path <- strsplit(path, .Platform$path.sep)$PATH
  if (!any(split.path == bindir)) {
      path <- paste(path, bindir, sep=.Platform$path.sep)
      Sys.setenv("PATH"=path)
  }
      
  library.dynam("rjags", pkg, lib, local=FALSE)

  if (is.null(getOption("jags.moddir"))) {
      options("jags.moddir" = file.path(jags.home, "modules"))
  }
  jags.module(c("basemod","bugs"))

  .Call("init_jags_console", PACKAGE="rjags")
}
