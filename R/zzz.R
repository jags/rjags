.onLoad <- function(lib, pkg)
{
  ## Default location of JAGS is set at build time. The user can override
  ## this by setting the environment variable JAGS_HOME, or by setting
  ## the option "jagshome"

  if (is.null(getOption("jagshome"))) {
     if (nchar(Sys.getenv("JAGS_HOME")) == 0) {
        options("jagshome" = "/usr/local/lib/jags")
     }
     else {
        options("jagshome" = Sys.getenv("JAGS_HOME"))
     }
  }

  ## The linker won't find libjags.so, so we have to dynamically load it
  dyn.load(paste(getOption("jagshome"),"/lib/libjags",
           .Platform$dynlib.ext, sep=""))

  ## Now we can load rjags ...
  library.dynam("rjags", pkg, lib, local=FALSE)

  ## ... and the modules.
  cat("loading JAGS modules\n")
  config.file <- paste(getOption("jagshome"),"etc","modules.conf",sep="/")
  if (file.exists(config.file)) {
     default.libs <- readLines(config.file)
     for (i in 1:length(default.libs)) {
        cat("   ", default.libs[i], "\n", sep="")
        dyn.load(paste(getOption("jagshome"), "/modules/", default.libs[i],
                       .Platform$dynlib.ext, sep=""))
     }
  }

  .Call("init_jags_console", PACKAGE="rjags")
}
