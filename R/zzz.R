.onLoad <- function(lib, pkg)
{
  ## Load the rjags wrapper ...
  library.dynam("rjags", pkg, lib, local=FALSE)

  ## ... and the modules.
  cat("loading JAGS modules\n")
  default.libs <- c("basemod", "bugs")
  for (i in 1:length(default.libs)) {
     cat("   ", default.libs[i], "\n", sep="")
     dyn.load(paste("/usr/local/lib/JAGS/modules/", default.libs[i], 
                    .Platform$dynlib.ext, sep=""))
  }

  .Call("init_jags_console", PACKAGE="rjags")
}
