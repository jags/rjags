.onLoad <- function(lib, pkg)
{
    regkey <- try(readRegistry("SOFTWARE\\JAGS\\JAGS-2.0.0", 
                  hive = "HLM", maxdepth = 1), silent = TRUE)
    if (inherits(regkey, "try-error"))
        stop("Failed to locate JAGS 2.0.0 installation")
    jags.home <- regkey[["Install_Dir"]]

    ## Add jags.home to the windows PATH, if not already present

    bindir <- file.path(jags.home, "bin")
    path <- Sys.getenv("PATH")
    split.path <- strsplit(path, .Platform$path.sep)$PATH
    if (!any(split.path == bindir)) {
        path <- paste(bindir, path, sep=.Platform$path.sep)
        Sys.setenv("PATH"=path)
    }
    
    ## Set the module directory, if the option jags.moddir is not already set
    
    if (is.null(getOption("jags.moddir"))) {
        options("jags.moddir" = file.path(jags.home, "modules-2.0.0"))
    }
    jags.module(c("basemod","bugs"))
    
    library.dynam("rjags", pkg, lib, local=FALSE)
    .Call("init_jags_console", PACKAGE="rjags")
}
