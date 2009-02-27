update.jags <- function(object, n.iter = 1, by, progress.bar, ...)
{
    if (!is.numeric(n.iter) || n.iter < 1) {
        stop("Invalid n.iter")
    }

    if ("update" in names(object)) {
        ## Old-style jags.model object
        ## The progress bar was created by the object$update() function
        
        if (missing(by))
            by <- floor(n.iter/50)

        object$update(n.iter, by)
    }
    else {
        ## New jags.model object (in version 1.0.3-6)

        on.exit(object$sync())
    
        adapting <- .Call("is_adapting", object$ptr(), PACKAGE="rjags")

        if (missing(progress.bar)) {
            jpb <- getOption("jags.pb")
            if(!is.null(jpb)) {
                progress.bar <- jpb
            }
            else {
                progress.bar <- "text"
            }
        }
        match.arg(progress.bar, c("text","gui","none"))
    
        if (!interactive() || n.iter <100 || progress.bar == "none") {
            ##Suppress progress bar
            ##FIXME: this is not sensitive to user interrupt
            .Call("update", object$ptr(), n.iter, PACKAGE="rjags")
        }
        else {
        
            if (progress.bar=="text") {

                if (missing(by))
                    by <- floor(n.iter/50)
                else {
                    if (by <= 0)
                        stop("by must be positive")
                    by <- ceiling(by)
                }

                pb <- txtProgressBar(object$iter(), object$iter() + n.iter,
                                     style=3, width=50,
                                     char=ifelse(adapting,"+","*"))
                n <- n.iter
                while (n > 0) {
                    .Call("update", object$ptr(), min(n,by), PACKAGE="rjags")
                    n <- n - by
                    setTxtProgressBar(pb, object$iter())
                }
                close(pb)
            }
            else if (progress.bar=="gui") {

                if (missing(by))
                    by <- min(floor(n.iter/50), 100)

                pb <- updatePB(object$iter(), n.iter, adapting)
                n <- n.iter
                while (n > 0) {
                    .Call("update", object$ptr(), min(n,by), PACKAGE="rjags")
                    n <- n - by
                    setPB(pb, object$iter())
                }
            }
            close(pb)
        }

        if (adapting) {
            if (!.Call("adapt_off", object$ptr(), PACKAGE="rjags")) {
                warning("Adaptation incomplete");
            }
        }
    }

    invisible(NULL)
}
    
coef.jags <- function(object, chain = 1, ...) {
    if (!is.numeric(chain) || chain < 1 || chain > object$nchain()) {
        stop("Invalid chain")
    }
    object$state(internal=FALSE)[[chain]]
}

variable.names.jags <- function(object, ...) {
    .Call("get_variable_names", object$ptr(), PACKAGE="rjags")
}
