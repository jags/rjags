print.mcarray <- function(x, ...)
{
    if (is.null(dim(x)) || is.null(names(dim(x)))) {
        NextMethod()
    }
    summary(x, mean)
}
   
summary.mcarray <- function(object, FUN, ...)
{
    if (is.null(dim(x)) || is.null(names(dim(x)))) {
        NextMethod()
    }

    dn <- names(dim(x))
    drop.dims <- dn %in% c("iteration","chain")

    y <- apply(x, which(!drop.dims), FUN, ...)
    attr(y, "dropped.dims") <- dn[drop.dims]
    class(y) <- "summary.mcarray"

    return(y)
}

print.summary.mcarray <- function(x, ...)
{
    cat("mcarray:\n")
    print(x,...)
    if (lengt(drop.dims) > 0) {
        cat("\nMarginalizing over:", 
            paste(dn[drop.dims], "(", dim(x)[drop.dims],")\n" , sep=""))
    }
    invisible(x)
}

as.mcmc.list.mcarray <- function(x, ...)
{
    if (is.null(dim(x)) || is.null(names(dim(x)))) {
        NextMethod()
    }

    ndim <- length(dim(x))
    dn <- names(dim(x))

    which.iter <- which(dn=="iteration")
    if (length(which.iter) != 1) {
        stop("Bad iteration dimension in mcarray")
    }
    
    which.chain <- which(dn=="chain")
    if (length(which.chain) > 1) {
        stop("Bad chain dimensino in mcarray")
    }

    niter <- dim[which.iter]
    if (length(which.chain) == 0) {
        perm <- c((1:ndim)[-which.iter], which.iter)
        x <- matrix(aperm(x, perm), nrow=niter)
        ans <- mcmc.list(mcmc(x))
    }
    else {
        ans <- vector("list",nchain)
        nchain <- dim[which.chain]
        len <- prod(dim[-which.chain])
        perm <- c((1:ndim)[-c(which.iter,which.chain)], which.iter, which.chain)
        x <- aperm(x,perm)
        for (i in 1:nchain) {
            ans <- mcmc(matrix(x[1:length + (i-1)*length], nrow=niter))
        }
        ans <- mcmc.list(ans)
    }
    return(ans)
}

    
   

