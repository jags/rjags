#  R package rjags file R/mcarray.R
#  Copyright (C) 2007-2009 Martyn Plummer
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License version
#  2 as published by the Free Software Foundation.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

print.mcarray <- function(x, ...)
{
    if (is.null(dim(x)) || is.null(attr(x, "dimtags"))) {
        NextMethod()
    }
    print(summary(x, mean))
}

summary.mcarray <- function(object, FUN, ...)
{
    if (is.null(dim(object)) || is.null(attr(object, "dimtags"))) {
        NextMethod()
    }

    dimtags <- attr(object, "dimtags")
    if (length(dimtags) != length(dim(object))) {
        stop("length mismatch between dimtags and dim")
    }
    drop.dims <- dimtags %in% c("iteration","chain")

    ans <- list("stat" = apply(object, which(!drop.dims), FUN, ...),
                "drop.dims" = dim(object)[drop.dims])
    class(ans) <- "summary.mcarray"

    return(ans)
}

print.summary.mcarray <- function(x, ...)
{
    cat("mcarray:\n")
    print(x$stat,...)
    if (length(x$drop.dims) > 0) {
        cat("\nMarginalizing over:",
            paste(paste(names(x$drop.dims), "(", x$drop.dims,")" , sep=""),
                  collapse=","),"\n")
    }
}

make.coda.names <- function(basename, dim)
{
    if (all(dim == 1)) {
        return(basename)
    }
    else {
        ll <- lapply(as.list(dim), function(n) seq(from=1, to=n))
        elements <- expand.grid(ll)
        elt.names <- apply(elements, 1, paste, collapse=",")
        elt.names <- paste0(basename, "[", elt.names, "]")
        return(elt.names)
    }
}

checkdimtags <- function(tags)
{
    if(is.null(tags)) {
        return (FALSE)
    }
    else if (!all(tags %in% c("value", "iteration", "chain"))) {
        return (FALSE)
    }
    return(TRUE)
    
}

as.mcmc.list.mcarray <- function(x, ...)
{
    if (is.null(dim(x)) || !checkdimtags(attr(x, "dimtags"))) {
        NextMethod()
    }

    xdim <- dim(x)
    ndim <- length(xdim)
    dimtags <- attr(x, "dimtags")

    which.val <- which(dimtags == "value")

    which.iter <- which(dimtags == "iteration")
    if (length(which.iter) == 0) {
        stop("mcarray has no iteration dimension")
    }
    if (length(which.iter) > 1) {
        stop("Multiple iteration dimensions in mcarray")
    }

    which.chain <- which(dimtags == "chain")
    if (length(which.chain) > 1) {
        stop("Multiple chain dimensions in mcarray")
    }

    niter <- xdim[which.iter]
    if (length(which.chain) == 0) {
        perm <- c(which.val, which.iter)
        y <- matrix(aperm(x, perm), nrow=niter, byrow=TRUE)
        ans <- mcmc.list(mcmc(y))
    }
    else {
        nchain <- xdim[which.chain]
        ans <- vector("list", nchain)
        len <- prod(xdim[-which.chain])
        perm <- c(which.val, which.iter, which.chain)
        y <- aperm(x,perm)
        for (i in 1:nchain) {
            ans[[i]] <- mcmc(matrix(y[1:len + (i-1)*len], nrow=niter, byrow=TRUE))
        }
        ans <- mcmc.list(ans)
    }
    
    val.names <- NULL
    if (!is.null(attr(x, 'valuenames', exact=TRUE))) {
        ## If valuenames attribute is set then use this
        val.names <- attr(x, 'valuenames')
        if (length(val.names) != nvar(ans)){
            stop(paste0('The length of the valuenames attr (', length(val.names), ') does not match the number of variables (', nvar(ans), ')'))
        }
    }
    else {
        ## Set default value names based on the varname attribute, if
        ## set. Failing that fall back to "x" as a generic variable
        ## name
        varname <- attr(x, "varname", exact=TRUE)
        if (is.null(varname)) {
            varname <- "x"
        }
        val.names <-  make.coda.names(varname, xdim[which.val])
    }
    
    if (!is.null(val.names)) {
        ## Work around bug in coda::varnames<-
        for (i in 1:nchain) {
            colnames(ans[[i]]) <- val.names
        }
    }
    
    return(ans)
}

