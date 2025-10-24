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
    if (is.null(dim(x)) || is.null(dimtags(x))) {
        NextMethod()
    }

    cat("mcarray:",
        sprintf("%s", attr(x, "stat")),
        sprintf("%s", attr(x, "summary")), "\n\n")

    print(summary.mcarray(x, mean))

    tags <- dimtags(x)
    drop.dims <- tags %in% c("iteration","chain")
    if (any(drop.dims)) {
        cat("\nMarginalizing over:",
            paste(tags[drop.dims], "(", dim(x)[drop.dims],")" ,
                  sep="", collapse=", "),
            "\n")
    }

    invisible(x)
}

summary.mcarray <- function(object, FUN, ...)
{
    if (is.null(dim(object)) || is.null(dimtags(object))) {
        NextMethod()
    }
    
    tags <- dimtags(object)
    if (length(tags) != length(dim(object))) {
        stop("length mismatch between dimtags and dim")
    }
    apply(object, which(tags == "value"), FUN, ...)
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

dimtags <- function(x)
{
    tags <- attr(x, "dimtags")
    if (is.null(tags)) {
        ## Back-compatibility: In rjags < 5 dimtags were stored as the
        ## names attribute of the dim attribute
        tags <- names(dim(x))
        if (any(nchar(tags)==0)) {
            ## Value dimensions were implicitly represented by empty strings
            tags[nchar(tags) == 0] <- "value"
        }
    }
    return(tags)
}

`dimtags<-` <- function(x, value)
{
    if (is.null(dim(x))) {
        stop("Cannot set dimtags for object with no dims")
    }
    else if (length(dim(x)) != length(value)) {
        stop("Length mismatch between dimtags and dims")
    }
    attr(x, "dimtags") <- value
    return(x)
}

checkdimtags <- function(x)
{
    tags <- dimtags(x)
    if (is.null(tags)) {
        return (FALSE)
    }
    else if (!all(tags %in% c("value", "iteration", "chain"))) {
        return (FALSE)
    }
    return(TRUE)
}

as.mcmc.list.mcarray <- function(x, ...)
{
    if (is.null(dim(x)) || !checkdimtags(x)) {
        NextMethod()
    }

    xdim <- dim(x)
    ndim <- length(xdim)
    tags <- dimtags(x)

    which.val <- which(tags == "value")

    which.iter <- which(tags == "iteration")
    if (length(which.iter) == 0) {
        stop("mcarray has no iteration dimension")
    }
    if (length(which.iter) > 1) {
        stop("Multiple iteration dimensions in mcarray")
    }

    which.chain <- which(tags == "chain")
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

