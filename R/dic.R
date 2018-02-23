#  R package rjags file R/dic.R
#  Copyright (C) 2009-2013 Martyn Plummer
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

"dic.samples" <-
  function(model, n.iter, thin=1, type="pD", ...)
{
    if (nchain(model) == 1) {
        stop("2 or more parallel chains required")
    }
    if (!inherits(model, "jags"))
      stop("Invalid JAGS model")
    
    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
      stop("n.iter must be a positive integer")
    load.module("dic", quiet=TRUE)
    limits <- vector("list",2)
    pdtype <- match.arg(type, c("pD","popt"))
    status <- .Call("set_monitors", model$ptr(), c("deviance",pdtype),
                    limits, limits, as.integer(thin), "mean", PACKAGE="rjags")
    if (!any(status)) {
      stop("Failed to set monitors")
    }
    
    update(model, n.iter = as.integer(n.iter), ...)
    dev <- .Call("get_monitored_values_flat", model$ptr(), "mean",
                 PACKAGE="rjags")
    for (i in seq(along=dev)) {
        class(dev[[i]]) <- "mcarray"
    }

    if (status[1]) {
        .Call("clear_monitor", model$ptr(), "deviance", NULL, NULL, "mean",
              PACKAGE="rjags")
    }
    if (status[2]) {
        .Call("clear_monitor", model$ptr(), pdtype, NULL, NULL, "mean",
              PACKAGE="rjags")
    }

    ans <- list("deviance" = dev$deviance, "penalty" = dev[[type]],
                "type" = type)
    class(ans) <- "dic"
    return(ans)
}

"print.dic" <- function(x, digits= max(3, getOption("digits") - 3), ...)
{
    deviance <- sum(x$deviance)
    cat("Mean deviance: ", format(deviance, digits=digits), "\n")
    psum <- sum(x[[2]])
    cat(names(x)[[2]], format(mean(psum), digits=digits), "\n")
    cat("Penalized deviance:", format(deviance + psum, digits=digits), "\n")
    invisible(x)
}

"-.dic" <- function(e1, e2)
{
    diffdic(e1, e2)
}
            
"diffdic" <- function(dic1,dic2)
{
    if (!identical(dic1$type, dic2$type)) {
        stop("incompatible dic object: different penalty types")
    }
    n1 <- names(dic1$deviance)
    n2 <- names(dic2$deviance)
    if (!identical(n1, n2)) {

        ### Try matching names in lexicographic order
        if(!identical(sort(n1), sort(n2))) {
            stop("incompatible dic objects: variable names differ")
        }
        ### Reset names to order of the first argument
        ord1 <- order(n1)
        ord2 <- order(n2)
        dic2$deviance[ord1] <- dic2$deviance[ord2]
        dic2$penalty[ord1] <- dic2$penalty[ord2]
    }
    delta <- sapply(dic1$deviance, mean) + sapply(dic1$penalty, mean) -
        sapply(dic2$deviance, mean) - sapply(dic2$penalty, mean)
    class(delta) <- "diffdic"
    return(delta)
}

"print.diffdic" <- function(x, ...)
{
    cat("Difference: ", sum(x), "\n", sep="") 
    cat("Sample standard error: ", sqrt(length(x)) * sd(x), "\n", sep="")
    invisible(x)
}

"waic.samples" <-
  function(model, n.iter, node=NULL, trace=FALSE, thin=1, ...)
{
    if (!inherits(model, "jags"))
      stop("Invalid JAGS model")
    
    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
      stop("n.iter must be a positive integer")

	if(! jags.version() > 4.3 ) {
		stop('This function cannot be used with the version of JAGS on your system: consider updating')
	}

	if(is.null(node)){
		node <- "deviance"
	}else{
		if(is.character(node) && any(node == "deviance") && any(node %in% observed.stochastic.nodes(model))){
			warning("One or more of the provided node names overlaps with those given by 'deviance'")
		}
	}
	
	if(!is.character(node) || length(node)==0)
		stop("node must either be NULL or a character string of length >=1")
	
	if(!is.logical(trace) || length(trace)!=1)
		stop("trace must logical of length 1")
	
	pn <- parse.varnames(node)
	
    load.module("dic", quiet=TRUE)
	
    status <- .Call("set_monitors", model$ptr(), pn$names, pn$lower, pn$upper, 
					as.integer(thin), "density_mean", PACKAGE="rjags")
    if (!any(status)) {
        stop("Failed to set a necessary monitor")
    }

    status <- .Call("set_monitors", model$ptr(), pn$names, pn$lower, pn$upper, 
					as.integer(thin), "logdensity_variance", PACKAGE="rjags")
    if (!any(status)) {
        stop("Failed to set a necessary monitor")
    }
	
	if(trace){
	    status <- .Call("set_monitors", model$ptr(), pn$names, pn$lower, pn$upper, 
						as.integer(thin), "logdensity_trace", PACKAGE="rjags")
	    if (!any(status)) {
	        stop("Failed to set the optional trace monitor")
	    }
	}
    
    update(model, n.iter = as.integer(n.iter), ...)

    density_mean <- .Call("get_monitored_values", model$ptr(), "density_mean", PACKAGE="rjags")
	for(i in seq(along=density_mean)){
		tname <- names(density_mean)[i]
		curdim <- dim(density_mean[[i]])
        class(density_mean[[i]]) <- "mcarray"

		# Ensure dim and dimnames are correctly set:
		if(is.null(curdim)){
			curdim <- c(variable=length(density_mean[[i]]))
			dim(density_mean[[i]]) <- curdim
		}

		# If this is a deviance-type monitor then set the stochastic node names:
		if(tname=='deviance'){
	        attr(density_mean[[i]], "elementnames") <- observed.stochastic.nodes(model)
		# If a partial node array then extract the precise element names:
		}else if(!tname %in% node.names(model)){
			attr(density_mean[[i]], "elementnames") <- expand.varname(tname, dim(density_mean[[i]])[1])
		# Otherwise just set the varname as the whole array:
		}else{
	        attr(density_mean[[i]], "varname") <- tname
		}
		.Call("clear_monitor", model$ptr(), pn$names[i], pn$lower[[i]], pn$upper[[i]], "density_mean", PACKAGE="rjags")    
	}
    logdensity_variance <- .Call("get_monitored_values", model$ptr(), "logdensity_variance", PACKAGE="rjags")
	for(i in seq(along=pn$names)){
		tname <- names(logdensity_variance)[i]
		curdim <- dim(logdensity_variance[[i]])
        class(logdensity_variance[[i]]) <- "mcarray"

		# Ensure dim and dimnames are correctly set:
		if(is.null(curdim)){
			curdim <- c(variable=length(logdensity_variance[[i]]))
			dim(logdensity_variance[[i]]) <- curdim
		}

		# If this is a deviance-type monitor then set the stochastic node names:
		if(tname=='deviance'){
	        attr(logdensity_variance[[i]], "elementnames") <- observed.stochastic.nodes(model)
		# If a partial node array then extract the precise element names:
		}else if(!tname %in% node.names(model)){
			attr(logdensity_variance[[i]], "elementnames") <- expand.varname(tname, dim(logdensity_variance[[i]])[1])
		# Otherwise just set the varname as the whole array:
		}else{
	        attr(logdensity_variance[[i]], "varname") <- tname
		}
		.Call("clear_monitor", model$ptr(), pn$names[i], pn$lower[[i]], pn$upper[[i]], "logdensity_variance", PACKAGE="rjags")    
	}
	
  	raw <- list(density_mean, logdensity_variance)
  	names(raw) <- c('density_mean', 'logdensity_variance')
  
	if(trace){
	    logdensity_trace <- .Call("get_monitored_values", model$ptr(), "logdensity_trace", PACKAGE="rjags")
		for(i in seq(along=pn$names)){
			tname <- names(logdensity_trace)[i]
			curdim <- dim(logdensity_trace[[i]])
	        class(logdensity_trace[[i]]) <- "mcarray"

			# Ensure dim and dimnames are correctly set:
			if(is.null(curdim)){
				curdim <- c(variable=length(logdensity_trace[[i]]))
				dim(logdensity_trace[[i]]) <- curdim
			}

			# If this is a deviance-type monitor then set the stochastic node names:
			if(tname=='deviance'){
		        attr(logdensity_trace[[i]], "elementnames") <- observed.stochastic.nodes(model)
			# If a partial node array then extract the precise element names:
			}else if(!tname %in% node.names(model)){
				attr(logdensity_trace[[i]], "elementnames") <- expand.varname(tname, dim(logdensity_trace[[i]])[1])
			# Otherwise just set the varname as the whole array:
			}else{
		        attr(logdensity_trace[[i]], "varname") <- tname
			}
			.Call("clear_monitor", model$ptr(), pn$names[i], pn$lower[[i]], pn$upper[[i]], "logdensity_trace", PACKAGE="rjags")    
		}
		
		raw <- c(raw, list(logdensity_trace = logdensity_trace))
	}
	
	# Calculation is always done using running mean/variance:
	waictable <- waic.table(density_mean, logdensity_variance)
	ans <- list(waictable=waictable, mcarray=raw)
	class(ans) <- 'JAGSwaic'	
	
    return(ans)
}

waic.table <- function(density_mean, logdensity_variance){
	
	if(missing(density_mean) || missing(logdensity_variance)){
		stop('Missing arguments to density_mean and logdensity_variance are not allowed')
	}

	# Collapse variable lists to single matrix:
	dm_matrix <- do.call('cbind', lapply(density_mean, function(x){
		if('iteration' %in% names(dim(x))){
			stop('iteration numbers detected in the density_mean')
		}
		cdim <- dim(x)
		dim(x) <- c(cdim[-length(cdim)], iteration=1, cdim[length(cdim)])
		return(do.call('rbind', as.mcmc.list(x)))
	}))
	ldv_matrix <- do.call('cbind', lapply(logdensity_variance, function(x){
		if('iteration' %in% names(dim(x))){
			stop('iteration numbers detected in the logdensity_variance')
		}
		cdim <- dim(x)
		dim(x) <- c(cdim[-length(cdim)], iteration=1, cdim[length(cdim)])
		return(do.call('rbind', as.mcmc.list(x)))
	}))

	stopifnot(all(dim(dm_matrix)==dim(ldv_matrix)))
	N <- ncol(dm_matrix)
	result <- lapply(1:nrow(dm_matrix), function(chain){
		lpd <- log(dm_matrix[chain,])
		elpd <- lpd - ldv_matrix[chain,]
		waic <- -2 * elpd
		ans <- c(elpd_waic=sum(elpd), p_waic=sum(ldv_matrix[chain,]), waic=-2*sum(elpd))
	})
	result <- do.call('cbind', result)
	dimnames(result)[[2]] <- paste0('chain', 1:ncol(result))
		
	return(result)
	
}

print.JAGSwaic <- function(x, ...){
	
	print.default(x$waictable, ...)
	
}
