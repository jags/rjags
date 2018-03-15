#  R package rjags file R/utilities.R
#  Copyright (C) 2006-2018 Martyn Plummer and Matt Denwood
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

# Note: all functions in here are intended to be exported


# Required to check availability of new features but also useful to export:
jags.version <- function(){
	vers <- .Call("get_version", PACKAGE="rjags")	
	return(package_version(vers))	
}

# Not exported yet:
observed.stochastic.nodes <- function(model, dim){
    if (!inherits(model, "jags")) {
		stop("Invalid JAGS model")
	}
	vars <- .Call("get_obs_stoch_names", model$ptr(), PACKAGE="rjags")

	# Currently just a stub unless compiled against JAGS 4.4.0
	# so it is necessary to also supply dim for now:
	if(identical(vars, "deviance")){
		stopifnot(is.numeric(dim) && length(dim)==1)
		vars <- coda.names("deviance", dim)
	}
	return(vars)
}

node.names <- function(model){
    if (!inherits(model, "jags")) {
		stop("Invalid JAGS model")
	}
	vars <- .Call("get_variable_names", model$ptr(), PACKAGE="rjags")
	return(vars)
}

expand.varname <- function(varname, dimensions=NULL){
	
	if(!is.character(varname) || length(varname)!=1){
		stop("varnames must be a character of length 1")
	}
	
	# TODO: implement
	if(is.null(dimensions)){
		# If dimensions aren't provided then attempt to guess:
		dimensions <- c(1)
	}
	
	return(coda.names(varname, dimensions))
	
}