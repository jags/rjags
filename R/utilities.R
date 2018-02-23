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

# Requires JAGS >4.3.0 (i.e. >=4.3.1 or >=4.4.0?)
observed.stochastic.nodes <- function(model){
	if(! jags.version() > 4.3 ) {
		stop('This function cannot be used with the version of JAGS on your system: consider updating')
	}
    if (!inherits(model, "jags")) {
		stop("Invalid JAGS model")
	}
	vars <- .Call("get_obs_stoch_names", model$ptr(), PACKAGE="rjags")
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