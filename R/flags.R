##  R package rjags file R/flags.R
##  Copyright (C) 2006-2026 Martyn Plummer and Matt Denwood
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License version
##  2 as published by the Free Software Foundation.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/
##

pkg.compile.flags <- function(type = c("BINCOMPAT", "CPPFLAGS", "CXXFLAGS", "LIBS"), print=type!="BINCOMPAT")
{
  type <- match.arg(type)
  flags <- list(
    "CPPFLAGS" = .Call("get_rjags_cppflags", PACKAGE="rjags"),
    "CXXFLAGS" = .Call("get_rjags_cxxflags", PACKAGE="rjags"),
    "LIBS" = .Call("get_rjags_libs", PACKAGE="rjags")    
  )
  value <- c(flags, "BINCOMPAT"=list(c(list("MAJOR_VERSION"=as.numeric(jags.version()[,1])), flags)))[[type]]
  
  if(isTRUE(print)){
    cat(value)
    invisible(value)
  }else{
    return(value)
  }
}
