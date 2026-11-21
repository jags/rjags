/*
 *  R package rjags file inst/include/rjags.h
 *  Copyright (C) 2006-2026 Martyn Plummer and Matt Denwood
 *
 *  This small header is intended to be included by all R
 *  packages implementing JAGS modules in order to have a
 *  systematic way of indicating that the package needs to
 *  be re-compiled following a change in major version of
 *  the rjags package. The secondary benefit is a slightly
 *  nicer error message in the case that a call to
 *  rjags::pkg.compile.flags() in a Makevars(.win) file
 *  failed due to a missing/incompatible JAGS installation.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  version 2 as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef RJAGS_H_
#define RJAGS_H_

// Provide an informative error message if passing compiler
// flags from rjags::pkg.compile.flags() failed:
#if defined(__has_include)
  #if !__has_include(<version.h>)
    #error "Compiler flag error - check your rjags/JAGS installations"
  #endif
#endif

// This #defines JAGS_MAJOR as a positive integer >= 4, as
// well as JAGS_MINOR and JAGS_PATCH if JAGS_MAJOR >= 5.
// These can be used together with conditional branching 
// to provide compatibility across JAGS_MAJOR versions
// with breaking API changes.
#include <version.h>

#endif /* RJAGS_H_ */
