AC_INIT([rjags], [4])

if test -z "${R_HOME}"; then
   AC_MSG_ERROR("R_HOME is not defined")
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

# Guess the installation prefix from location of the jags executable

AC_PREFIX_PROGRAM(jags)

# For installations in a non-standard location, set the prefix
# manually, using either option --with-jags-prefix or environment
# variable JAGS_PREFIX. Otherwise we accept the guess from
# AC_PREFIX_PROGRAM.

AC_ARG_WITH([jags-prefix],
            AC_HELP_STRING([--with-jags-prefix=PREFIX_PATH],
                           [the prefix used when installing JAGS]),
            [jags_prefix_path=$withval])
if test [ -n "$jags_prefix_path" ] ; then
   JAGS_PREFIX=${jags_prefix_path}
else
   if test [ -z "${JAGS_PREFIX}" ] ; then
      if test [ "$prefix" = "NONE" ]; then
         AC_MSG_ERROR("Cannot find JAGS installation. See the file README for help.")
      else
         JAGS_PREFIX=${prefix}
      fi
   fi
fi

# Set include path with option --with-jags-includedir or environment
# variable JAGS_INCLUDEDIR. By default, this is determined from
# JAGS_PREFIX.

AC_ARG_WITH([jags-includedir],
            AC_HELP_STRING([--with-jags-includedir=INCLUDE_PATH],
                           [the location of JAGS header files]),
            [jags_include_path=$withval])
if test [ -n "$jags_include_path" ] ; then
   JAGS_INCLUDEDIR=${jags_include_path}
else
   if test [ -z "${JAGS_INCLUDEDIR}" ] ; then
      JAGS_INCLUDEDIR=${JAGS_PREFIX}/include
   fi
fi
JAGS_PKGINCLUDEDIR=${JAGS_INCLUDEDIR}/JAGS

AC_LANG_PUSH(C++)
OCPPFLAGS=${CPPFLAGS}
CPPFLAGS=-I${JAGS_PKGINCLUDEDIR}
AC_CHECK_HEADER(Console.h, [],
             [AC_MSG_FAILURE("Cannot find header file ${JAGS_PKGINCLUDEDIR}/Console.h ")])
CPPFLAGS=${OCPPFLAGS}
AC_LANG_POP()


# There is no consensus on where 64- versus 32-bit libraries should be
# installed.  We take a hint from R, assuming that if R is installed
# in, say, /usr/LIBnn/R then we can find the jags library in
# ${JAGS_PREFIX}/LIBnn.
#
# This is a reasonable assumption if both JAGS and R have been
# properly packaged, but may not be true if either has been manually
# installed. The user can set the environment variable LIBnn to
# override this.

if test -z "${LIBnn}"; then 
   LIBnn=`"${R_HOME}/bin/R" CMD config LIBnn`
fi

# Set optional library path with option --with-jags-libdir or
# environment variable JAGS_LIBDIR. By default this is determined by
# JAGS_PREFIX and LIBnn.

AC_ARG_WITH([jags-libdir],
            AC_HELP_STRING([--with-jags-libdir=LIB_PATH],
                           [the location of the JAGS library]),
            [jags_lib_path=$withval])
if test [ -n "$jags_lib_path" ] ; then
   JAGS_LIBDIR=$jags_lib_path
else
   if test [ -z "${JAGS_LIBDIR}" ] ; then
      JAGS_LIBDIR=${JAGS_PREFIX}/${LIBnn}
   fi
fi


# Set location of modules

JAGS_MODDIR=${JAGS_LIBDIR}/JAGS/modules-${PACKAGE_VERSION}

# Support for private installations of JAGS. If the jags library is not
# on the linker path then it helps to hard-code the location of JAGS into
# the DSO for rjags. Suggested by Doug Bates, April 2011.

AC_ARG_ENABLE([rpath],
            AC_HELP_STRING([--enable-rpath],
                           [Hard link the location of the JAGS library]))
if test [ "x$enable_rpath" = "xyes"]; then
   JAGS_RPATH="-Wl,-rpath ${JAGS_LIBDIR}"
fi

OLDFLAGS=${LDFLAGS}
AC_LANG_PUSH(C)
LDFLAGS="-L${JAGS_LIBDIR}"
AC_CHECK_LIB(jags, jags_version, [],
             [AC_MSG_ERROR("Cannot load JAGS library in ${JAGS_LIBDIR}")])

AC_LANG_POP()
LDFLAGS=${OLDFLAGS}

AC_SUBST(JAGS_PKGINCLUDEDIR)
AC_SUBST(JAGS_LIBDIR)
AC_SUBST(JAGS_MODDIR)
AC_SUBST(JAGS_RPATH)

AC_OUTPUT(src/Makevars)
AC_OUTPUT(R/unix/zzz.R)
