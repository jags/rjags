AC_INIT([rjags], [5])

if test -z "${R_HOME}"; then
   AC_MSG_ERROR("R_HOME is not defined")
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

# Use pkg-config to set compiler flags

AC_PATH_PROG([PKG_CONFIG], [pkg-config], [], [$PATH:/usr/local/bin])
if test [ -n "${PKG_CONFIG}" ] ; then
   if `${PKG_CONFIG} --exists jags`; then
       if `${PKG_CONFIG} --exists --atleast-version=${PACKAGE_VERSION}.0.0 jags` &&
          `${PKG_CONFIG} --exists --max-version=${PACKAGE_VERSION}.99.99 jags`; then
         JAGS_LIBS=`${PKG_CONFIG} --libs jags`
         JAGS_CFLAGS=`${PKG_CONFIG} --cflags jags`
         JAGS_LIBDIR=`${PKG_CONFIG} --variable libdir jags`
         if test [ -z "${JAGS_LIBS}" -a -z "${JAGS_CFLAGS}" ] ; then
            AC_MSG_ERROR([Cannot get information about jags ${PACKAGE_VERSION} from pkg-config])
         else
            AC_MSG_NOTICE([Setting compile and link flags according to pkg-config])
            AC_MSG_NOTICE([Compile flags are $JAGS_CFLAGS])
            AC_MSG_NOTICE([Link flags are $JAGS_LIBS])
         fi
      else
         jags_installed_version=`${PKG_CONFIG} --modversion jags`
         AC_MSG_ERROR([pkg-config found JAGS ${jags_installed_version} but rjags requires JAGS ${PACKAGE_VERSION}.x.y])
      fi
   else 
      AC_MSG_ERROR([pkg-config file for jags ${PACKAGE_VERSION} unavailable]
                   [Try adding the directory containing 'jags.pc']
                   [to the PKG_CONFIG_PATH environment variable])
   fi
else
   AC_MSG_ERROR([pkg-config is required to configure rjags])
fi

# Support for private installations of JAGS. If the jags library is not
# on the linker path then it helps to hard-code the location of JAGS into
# the DSO for rjags. Suggested by Doug Bates, April 2011.

AC_ARG_ENABLE([rpath],
            AC_HELP_STRING([--enable-rpath],
                           [Hard link the location of the JAGS library]))
if test [ "x$enable_rpath" = "xyes"]; then
   JAGS_RPATH="-Wl,-rpath ${JAGS_LIBDIR}"
fi

# Sanity checks. 
OLDFLAGS=${LDFLAGS}
OCXXFLAGS=${CXXFLAGS}
AC_LANG_PUSH(C++)
CXXFLAGS=${JAGS_CFLAGS}
LDFLAGS="-L${JAGS_LIBDIR} ${JAGS_RPATH}"
# Can we link to the library?
AC_CHECK_LIB(jags, jags_version, [],
             [AC_MSG_ERROR("cannot link to JAGS library in ${JAGS_LIBDIR}.")])
# Does the library have the correct major version number?
AC_MSG_CHECKING([version of JAGS library])
# Need to run this trivial test program first to detect runtime linkage problems
AC_RUN_IFELSE(
   [AC_LANG_SOURCE([
      #include <version.h>
      int main() { return 0; }])],
   [],
   [AC_MSG_RESULT([link error])
    AC_MSG_ERROR([Runtime link error: See the INSTALL file for help])])
# Now we can test
AC_RUN_IFELSE(
   [AC_LANG_SOURCE([
      #include <version.h>
      int main() { return JAGS_MAJOR == ${PACKAGE_VERSION} ? 0 : 1; }])],
   [AC_MSG_RESULT([OK])],
   [AC_MSG_RESULT([wrong version])
    AC_MSG_ERROR([rjags requires JAGS version ${PACKAGE_VERSION}.x.y])])
AC_LANG_POP(C++)
LDFLAGS=${OLDFLAGS}
CXXFLAGS=${OCXXFLAGS}

# Set location of modules

JAGS_MODDIR=${JAGS_LIBDIR}/JAGS/modules-${PACKAGE_VERSION}

AC_SUBST(JAGS_LIBS)
AC_SUBST(JAGS_CFLAGS)
AC_SUBST(JAGS_MODDIR)
AC_SUBST(JAGS_RPATH)

AC_OUTPUT(src/Makevars)
AC_OUTPUT(R/unix/zzz.R)
