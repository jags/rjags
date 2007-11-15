AC_INIT([rjags], [0.99.1])

dnl Get location of JAGS library and header files from configure option or 
dnl environment variable. Failing that, assume it is installed in ${libdir} 

AC_ARG_WITH([jags],
   AC_HELP_STRING([--with-jags=JAGS_HOME], [the ocation of the JAGS pkgconfig file]), [JAGS_ROOT=$withval])

if ! test -z "${JAGS_ROOT}"; then
   export PKG_CONFIG_PATH=${JAGS_ROOT}:$PKG_CONFIG_PATH
fi

PKG_CHECK_MODULES(JAGS, [jags = $PACKAGE_VERSION])
AC_SUBST(JAGS_CFLAGS)
AC_SUBST(JAGS_LIBS)

JAGS_MODDIR=`$PKG_CONFIG --variable=moduledir jags`
AC_SUBST(JAGS_MODDIR)

AC_OUTPUT(src/Makevars)
AC_OUTPUT(R/zzz.R)
