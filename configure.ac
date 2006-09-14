AC_INIT([rjags], [0.1])

dnl Get location of JAGS library and header files from configure option or 
dnl environment variable. Failing that, assume it is installed in ${libdir} 

AC_ARG_WITH([jags-home],
   AC_HELP_STRING([--with-jags-home=JAGS_HOME_PATH], [the location of JAGS]),
                  [jags_home_path=$withval])
if test [ -n "$jags_home_path" ] ; then
   AC_SUBST([JAGS_HOME],["${jags_home_path}"])
   AC_SUBST([JAGS_MODULES],["${jags_home_path}/modules"])
   AC_SUBST([CPPFLAGS],["-I${jags_home_path}/include ${CPPFLAGS}"])
elif test [ -n "${JAGS_HOME}" ] ; then
   AC_SUBST([JAGS_HOME],["${JAGS_HOME}"])
   AC_SUBST([JAGS_MODULES],["${JAGS_HOME}/modules"])
   AC_SUBST([CPPFLAGS],["-I${JAGS_HOME}/include ${CPPFLAGS}"])
else
   AC_SUBST([JAGS_HOME],["/usr/local/lib/jags"])
   AC_SUBST([JAGS_MODULES],["/usr/local/lib/jags/modules"])
   AC_SUBST([CPPFLAGS],["-I/usr/local/lib/jags/include ${CPPFLAGS}"])
fi

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_SUBST(JAGS_HOME)

AC_OUTPUT(src/Makevars)
AC_OUTPUT(R/zzz.R)
