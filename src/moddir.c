#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

/* 
   This requires a little pre-processor voodoo to stringify the 
   variable JAGS_MODDIR
*/

SEXP compile_time_moddir()
{
#ifdef JAGS_MODDIR
#define stringify(s) #s
#define xstringify(s) stringify(s)
    SEXP dir;
    PROTECT(dir = allocVector(STRSXP, 1));
    SET_STRING_ELT(dir, 0, mkChar(xstringify(JAGS_MODDIR)));
    UNPROTECT(1);
    return(dir);
#else 
    return R_NilValue;
#endif
}
