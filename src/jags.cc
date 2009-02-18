#include <map>
#include <string>
#include <sstream>
#include <algorithm>
#include <vector>
#include <stdexcept>

#include <Console.h>
#include <util/nainf.h>

using std::string;
using std::map;
using std::pair;
using std::vector;
using std::copy;

/* Workaround length being remapped to Rf_length
   by the preprocessor */

unsigned long sarray_len(SArray const &s)
{
  return s.length();
}

int min2(int a, int b)
{
  return std::min(a,b);
}

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

std::ostringstream jags_out; //Output stream
std::ostringstream jags_err; //Error stream
static SEXP JAGS_console_tag; //Run-time type checking for external pointer

static void checkConsole (SEXP s)
{				  
  if (TYPEOF(s) != EXTPTRSXP || R_ExternalPtrTag(s) != JAGS_console_tag)
    {
        error("bad JAGS console pointer");
    }
}

static int intArg(SEXP arg)
{
    if (!isNumeric(arg)) {
	error("Invalid integer parameter");
    }
    
    SEXP intarg;
    PROTECT(intarg = AS_INTEGER(arg));
    int i = INTEGER_POINTER(intarg)[0];
    UNPROTECT(1);
    return i;
}

static char const *stringArg(SEXP arg, unsigned int i = 0)
{
    if (!isString(arg)) {
	error("Invalid string parameter");
    }
    return R_CHAR(STRING_ELT(arg,i));
}

static bool boolArg(SEXP arg)
{
    if (!isLogical(arg)) {
	error("Invalid logical parameter");
    }
    return LOGICAL_POINTER(arg)[0];
}

static Console * ptrArg(SEXP ptr)
{
    checkConsole(ptr);
    Console *console = static_cast<Console*>(R_ExternalPtrAddr(ptr));
    if (console == NULL)
	error("JAGS model must be recompiled");
    return console;
}

static void printMessages(bool status)
{
    /* Print any messages from JAGS and clear the stream buffer */
    if(!jags_out.str().empty()) {
	Rprintf("%s\n", jags_out.str().c_str());
	jags_out.str("");
    }
    string msg;
    if (!jags_err.str().empty()) {
	msg = jags_err.str();
	jags_err.str("");
    }
    if (status == true) {
	if (!msg.empty()) {
	    warning("%s\n", msg.c_str());
	}
    }
    else {
	//Something bad happened
	if (!msg.empty()) {
	    error("%s\n", msg.c_str());
	}
	else {
	    error("Internal error in JAGS library");
	}
    }
}

static void setSArrayValue(SArray &sarray, SEXP e)
{
    vector<double> v(length(e));
    copy(NUMERIC_POINTER(e), NUMERIC_POINTER(e) + length(e), v.begin());
    sarray.setValue(v);
}

/* Write data from an R list into a JAGS data table */
static void writeDataTable(SEXP data, map<string,SArray> &table)
{
    SEXP names;
    PROTECT(names = getAttrib(data, R_NamesSymbol));
    if (!isNewList(data)) {
	error("data must be a list");
    }
    if (length(names) != length(data)) {
	error("data must be a named list");
    }
    int N = length(data);

    for (int i = 0; i < N; ++i) {
	SEXP e, e2, dim;
	PROTECT(e = VECTOR_ELT(data, i));
	PROTECT(dim = GET_DIM(e)); 
	PROTECT(e2 = AS_NUMERIC(e));
	//Replace R missing values in e2 with JAGS missing values
	int elength = length(e2);
	for (int j = 0; j < elength; ++j) {
	    if (ISNA(NUMERIC_POINTER(e2)[j])) {
		NUMERIC_POINTER(e2)[j] = JAGS_NA;
	    }
	}

	string ename = CHAR(STRING_ELT(names, i));

	int ndim = length(dim);
	if (ndim == 0) {
	    // Scalar or vector entry
	    if (e2 > 0) {
		SArray sarray(vector<unsigned int>(1, length(e2)));
		setSArrayValue(sarray, e2);
		table.insert(pair<string,SArray>(ename, sarray));
	    }
	}
	else {
	    // Array entry
	    vector<unsigned int> idim(ndim);
	    SEXP dim2;
	    PROTECT(dim2 = AS_INTEGER(dim));
	    for (int j = 0; j < ndim; ++j) {
		idim[j] = INTEGER_POINTER(dim2)[j];
	    }
	    UNPROTECT(1);
	    SArray sarray(idim);
	    setSArrayValue(sarray, e2);
	    table.insert(pair<string,SArray>(ename,sarray));
	}
	UNPROTECT(3);
    }
    UNPROTECT(1);
}

static Range makeRange(SEXP lower, SEXP upper)
{
    if (lower == R_NilValue || upper == R_NilValue) {
	return Range();
    }
    if (length(lower) != length(upper)) {
	error("length mismatch between lower and upper limits");
    }
    int n = length(lower);

    SEXP il, iu;
    PROTECT(il = AS_INTEGER(lower));
    PROTECT(iu = AS_INTEGER(upper));
    vector<int> lvec(n), uvec(n);
    copy(INTEGER(il), INTEGER(il) + n, lvec.begin());
    copy(INTEGER(iu), INTEGER(iu) + n, uvec.begin());
    UNPROTECT(2);

    Range r;
    try {
	r = Range(lvec, uvec);
    }
    catch (std::logic_error except) {                                   
	error("Invalid range");
    }
    return r;
}

#include <iostream>
/* Read data from a JAGS data table into and R list */
static SEXP readDataTable(map<string,SArray> const &table)
{
    int N = table.size();

    SEXP data;
    PROTECT(data = allocVector(VECSXP, N));

    int i;
    map<string,SArray>::const_iterator p;

    for (i = 0, p = table.begin(); p != table.end(); ++p, ++i) {
	int len = sarray_len(p->second);

	//Allocate new numeric vector
	SEXP e;
	PROTECT(e = allocVector(REALSXP, len));

	//Copy values
	vector<double> const &value = p->second.value();
	for (int j = 0; j < len; ++j) {
            if (value[j] == JAGS_NA) {
               NUMERIC_POINTER(e)[j] = NA_REAL;
            }
            else {
	       NUMERIC_POINTER(e)[j] = value[j];
            }
	}
    
	if (p->second.ndim(false) > 1) {

	    //Set dim attribute
	    vector<unsigned int> const &idim = p->second.dim(false);
	    unsigned int ndim = idim.size();
	    SEXP dim;
	    PROTECT(dim = allocVector(INTSXP, ndim));
	    for (unsigned int k = 0; k < ndim; ++k) {
		INTEGER_POINTER(dim)[k] = idim[k];
	    }

	    //Set names of the dimensions 
	    vector<string> const &names = p->second.dimNames();
	    if (!names.empty()) {
		SEXP dimnames;
		PROTECT(dimnames = allocVector(STRSXP, ndim));
		for (unsigned int k = 0; k < ndim; ++k) {
		    SET_STRING_ELT(dimnames, k, mkChar(names[k].c_str()));
		}
		setAttrib(dim, R_NamesSymbol, dimnames);
	        SET_DIM(e, dim);
		UNPROTECT(2); //dimnames, dim
	    }
            else {
	        SET_DIM(e, dim);
	        UNPROTECT(1); //dim
            }
	}
    
	SET_ELEMENT(data, i, e);
	UNPROTECT(1); //e
    }

    //Set names
    SEXP names;
    PROTECT(names = allocVector(STRSXP, table.size()));
    for (i = 0, p = table.begin() ; p != table.end(); ++p, ++i) {
	SET_STRING_ELT(names, i, mkChar(p->first.c_str()));
    }
    setAttrib(data, R_NamesSymbol, names);
    UNPROTECT(2); //names, data
    return data;
}

extern "C" {
    
    SEXP init_jags_console()
    {
	/* Called by .First.lib */
	JAGS_console_tag = install("JAGS_CONSOLE_TAG");
	return R_NilValue;
    }

    SEXP clear_console(SEXP s)
    {
	/* Finalizer for console pointers. Frees the external memory
	   and zeroes the pointer when the R object is deleted */

	checkConsole(s);
	Console *console = static_cast<Console*>(R_ExternalPtrAddr(s));
	if (console != NULL) {
	    delete console;
	    R_ClearExternalPtr(s);
	}
	return R_NilValue;
    }

    SEXP make_console()
    {
	void *p = static_cast<void*>(new Console(jags_out, jags_err));
	SEXP ptr = R_MakeExternalPtr(p, JAGS_console_tag, R_NilValue);
	R_RegisterCFinalizer(ptr, (R_CFinalizer_t) clear_console);
	return ptr;
    }
  
    SEXP check_model(SEXP ptr, SEXP name)
    {
	/* Name should be a name of a file containing the model */
    
	string sname = stringArg(name);
	FILE *file = fopen(sname.c_str(), "r");
	if (!file) {
	    jags_err << "Failed to open file " << sname << "\n";
	    return R_NilValue;
	}
	else {
	    bool status = ptrArg(ptr)->checkModel(file);	    
 	    printMessages(status);
	    fclose(file);
	    return R_NilValue;
	}
    }

    SEXP compile(SEXP ptr, SEXP data, SEXP nchain, SEXP gendata)
    {
	if (!isNumeric(nchain)) {
	    error("nchain must be numeric");
	}
	if (!isVector(data)) {
	    error("invalid data");
	}

	map<string, SArray> table;
	writeDataTable(data, table);
	bool status = ptrArg(ptr)->compile(table, intArg(nchain),
					   boolArg(gendata));
	printMessages(status);
	return R_NilValue;
    }

    SEXP set_parameters(SEXP ptr, SEXP data, SEXP nchain)
    {
	map<string,SArray> data_table;
	writeDataTable(data, data_table);
	bool status = ptrArg(ptr)->setParameters(data_table, intArg(nchain));
	printMessages(status);
	return R_NilValue;
    }
  
    SEXP set_rng_name(SEXP ptr, SEXP name, SEXP chain)
    {
	bool status = ptrArg(ptr)->setRNGname(stringArg(name), intArg(chain));
	printMessages(status);
 	return R_NilValue;
    }
  
    SEXP initialize(SEXP ptr)
    {
	bool status = ptrArg(ptr)->initialize();
	printMessages(status);
	return R_NilValue;
    }
/*  
    void do_update(Console *console, int niter)
    {
	int width = 50;
	int refresh = niter/width;

	bool adapt = console->isAdapting();

	if (refresh == 0) {
	    refresh = 10;
	    for (int n = niter; n > 0; n-=refresh) {
		console->update(min2(n, refresh));
		R_CheckUserInterrupt();
	    }
	    bool status = true;
	    if (adapt) {
		console->adaptOff(status);
	    }
	    if (!status) {
		warning("Adaptation incomplete");
	    }
	    return;
	}
    
	if (width > niter / refresh + 1) {
	    width = niter / refresh + 1;
        }

        if (adapt) {
	    Rprintf("Adapting %d\n", niter);
        }
        else {
	    Rprintf("Updating %d\n", niter);
        }
	for (int i = 0; i < width - 1; ++i) {
	    Rprintf("-");
	}
	Rprintf("| %d\n", min2(width * refresh, niter));
    
	int col = 0;
	for (int n = niter; n > 0; n -= refresh) {
	    int nupdate = min2(n, refresh);
	    if(console->update(nupdate)) {
                if (adapt) {
		   Rprintf("+");
                }
                else {
                   Rprintf("*");
                }
            }
	    else {
		Rprintf("\n");
		printMessages(false);
		return;
	    }
	    col++;
	    if (col == width || n <= nupdate) {
		int percent = 100 - (n-nupdate) * 100/niter;
		Rprintf(" %d%%\n", percent); 
		if (n > nupdate) {
		    col = 0;
		}
	    }
            R_CheckUserInterrupt();
        }

	bool status = true;
	if (adapt) {
	    // Turn off adaptive mode 
	    console->adaptOff(status);
	    adapt = false;
	}
	if (!status) {
	    warning("Adaptation incomplete");
	}
    }
*/

    SEXP is_adapting(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	return ScalarLogical(console->isAdapting());
    }

    SEXP adapt_off(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	bool status = true;
	console->adaptOff(status);
	return ScalarLogical(status);
    }

    SEXP update(SEXP ptr, SEXP rniter)
    {
        int niter = intArg(rniter);
        Console *console = ptrArg(ptr);
	if (!console->update(niter)) {
	    Rprintf("\n");
	    printMessages(false);
	}
	return R_NilValue;
    }

    SEXP set_monitors(SEXP ptr, SEXP names, SEXP lower, SEXP upper, 
		      SEXP thin, SEXP type)
    {
	if (!isString(names)) {
	    error("names must be a character vector");
	}

	unsigned int n = length(names);
	if (length(lower) != n || length(upper) != n) {
	    error("length of names must match length of lower and upper");
	}
	unsigned int i;
	for (i = 0; i < n; ++i) {
	    Range range = makeRange(VECTOR_ELT(lower, i), VECTOR_ELT(upper, i));
	    bool status = ptrArg(ptr)->setMonitor(stringArg(names,i), range, 
						  intArg(thin), 
						  stringArg(type));
	    if (!status)
		break;
	}
	if (i < n) {
	    //Failure to set monitor i: unwind the others
	    Range range = makeRange(VECTOR_ELT(lower, i), VECTOR_ELT(upper, i));
	    for (unsigned int j = i; j > 0; --j) {
		ptrArg(ptr)->clearMonitor(stringArg(names, j - 1), range,
					  stringArg(type));
	    }
	    printMessages(false);
	}
	else {
	    printMessages(true);
	}
	return R_NilValue;
    }

    SEXP set_default_monitors(SEXP ptr, SEXP thin, SEXP type)
    {
	bool status = ptrArg(ptr)->setDefaultMonitors(stringArg(type),
						      intArg(thin));
	printMessages(status);
	return R_NilValue;
    }

    SEXP clear_monitor(SEXP ptr, SEXP name, SEXP lower, SEXP upper, SEXP type)
    {
        Range range = makeRange(lower, upper);
	bool status = ptrArg(ptr)->clearMonitor(stringArg(name), range, 
						stringArg(type));
	printMessages(status);
	return R_NilValue;
    }

    SEXP clear_default_monitors(SEXP ptr, SEXP type)
    {
	bool status = ptrArg(ptr)->clearDefaultMonitors(stringArg(type));
	printMessages(status);
	return R_NilValue;
    }

    SEXP get_monitored_values(SEXP ptr, SEXP type)
    {
	map<string,SArray> data_table;
	bool status = ptrArg(ptr)->dumpMonitors(data_table, stringArg(type));
	printMessages(status);
	return readDataTable(data_table);
    }

    SEXP get_data(SEXP ptr)
    {
	map<string,SArray> data_table;
	string rngname; //Not actually needed
	bool status = ptrArg(ptr)->dumpState(data_table, rngname, DUMP_DATA, 1);
	printMessages(status);
	return readDataTable(data_table);
    }

    SEXP get_state(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	unsigned int nchain = console->nchain();
	if (nchain == 0) {
	    return R_NilValue;
	}

	//ans is the list that contains the state for each chain
	SEXP ans;
	PROTECT(ans = allocVector(VECSXP, nchain));
	for (unsigned int n = 0; n < nchain; ++n) {
	    string srng;
	    map<string,SArray> param_table;
	    console->dumpState(param_table, srng, DUMP_PARAMETERS, n+1);
	    //Read the parameter values into an R list
	    SEXP params, names;
	    PROTECT(params = readDataTable(param_table));
	    int nparam = length(params);
	    PROTECT(names = getAttrib(params, R_NamesSymbol));
	    //Now we have to make a copy of the list with an extra element
	    SEXP staten, namesn;
	    PROTECT(staten = allocVector(VECSXP, nparam + 1));
	    PROTECT(namesn = allocVector(STRSXP, nparam + 1));
	    for (int j = 0; j < nparam; ++j) {
		SET_ELEMENT(staten, j, VECTOR_ELT(params, j));
		SET_STRING_ELT(namesn, j, STRING_ELT(names, j));
	    }
	    //Assign .RNG.name as the last element
	    SEXP rngname;
	    PROTECT(rngname = allocVector(STRSXP,1));
	    SET_STRING_ELT(rngname, 0, mkChar(srng.c_str()));
	    SET_ELEMENT(staten, nparam, rngname);
	    SET_STRING_ELT(namesn, nparam, mkChar(".RNG.name"));
	    setAttrib(staten, R_NamesSymbol, namesn);
	    //And we're done with this chain
	    SET_ELEMENT(ans, n, staten);
	    UNPROTECT(5); //rngname, namesn, statesn, names, params
	}
	UNPROTECT(1); //ans
	return ans;
    }


    SEXP get_variable_names(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	vector<string> const &namevec = console->variableNames();
	SEXP varnames;
	PROTECT(varnames = allocVector(STRSXP,namevec.size()));
	for (unsigned int i = 0; i < namevec.size(); ++i) {
	    SET_STRING_ELT(varnames, i, mkChar(namevec[i].c_str()));
	}
	UNPROTECT(1);
	return varnames;
    }

    SEXP get_samplers(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	vector<vector<string> > samplers;
	bool status = console->dumpSamplers(samplers);
	printMessages(status);
	    
	unsigned int n = samplers.size();
	SEXP node_list, sampler_names;
	PROTECT(node_list = allocVector(VECSXP, n));
	PROTECT(sampler_names = allocVector(STRSXP, n));
	
	for (unsigned int i = 0; i < n; ++i) {
	    int nnode = samplers[i].size() - 1;
	    SEXP e;
	    PROTECT(e=allocVector(STRSXP, nnode));
	    for (int j = 0; j < nnode; ++j) {
		SET_STRING_ELT(e, j, mkChar(samplers[i][j+1].c_str()));
	    }
	    SET_ELEMENT(node_list, i, e);
	    SET_STRING_ELT(sampler_names, i, mkChar(samplers[i][0].c_str()));
	    UNPROTECT(1); //e
	}
	setAttrib(node_list, R_NamesSymbol, sampler_names);	
	UNPROTECT(2); //names, ans
	return node_list;
    }
    
    SEXP get_iter(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	unsigned int iter = console->iter();

	SEXP ans;
	PROTECT(ans = allocVector(INTSXP, 1));
	INTEGER(ans)[0] = iter;
	UNPROTECT(1);
	return ans;
    }
    
    SEXP get_nchain(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	unsigned int nchain = console->nchain();
    
	SEXP ans;
	PROTECT(ans = allocVector(INTSXP,1));
	INTEGER(ans)[0] = nchain;
	UNPROTECT(1);
	return ans;
    }
}
