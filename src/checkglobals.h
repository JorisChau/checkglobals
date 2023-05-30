#ifndef CHECKGLOBALS_H
#define CHECKGLOBALS_H

#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdio.h>

// constants
extern const char *functionals[33];
extern const char *functional_argnms[33];
extern const int functional_argpos[33];
extern const char *skip_nms[7];
extern const char *compiled_nms[7];
extern const char *assign_nms[9];
extern const char *formals_parallel[7][12];

// structs
typedef struct R_args
{
    int skip[7];
    Rboolean compiled;
    Rboolean verbose;
    Rboolean include_datasets;
} R_args;

// helpers
int ddval(SEXP symbol);
int strmatch(const char *target, const char **table, int len);
SEXP matcharg_bypos(SEXP op, SEXP call, SEXP rho, int argpos);
SEXP matcharg_bynamepos(SEXP op, SEXP call, SEXP rho, const char **formals, const char *argname, int argpos);
SEXP operator(SEXP call, SEXP rho);
void global_vars(SEXP call, SEXP rho, SEXP enclos, SEXP env0, Rboolean verbose);
void import_ns(SEXP op, const char *opchar, SEXP call, SEXP rho, SEXP envi, SEXP enclos, Rboolean verbose);
void fun_call(SEXP op, SEXP call, SEXP enclos);
void inline_fun(SEXP call, SEXP enclos, Rboolean verbose);
void local_assign(SEXP op, const char *opchar, SEXP call, SEXP rho, SEXP env0, SEXP enclos, Rboolean verbose);
void import_fun(SEXP op, SEXP call, SEXP rho, SEXP envi, SEXP enclos, SEXP srcrefi, Rboolean verbose);
void local_expr(SEXP enclos);
void compiled_call(SEXP op, SEXP call, SEXP rho, SEXP env0, Rboolean verbose);
void func_call(SEXP op, SEXP call, SEXP rho, int func_id);
void add_reserved_R6(SEXP enclos);

// exported function
SEXP walk_expr(SEXP expr, SEXP env0, SEXP envi, SEXP envg, SEXP rho, SEXP srcrefi, SEXP srcrefg, SEXP R_include_compiled, SEXP R_verbose, SEXP R_include_datasets);

#endif
