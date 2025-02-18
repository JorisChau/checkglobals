#define R_NO_REMAP

#include "checkglobals.h"

/*----------------------------------------------------------------------

  init_enclos

  Initialize new enclosing environment for a call object to assign
  local variables.

*/
static SEXP init_enclos(SEXP call, SEXP enclos, R_len_t i, R_len_t n, SEXPTYPE type)
{
    int nprotect = 1;
    PROTECT_INDEX ipx = 0;
    SEXP enclos1 = NULL, srcref = NULL;
    PROTECT_WITH_INDEX(srcref = Rf_getAttrib(call, Rf_install("srcref")), &ipx);
    if (type != VECSXP)
    {
        enclos1 = PROTECT(R_NewEnv(enclos, TRUE, 0));
        Rf_defineVar(Rf_install(".__closure__"), PROTECT(Rf_ScalarLogical(FALSE)), enclos1);
        nprotect += 2;
    }
    else
    {
        enclos1 = enclos;
    }
    if (!Rf_isNull(srcref))
    {
        Rf_defineVar(Rf_install(".__srcref__"), PROTECT(Rf_duplicate(srcref)), enclos1);
        nprotect++;
    }
    else
    {
        REPROTECT(srcref = R_getVarEx1(Rf_install(".__srcref__"), enclos, FALSE), ipx);
        if (srcref != R_UnboundValue && TYPEOF(srcref) == VECSXP && Rf_length(srcref) == n)
            Rf_defineVar(Rf_install(".__srcref__"), VECTOR_ELT(srcref, i), enclos1);
    }
    UNPROTECT(nprotect);
    return enclos1;
}

/*----------------------------------------------------------------------

  is_reserved

  Check if symbol is a reserved keyword and should be skipped as global
  variable. Currently includes ..1, ..2, etc., and only if ... is present in
  enclosing environment.

*/
static Rboolean is_reserved(SEXP sym, SEXP enclos)
{
    SEXP dots = PROTECT(R_getVarEx1(Rf_install("..."), enclos, TRUE));
    Rboolean reserved = FALSE;
    if (dots != R_UnboundValue && ddval(sym) > 0)
        reserved = TRUE;
    UNPROTECT(1);
    return reserved;
}

/*----------------------------------------------------------------------

  assign_global

  Assign global variables not in enclosing environment to globals
  environment and update matching source references.

*/
static void assign_global(SEXP sym, const char *opchar, SEXP enclos, R_len_t i, R_len_t n, SEXP envg, SEXP srcrefg)
{
    if (sym != R_MissingArg && strcmp(opchar, "::") != 0 && strcmp(opchar, ":::") != 0 && ((strcmp(opchar, "@") != 0 && strcmp(opchar, "$") != 0) || i == 1))
    {
        SEXP nm = PROTECT(R_getVarEx1(sym, enclos, TRUE));
        if (nm == R_UnboundValue && !is_reserved(sym, enclos))
        {
            Rf_defineVar(sym, PROTECT(Rf_mkString(!strcmp(CHAR(PRINTNAME(sym)), opchar) ? "function" : "variable")), envg);
            SEXP srcsym = PROTECT(R_getVarEx1(sym, srcrefg, FALSE));
            SEXP srcsym1 = NULL;
            if (srcsym != R_UnboundValue)
            {
                srcsym1 = PROTECT(Rf_allocVector(VECSXP, Rf_length(srcsym) + 1));
                PROTECT_INDEX ipx = 0;
                SEXP srcsymj = NULL;
                PROTECT_WITH_INDEX(srcsymj = VECTOR_ELT(srcsym, 0), &ipx);
                SET_VECTOR_ELT(srcsym1, 0, srcsymj);
                for (int j = 1; j < Rf_length(srcsym); j++)
                {
                    REPROTECT(srcsymj = VECTOR_ELT(srcsym, j), ipx);
                    SET_VECTOR_ELT(srcsym1, j, srcsymj);
                }
                UNPROTECT(1);
            }
            else
                srcsym1 = PROTECT(Rf_allocVector(VECSXP, 1));
            SEXP nmsrc = PROTECT(R_getVarEx1(Rf_install(".__srcref__"), enclos, TRUE));
            if (nmsrc != R_UnboundValue && TYPEOF(nmsrc) == VECSXP && Rf_length(nmsrc) == n)
            {
                SET_VECTOR_ELT(srcsym1, Rf_length(srcsym1) - 1, PROTECT(VECTOR_ELT(nmsrc, i)));
                UNPROTECT(1);
            }
            else if (nmsrc != R_UnboundValue)
                SET_VECTOR_ELT(srcsym1, Rf_length(srcsym1) - 1, nmsrc);
            Rf_defineVar(sym, srcsym1, srcrefg);
            UNPROTECT(4);
        }
        UNPROTECT(1);
    }
}

/*----------------------------------------------------------------------

  walk

  Main workhorse of C_walk_expr evaluating all call handlers for a single
  object of type EXPRSXP, VECSXP, LANGSXP, LISTSXP, DOTSXP.

*/
static void walk(SEXP call, SEXP enclos, SEXP env0, SEXP envi, SEXP envg, SEXP rho, SEXP srcrefi, SEXP srcrefg, R_args *args)
{
    SEXPTYPE type = TYPEOF(call);
    // increase level
    if ((args->pending_exit)[0])
        (args->pending_exit)[2] += 1;

    if (type == VECSXP || type == EXPRSXP || type == LISTSXP)
    {
        SEXP calli = NULL, enclosi = NULL;
        R_len_t n = Rf_length(call);
        for (R_len_t i = 0; i < n; i++)
        {
            if (type == VECSXP || type == EXPRSXP)
            {
                calli = PROTECT(VECTOR_ELT(call, i));
                if (TYPEOF(calli) == STRSXP && args->is_pkg && enclos == env0)
                {
                    Rf_defineVar(Rf_installChar(STRING_ELT(calli, 0)), R_NilValue, env0);
                    if (args->verbose)
                        Rprintf("DATASET: %s\n", CHAR(STRING_ELT(calli, 0)));
                }
                else
                {
                    enclosi = PROTECT(init_enclos(calli, enclos, i, n, type));
                    walk(calli, enclosi, env0, envi, envg, rho, srcrefi, srcrefg, args);
                    UNPROTECT(1);
                }
                UNPROTECT(1);
            }
            else
            {
                calli = CAR(call);
                enclosi = PROTECT(init_enclos(calli, enclos, i, n, type));
                walk(calli, enclosi, env0, envi, envg, rho, srcrefi, srcrefg, args);
                UNPROTECT(1);
                call = CDR(call);
            }
        }
    }
    else if (type == LANGSXP)
    {
        SEXP op = NULL;
        int is_func = -1, is_skip = -1;
        const char *opchar = "";
        // 1) get operator
        op = PROTECT(operator(call, rho));
        opchar = CHAR(PRINTNAME(op));
        // 2) global variables
        if (args->is_pkg && R_ParentEnv(enclos) == env0 && strcmp(opchar, "globalVariables") == 0)
            global_vars(call, rho, enclos, env0, args->verbose);
        // 3) import namespaces
        if (strcmp(opchar, "library") == 0 || strcmp(opchar, "require") == 0 || strcmp(opchar, "requireNamespace") == 0 || strcmp(opchar, "attachNamespace") == 0)
            import_ns(op, opchar, call, rho, envi, enclos, args->verbose);
        // 4) special functions
        if (args->is_pkg && R_ParentEnv(enclos) == env0 && strmatch(opchar, assign_nms, 9) > -1)
            special_funs(op, opchar, call, rho, env0, args);
        // 5) inline function calls
        fun_call(op, call, enclos);
        // 6) local assignment
        if (strmatch(opchar, assign_nms, 9) > -1)
            local_assign(op, opchar, call, rho, env0, enclos, args->verbose);
        // 7) external function calls
        if (strcmp(opchar, "::") == 0 || strcmp(opchar, ":::") == 0)
            import_fun(op, call, rho, envi, enclos, srcrefi, args);
        // 8) inline functions
        if (strcmp(opchar, "function") == 0)
            inline_fun(call, enclos, args);
        // 9) local expressions
        if (strcmp(opchar, "local") == 0)
            local_expr(enclos);
        // 10) functional calls
        is_func = strmatch(opchar, functionals, 33);
        if (is_func > -1)
            func_call(op, call, rho, is_func, args->parent_opchar);
        // 11) compiled function calls
        if (!(args->compiled) && strmatch(opchar, compiled_nms, 7) > -1)
            compiled_call(op, call, rho, env0, args->verbose);
        // 12) extra reserved names R6Class
        if (strcmp(opchar, "R6Class") == 0)
            add_reserved_R6(enclos);
        // 13) on.exit call
        if (strcmp(opchar, "on.exit") == 0)
            exit_expr(call, enclos, args);
        // unprotect operator
        UNPROTECT(1);
        // 14) find globals
        is_skip = strmatch(opchar, skip_nms, 7);
        if (is_skip > -1 && args->skip[is_skip] < 1)
        {
            if (args->verbose)
                Rprintf("Note: skipping globals in calls to '%s'\n", opchar);
            (args->skip)[is_skip] = 1;
        }
        if (is_skip == -1)
        {
            SEXP calli = NULL, enclosi = NULL;
            R_len_t n = Rf_length(call);
            for (R_len_t i = 0; i < n; i++)
            {
                calli = CAR(call);
                if (Rf_isSymbol(calli))
                {
                    assign_global(calli, opchar, enclos, i, n, envg, srcrefg);
                }
                else if (Rf_isPairList(calli) && !Rf_isNull(calli))
                {
                    enclosi = PROTECT(init_enclos(calli, enclos, i, n, type));
                    args->parent_opchar = opchar;
                    walk(calli, enclosi, env0, envi, envg, rho, srcrefi, srcrefg, args);
                    UNPROTECT(1);
                }
                if (i < (n - 1))
                    call = CDR(call);
            }
            // replace lazy symbol
            if (Rf_isSymbol(calli) && R_ParentEnv(enclos) != env0)
            {
                SEXP fun = PROTECT(find_var_in_closure(calli, enclos));
                if (fun != R_UnboundValue && !Rf_isNull(fun) && Rf_isPairList(fun))
                {
                    SEXP sym = CAR(fun);
                    if (Rf_isSymbol(sym) && strcmp(CHAR(PRINTNAME(sym)), "function") == 0)
                    {
                        Rf_setVar(calli, R_NilValue, enclos);
                        walk(fun, enclos, env0, envi, envg, rho, srcrefi, srcrefg, args);
                    }
                }
                UNPROTECT(1);
            }
            // evaluate on.exit expression
            if ((args->pending_exit)[0] && (args->pending_exit)[1] == (args->pending_exit)[2])
            {
                SEXP exit = PROTECT(R_getVarEx1(Rf_install("on.exit"), enclos, FALSE));
                if (exit != R_UnboundValue && !Rf_isNull(exit))
                {
                    (args->pending_exit)[0] = 0;
                    Rf_setVar(Rf_install("on.exit"), R_NilValue, enclos);
                    walk(exit, enclos, env0, envi, envg, rho, srcrefi, srcrefg, args);
                }
                UNPROTECT(1);
            }
        }
    }
    // decrease level
    if ((args->pending_exit)[0])
        (args->pending_exit)[2] -= 1;
}

SEXP walk_expr(SEXP expr, SEXP env0, SEXP envi, SEXP envg, SEXP rho, SEXP srcrefi, SEXP srcrefg,
               SEXP R_is_pkg, SEXP R_include_compiled, SEXP R_verbose, SEXP R_basepkg)
{
    // settings
    R_args args = {
        .skip = {0, 0, 0, 0, 0, 0, 0},
        .is_pkg = LOGICAL_ELT(R_is_pkg, 0),
        .compiled = LOGICAL_ELT(R_include_compiled, 0),
        .verbose = LOGICAL_ELT(R_verbose, 0),
        .skip_closure = FALSE,
        .parent_opchar = "",
        .pending_exit = {0, 0, 0},
        .pkgname = CHAR(STRING_ELT(R_basepkg, 0))
    };
    // recurse
    walk(expr, env0, env0, envi, envg, rho, srcrefi, srcrefg, &args);
    // no return value
    return R_NilValue;
}
