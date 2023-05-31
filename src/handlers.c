#define R_NO_REMAP

#include "checkglobals.h"

/*----------------------------------------------------------------------

  operator

  Retrieve operator from call and return as symbol.

*/
SEXP operator(SEXP call, SEXP rho)
{
    SEXP sym = Rf_install(".__unknown__");
    SEXP arg = CAR(call);
    if (Rf_isSymbol(arg))
    {
        sym = Rf_install(CHAR(PRINTNAME(arg)));
    }
    else if (Rf_isPairList(arg))
    {
        SEXP cararg = CAR(arg);
        SEXP argarg = NULL;
        if (Rf_isSymbol(cararg))
        {
            const char *carchar = CHAR(PRINTNAME(cararg));
            if (strcmp(carchar, "::") == 0 || strcmp(carchar, "::") == 0)
                argarg = PROTECT(matcharg_bypos(cararg, arg, rho, 1));
            else if (strcmp(carchar, "get") == 0 || strcmp(carchar, "mget") == 0 || strcmp(carchar, "dynGet") == 0)
                argarg = PROTECT(matcharg_bypos(cararg, arg, rho, 0));
            else
            {
                while (!Rf_isNull(arg))
                {
                    cararg = CAR(arg);
                    arg = CDR(arg);
                }
                argarg = PROTECT(Rf_duplicate(cararg));
            }
            if (Rf_isSymbol(argarg))
                sym = Rf_install(CHAR(PRINTNAME(argarg)));
            else if (Rf_isValidString(argarg) && Rf_length(argarg) == 1)
                sym = Rf_installChar(STRING_ELT(argarg, 0));
            UNPROTECT(1);
        }
        else if (Rf_isValidString(cararg) && Rf_length(cararg) == 1)
            sym = Rf_installChar(STRING_ELT(cararg, 0));
    }
    else if (Rf_isValidString(arg) && Rf_length(arg) == 1)
    {
        // TODO: find example where this is actually reached
        sym = Rf_installChar(STRING_ELT(arg, 0));
    }
    return sym;
}

/*----------------------------------------------------------------------

   global_vars

   Handle names defined in utils::globalVariables as global variables
   by assigning names to root environment.

 */
void global_vars(SEXP call, SEXP rho, SEXP enclos, SEXP env0, Rboolean verbose)
{
    int evalerror = 0;
    SEXP arg = PROTECT(matcharg_bypos(Rf_install("globalVariables"), call, rho, 0));
    SEXP vars = PROTECT(R_tryEvalSilent(arg, enclos, &evalerror));
    if (!evalerror)
    {
        if (Rf_isValidString(vars))
            for (int j = 0; j < Rf_length(vars); j++)
                Rf_defineVar(Rf_installChar(STRING_ELT(vars, j)), R_NilValue, env0);
    }
    else if (verbose)
        Rprintf("ERROR: failed to evaluate call to globalVariables\n");
    UNPROTECT(2);
}

/*----------------------------------------------------------------------

  import_ns

  Handle calls to `library`, `require`, `requireNamespace`, `attachNamespace`
  by assigning loaded packages to imports environment.

*/
void import_ns(SEXP op, const char *opchar, SEXP call, SEXP rho, SEXP envi, SEXP enclos, Rboolean verbose)
{
    int nprotect = 2;
    int evalerror = 0;
    SEXP pkg = PROTECT(matcharg_bypos(op, call, rho, 0));
    SEXP pkgstr = R_NilValue;

    if (strcmp(opchar, "requireNamespace") == 0)
    {
        pkgstr = PROTECT(R_tryEvalSilent(pkg, enclos, &evalerror));
    }
    else if (strcmp(opchar, "library") == 0 || strcmp(opchar, "require") == 0)
    {
        SEXP characteronly = PROTECT(matcharg_bypos(op, call, rho, 4));
        if (Rf_isLogical(characteronly) && LOGICAL_ELT(characteronly, 0))
            pkgstr = PROTECT(R_tryEvalSilent(pkg, enclos, &evalerror));
        else
            pkgstr = PROTECT(Rf_coerceVector(pkg, STRSXP));
        nprotect++;
    }
    else
    {
        SEXP pkgstr0 = PROTECT(R_tryEvalSilent(pkg, enclos, &evalerror));
        if (!evalerror && R_IsNamespaceEnv(pkgstr0))
        {
            SEXP pkgspec = PROTECT(R_NamespaceEnvSpec(pkgstr0));
            pkgstr = PROTECT(Rf_ScalarString(STRING_ELT(pkgspec, 0)));
            nprotect += 2;
        }
        else
        {
            pkgstr = PROTECT(Rf_coerceVector(pkgstr0, STRSXP));
            nprotect++;
        }
    }
    if (!evalerror && Rf_isValidString(pkgstr) && Rf_length(pkgstr) == 1)
    {
        if (verbose)
            Rprintf("PKG_LOAD: %s\n", CHAR(STRING_ELT(pkgstr, 0)));
        SEXP pkgs = PROTECT(Rf_findVar(Rf_install(".__pkgs__"), envi));
        SEXP pkgs1 = PROTECT(Rf_allocVector(STRSXP, Rf_length(pkgs) + 1));
        if (Rf_length(pkgs) > 0)
            for (int j = 0; j < Rf_length(pkgs); j++)
                SET_STRING_ELT(pkgs1, j, STRING_ELT(pkgs, j));
        SET_STRING_ELT(pkgs1, Rf_length(pkgs), STRING_ELT(pkgstr, 0));
        Rf_defineVar(Rf_install(".__pkgs__"), pkgs1, envi);
        Rf_defineVar(Rf_install(CHAR(STRING_ELT(pkgstr, 0))), R_NilValue, enclos);
        nprotect += 2;
    }
    else if (evalerror && verbose)
        Rprintf("ERROR: failed to evaluate call to %s\n", opchar);
    UNPROTECT(nprotect);
}

/*----------------------------------------------------------------------

  inline_fun

  Handle `function` definitions by assigning to enclosing environment.

*/
void inline_fun(SEXP call, SEXP enclos, Rboolean verbose)
{
    SEXP args = CADR(call);
    if (TYPEOF(args) == LISTSXP)
    {
        while (!Rf_isNull(args))
        {
            if (verbose)
                Rprintf("SYMBOL_FORMALS: %s\n", CHAR(PRINTNAME(TAG(args))));
            Rf_defineVar(TAG(args), R_NilValue, enclos);
            args = CDR(args);
        }
        Rf_defineVar(Rf_install(".__closure__"), PROTECT(Rf_ScalarLogical(TRUE)), enclos);
        UNPROTECT(1);
    }
}

/*----------------------------------------------------------------------

  local_expr

  Handle `local` expressions by assigning to enclosing environment.

*/
void local_expr(SEXP enclos)
{
    Rf_defineVar(Rf_install(".__closure__"), PROTECT(Rf_ScalarLogical(TRUE)), enclos);
    UNPROTECT(1);
}

/*----------------------------------------------------------------------

  fun_call

  Search enclosing environment to replace function call by existing
  function definition.

*/
void fun_call(SEXP op, SEXP call, SEXP enclos)
{
    SEXP fun = PROTECT(Rf_findVar(op, enclos));
    SEXP funbase = PROTECT(Rf_findVar(op, R_BaseEnv));

    if (fun != R_UnboundValue && funbase == R_UnboundValue && !Rf_isNull(fun) && Rf_isPairList(fun))
    {
        SEXP sym = CAR(fun);
        if (Rf_isSymbol(sym) && strcmp(CHAR(PRINTNAME(sym)), "function") == 0)
        {
            SETCAR(call, fun);
            Rf_setVar(op, R_NilValue, enclos);
        }
    }
    UNPROTECT(2);
}

/*----------------------------------------------------------------------

  local_assign

  Handle calls to `<-`, `=`, `<<-`, `for`, `assign`, `delayedAssign`,
  `setMethod`, `setGeneric` by assigning local variable definitions to
  enclosing environment.

*/
void local_assign(SEXP op, const char *opchar, SEXP call, SEXP rho, SEXP env0, SEXP enclos, Rboolean verbose)
{
    SEXP sym = NULL, val = NULL;
    Rboolean setnullval = FALSE, assignfun = FALSE;
    PROTECT_INDEX ipx = 0;
    int nprotect = 2;

    if (strcmp(opchar, "assign") == 0 || strcmp(opchar, "delayedAssign") == 0 || strcmp(opchar, "setMethod") == 0 ||
        strcmp(opchar, "setGeneric") == 0 || strcmp(opchar, "makeActiveBinding") == 0)
    {
        PROTECT_WITH_INDEX(sym = matcharg_bypos(op, call, rho, 0), &ipx);
        val = PROTECT(matcharg_bypos(op, call, rho, strcmp(opchar, "setMethod") == 0 ? 2 : 1));
    }
    else
    {
        PROTECT_WITH_INDEX(sym = CADR(call), &ipx);
        val = PROTECT(CADDR(call));
    }
    if (TYPEOF(val) == LANGSXP && Rf_isSymbol(CAR(val)) && strcmp(CHAR(PRINTNAME(CAR(val))), "function") == 0)
    {
        assignfun = TRUE;
        if (ENCLOS(enclos) != env0)
            SETCADDR(call, Rf_install("function"));
        else
            setnullval = TRUE;
    }
    if (TYPEOF(sym) == STRSXP && Rf_length(sym) == 1)
    {
        REPROTECT(sym = Rf_installChar(STRING_ELT(sym, 0)), ipx);
    }
    if (Rf_isSymbol(sym))
    {
        if (verbose)
            Rprintf("SYMBOL: %s\n", CHAR(PRINTNAME(sym)));
        SEXP parent_env = enclos;
        SEXP closure = NULL;
        PROTECT_INDEX ipx1 = 0;
        PROTECT_WITH_INDEX(closure = Rf_findVarInFrame(parent_env, Rf_install(".__closure__")), &ipx1);
        nprotect++;
        Rboolean isclosure = (closure != R_UnboundValue) ? LOGICAL_ELT(closure, 0) : FALSE;
        while (!isclosure)
        {
            parent_env = ENCLOS(parent_env);
            REPROTECT(closure = Rf_findVarInFrame(parent_env, Rf_install(".__closure__")), ipx1);
            isclosure = (closure != R_UnboundValue) ? LOGICAL_ELT(closure, 0) : FALSE;
        }
        if (!setnullval)
        {
            Rf_defineVar(sym, PROTECT(Rf_duplicate(val)), parent_env);
            nprotect++;
        }
        else
        {
            Rf_defineVar(sym, R_NilValue, parent_env);
        }
        if (assignfun)
        {
            char symfun[256];
            strcpy(symfun, CHAR(PRINTNAME(sym)));
            const char *symend = strrchr(symfun, '<');
            if (symend && strcmp(symend, "<-") == 0)
            {
                symfun[strlen(symfun) - 2] = '\0';
                if (!setnullval)
                {
                    Rf_defineVar(Rf_install(symfun), PROTECT(Rf_duplicate(val)), parent_env);
                    nprotect++;
                }
                else
                {
                    Rf_defineVar(Rf_install(symfun), R_NilValue, parent_env);
                }
            }
        }
    }
    UNPROTECT(nprotect);
}

/*----------------------------------------------------------------------

  import_fun

  Handle calls to `::` or `:::` by assigning imported functions to
  imports and globals environments.

*/
void import_fun(SEXP op, SEXP call, SEXP rho, SEXP envi, SEXP enclos, SEXP srcrefi, Rboolean verbose)
{
    PROTECT_INDEX ipx = 0, ipxf = 0;
    SEXP pkg = NULL, pkgfun = NULL;
    PROTECT_WITH_INDEX(pkg = matcharg_bypos(op, call, rho, 0), &ipx);
    PROTECT_WITH_INDEX(pkgfun = matcharg_bypos(op, call, rho, 1), &ipxf);

    if (TYPEOF(pkg) == STRSXP && Rf_length(pkg) == 1)
        REPROTECT(pkg = Rf_installChar(STRING_ELT(pkg, 0)), ipx);
    if (TYPEOF(pkgfun) == STRSXP && Rf_length(pkgfun) == 1)
        REPROTECT(pkgfun = Rf_installChar(STRING_ELT(pkgfun, 0)), ipxf);

    if (Rf_isSymbol(pkg) && Rf_isSymbol(pkgfun) && strcmp(CHAR(PRINTNAME(pkg)), "base") != 0)
    {
        int nprotect = 4;
        if (verbose)
            Rprintf("PKG_SYMBOL: %s, %s\n", CHAR(PRINTNAME(pkg)), CHAR(PRINTNAME(pkgfun)));
        SEXP pkg0 = PROTECT(Rf_findVarInFrame(envi, pkgfun));
        if (pkg0 == R_UnboundValue)
        {
            Rf_defineVar(pkgfun, PROTECT(Rf_ScalarString(PRINTNAME(pkg))), envi);
            nprotect++;
        }
        else
        {
            Rboolean matched = FALSE;
            for (int i = 0; i < Rf_length(pkg0); i++)
            {
                if (strcmp(CHAR(STRING_ELT(pkg0, i)), CHAR(PRINTNAME(pkg))) == 0)
                {
                    matched = TRUE;
                    break;
                }
            }
            if (!matched)
            {
                SEXP pkg1 = PROTECT(Rf_allocVector(STRSXP, Rf_length(pkg0) + 1));
                for (int i = 0; i < Rf_length(pkg0); i++)
                    SET_STRING_ELT(pkg1, i, STRING_ELT(pkg0, i));
                SET_STRING_ELT(pkg1, Rf_length(pkg0), PRINTNAME(pkg));
                Rf_defineVar(pkgfun, pkg1, envi);
                nprotect++;
            }
        }
        SEXP srcfun = PROTECT(Rf_findVarInFrame(srcrefi, pkgfun));
        SEXP srcfun1 = NULL;
        if (srcfun != R_UnboundValue)
        {
            srcfun1 = PROTECT(Rf_allocVector(VECSXP, Rf_length(srcfun) + 1));
            PROTECT_INDEX ipx1 = 0;
            SEXP srcfunj = NULL;
            PROTECT_WITH_INDEX(srcfunj = VECTOR_ELT(srcfun, 0), &ipx1);
            SET_VECTOR_ELT(srcfun1, 0, srcfunj);
            for (int j = 1; j < Rf_length(srcfun); j++)
            {
                REPROTECT(srcfunj = VECTOR_ELT(srcfun, j), ipx1);
                SET_VECTOR_ELT(srcfun1, j, srcfunj);
            }
            UNPROTECT(1);
        }
        else
            srcfun1 = PROTECT(Rf_allocVector(VECSXP, 1));
        SEXP nmsrc = PROTECT(Rf_findVar(Rf_install(".__srcref__"), enclos));
        SET_VECTOR_ELT(srcfun1, Rf_length(srcfun1) - 1, nmsrc);
        Rf_defineVar(pkgfun, srcfun1, srcrefi);
        UNPROTECT(nprotect);
    }
    UNPROTECT(2);
}

/*----------------------------------------------------------------------

  compiled_call

  Handle calls `.Call`, `.C`, `.External`, `.Fortran` by assigning
  compiled function definitions to root environment.

*/
void compiled_call(SEXP op, SEXP call, SEXP rho, SEXP env0, Rboolean verbose)
{
    PROTECT_INDEX ipx = 0;
    SEXP fun = NULL;
    PROTECT_WITH_INDEX(fun = matcharg_bypos(op, call, rho, 0), &ipx);
    if (TYPEOF(fun) == STRSXP && Rf_length(fun) == 1)
        REPROTECT(fun = Rf_installChar(STRING_ELT(fun, 0)), ipx);
    if (Rf_isSymbol(fun))
    {
        if (verbose)
            Rprintf("COMPILED_FUN: %s\n", CHAR(PRINTNAME(fun)));
        Rf_defineVar(fun, R_NilValue, env0);
    }
    UNPROTECT(1);
}

/*----------------------------------------------------------------------

  func_call

  Handle common base functionals by converting character function
  arguments to symbols.

*/
void func_call(SEXP op, SEXP call, SEXP rho, int func_id)
{
    SEXP actualpos = NULL;
    const char *argname = functional_argnms[func_id];
    int argpos = functional_argpos[func_id];

    if (func_id >= 19 && func_id <= 25)
        actualpos = PROTECT(matcharg_bynamepos(op, call, R_NilValue, formals_parallel[func_id - 19], argname, argpos - 1));
    else
        actualpos = PROTECT(matcharg_bynamepos(op, call, rho, NULL, argname, argpos - 1));

    if (!Rf_isNull(actualpos))
        argpos = INTEGER_ELT(actualpos, 0);

    int i = 0;
    while (i < argpos && !Rf_isNull(call))
    {
        call = CDR(call);
        i++;
    }
    SEXP func_arg = CAR(call);
    if (TYPEOF(func_arg) == STRSXP && Rf_length(func_arg) == 1)
    {
        SETCAR(call, PROTECT(Rf_coerceVector(func_arg, SYMSXP)));
        UNPROTECT(1);
    }
    UNPROTECT(1);
}

/*----------------------------------------------------------------------

  add_reserved_R6

  Add reserved names in enclosing environment specific to R6
  to be skipped as global variables.

*/
void add_reserved_R6(SEXP enclos)
{
    const char *nms[3] = {"self", "private", "super"};
    for (int i = 0; i < 3; i++)
        Rf_defineVar(Rf_install(nms[i]), R_NilValue, enclos);
}
