#define R_NO_REMAP

#include "checkglobals.h"

// substitutes Rf_isValidString
#define validString(x) TYPEOF(x) == STRSXP &&LENGTH(x) > 0 && TYPEOF(STRING_ELT(x, 0)) != NILSXP

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
            if (strcmp(carchar, "::") == 0 || strcmp(carchar, ":::") == 0)
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
            else if (validString(argarg) && Rf_length(argarg) == 1)
                sym = Rf_installChar(STRING_ELT(argarg, 0));
            UNPROTECT(1);
        }
        else if (validString(cararg) && Rf_length(cararg) == 1)
            sym = Rf_installChar(STRING_ELT(cararg, 0));
    }
    else if (validString(arg) && Rf_length(arg) == 1)
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
        if (validString(vars))
            for (int j = 0; j < Rf_length(vars); j++)
                Rf_defineVar(Rf_installChar(STRING_ELT(vars, j)), R_NilValue, env0);
    }
    else if (verbose)
        Rprintf("ERROR: failed to evaluate call to globalVariables\n");
    UNPROTECT(2);
}

/*----------------------------------------------------------------------

   special_funs

   Handle special functions .onLoad and .onAttach by skipping
   function closure forcing assignment of local variable definitions to
   root environment.

 */
void special_funs(SEXP op, const char *opchar, SEXP call, SEXP rho, SEXP env0, R_args *args)
{
    SEXP sym = NULL;
    PROTECT_INDEX ipx = 0;
    int nprotect = 0;

    if (strcmp(opchar, "<-") == 0 || strcmp(opchar, "=") == 0 || strcmp(opchar, "<<-") == 0)
    {
        PROTECT_WITH_INDEX(sym = CADR(call), &ipx);
        nprotect++;
    }
    else if (strcmp(opchar, "assign") == 0 || strcmp(opchar, "delayedAssign") == 0)
    {
        PROTECT_WITH_INDEX(sym = matcharg_bypos(op, call, rho, 0), &ipx);
        nprotect++;
    }

    if (sym)
    {
        if (TYPEOF(sym) == STRSXP && Rf_length(sym) == 1)
            REPROTECT(sym = Rf_installChar(STRING_ELT(sym, 0)), ipx);
        if (Rf_isSymbol(sym) && (strcmp(CHAR(PRINTNAME(sym)), ".onLoad") == 0 || strcmp(CHAR(PRINTNAME(sym)), ".onAttach") == 0))
        {
            if (args->verbose)
                Rprintf("SPECIAL SYMBOL: %s\n", CHAR(PRINTNAME(sym)));
            args->skip_closure = TRUE;
        }
    }
    UNPROTECT(nprotect);
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
    if (!evalerror && validString(pkgstr) && Rf_length(pkgstr) == 1)
    {
        if (verbose)
            Rprintf("PKG_LOAD: %s\n", CHAR(STRING_ELT(pkgstr, 0)));
        SEXP pkgs = PROTECT(R_getVarEx1(Rf_install(".__pkgs__"), envi, TRUE));
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
void inline_fun(SEXP call, SEXP enclos, R_args *args)
{
    SEXP funargs = CADR(call);
    if (TYPEOF(funargs) == LISTSXP)
    {
        while (!Rf_isNull(funargs))
        {
            if (args->verbose)
                Rprintf("SYMBOL_FORMALS: %s\n", CHAR(PRINTNAME(TAG(funargs))));
            Rf_defineVar(TAG(funargs), R_NilValue, enclos);
            funargs = CDR(funargs);
        }
    }
    Rf_defineVar(Rf_install(".__closure__"), PROTECT(Rf_ScalarLogical(TRUE)), enclos);
    UNPROTECT(1);
    if (args->skip_closure)
    {
        Rf_defineVar(Rf_install(".__closure__"), PROTECT(Rf_ScalarLogical(FALSE)), enclos);
        UNPROTECT(1);
        args->skip_closure = FALSE;
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

  exit_expr

  Handle `on.exit` calls by replacing expression for evaluation
  as final call in enclosing environment

*/
void exit_expr(SEXP call, SEXP enclos, R_args *args)
{
    if ((args->pending_exit)[0] == 0)
    {
        SEXP parent_env = enclos;
        int parent_lvl = 0;
        SEXP closure = NULL;
        PROTECT_INDEX ipx = 0;
        PROTECT_WITH_INDEX(closure = R_getVarEx1(Rf_install(".__closure__"), parent_env, FALSE), &ipx);
        Rboolean isclosure = (closure != R_UnboundValue) ? LOGICAL_ELT(closure, 0) : FALSE;
        while (!isclosure)
        {
            parent_env = R_ParentEnv(parent_env);
            REPROTECT(closure = R_getVarEx1(Rf_install(".__closure__"), parent_env, FALSE), ipx);
            isclosure = (closure != R_UnboundValue) ? LOGICAL_ELT(closure, 0) : FALSE;
            parent_lvl -= 1;
        }
        if (args->verbose)
            Rprintf("SPECIAL SYMBOL: on.exit\n");
        SEXP expr = PROTECT(CADR(call));
        Rf_defineVar(Rf_install("on.exit"), PROTECT(Rf_duplicate(expr)), parent_env);
        SETCADR(call, Rf_install("on.exit")); // placeholder term
        (args->pending_exit)[0] = 1;
        (args->pending_exit)[1] = parent_lvl;
        (args->pending_exit)[2] = 0;
        UNPROTECT(3);
    }
    else if (args->verbose)
    {
        // TODO: find example where this is reached
        Rprintf("ERROR: multiple and/or nested on.exit calls in single closure\n");
    }
}

/*----------------------------------------------------------------------

  fun_call

  Search enclosing environment to replace function call by existing
  function definition.

*/
void fun_call(SEXP op, SEXP call, SEXP enclos)
{
    SEXP fun = PROTECT(R_getVarEx1(op, enclos, TRUE));
    SEXP funbase = PROTECT(R_getVarEx1(op, R_BaseEnv, TRUE));

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
        if (R_ParentEnv(enclos) != env0)
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
        /*
            NOTE: It is too cumbersome to assign variables created with assign, delayedAssign, ...
            exactly to the correct environments. As a consequence, these variables may be falsely
            labeled as unrecognized globals, This is safer than the alternative of assigning
            variables to the root environment by default if they do not belong there.
        */
        if (verbose)
            Rprintf("SYMBOL: %s\n", CHAR(PRINTNAME(sym)));
        SEXP parent_env = enclos;
        SEXP closure = NULL;
        PROTECT_INDEX ipx1 = 0;
        PROTECT_WITH_INDEX(closure = R_getVarEx1(Rf_install(".__closure__"), parent_env, FALSE), &ipx1);
        nprotect++;
        Rboolean isclosure = (closure != R_UnboundValue) ? LOGICAL_ELT(closure, 0) : FALSE;
        while (!isclosure)
        {
            parent_env = R_ParentEnv(parent_env);
            REPROTECT(closure = R_getVarEx1(Rf_install(".__closure__"), parent_env, FALSE), ipx1);
            isclosure = (closure != R_UnboundValue) ? LOGICAL_ELT(closure, 0) : FALSE;
        }
        if (strcmp(opchar, "<<-") == 0 && parent_env != env0)
        {
            SEXP val0 = NULL;
            PROTECT_INDEX ipx2 = 0;
            PROTECT_WITH_INDEX(val0 = R_getVarEx1(sym, parent_env, FALSE), &ipx2);
            Rboolean notinframe = (val0 == R_UnboundValue && parent_env != env0);
            while (notinframe)
            {
                parent_env = R_ParentEnv(parent_env);
                REPROTECT(val0 = R_getVarEx1(sym, parent_env, FALSE), ipx2);
                notinframe = (val0 == R_UnboundValue && parent_env != env0);
            }
            UNPROTECT(1);
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
        SEXP pkg0 = PROTECT(R_getVarEx1(pkgfun, envi, FALSE));
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
        SEXP srcfun = PROTECT(R_getVarEx1(pkgfun, srcrefi, FALSE));
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
        SEXP nmsrc = PROTECT(R_getVarEx1(Rf_install(".__srcref__"), enclos, TRUE));
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
void func_call(SEXP op, SEXP call, SEXP rho, int func_id, const char *parent_opchar)
{
    SEXP actualpos = NULL;
    const char *argname = functional_argnms[func_id];
    int argpos = functional_argpos[func_id];
    /* NOTE: we choose to handle %>% explicitly.
       |> does not need to be handled separately,
       since syntax tree is the same with or without
       a pipe.
    */
    if (strcmp(parent_opchar, "%>%") == 0)
        argpos -= 1;

    if (func_id >= 19 && func_id <= 25)
        actualpos = PROTECT(matcharg_bynamepos(op, call, R_NilValue, formals_parallel[func_id - 19], argname, argpos - 1));
    else
        actualpos = PROTECT(matcharg_bynamepos(op, call, rho, NULL, argname, argpos - 1));

    if (!Rf_isNull(actualpos))
        argpos = INTEGER_ELT(actualpos, 0);

    if (argpos > 0)
    {
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
