#define R_NO_REMAP

#include "checkglobals.h"

#define INTBITS (8 * (R_len_t)sizeof(R_len_t))

const char *functionals[33] = {
    "apply", "eapply", "Reduce", "Filter", "Map", "Negate", "Position",
    "Find", "lapply", "rapply", "mapply", ".mapply", "match.fun",
    ".S3method", "outer", "sapply", "vapply", "sweep", "tapply",
    "parSapply", "parSapplyLB", "parApply", "mclapply", "mcmapply",
    "mcMap", "pvec", "aggregate", "dendrapply", "integrate", "by",
    "do.call", "kronecker", ".kronecker"};

const char *functional_argnms[33] = {
    "FUN", "FUN", "f", "f", "f", "f", "f", "f", "FUN", "f", "FUN",
    "FUN", "FUN", "method", "FUN", "FUN", "FUN", "FUN", "FUN", "FUN",
    "FUN", "FUN", "FUN", "FUN", "f", "FUN", "FUN", "FUN", "f", "FUN",
    "what", "FUN", "FUN"};

const int functional_argpos[33] = {3, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 3, 3, 2, 2, 4, 3, 3, 3, 4, 2, 1, 1, 2, 3, 2, 1, 3, 1, 3, 3};

const char *skip_nms[7] = {
    "substitute", "~", "expression", "quote", "bquote", "Quote", "alist"};

const char *compiled_nms[7] = {
    ".Call", ".C", ".External", ".Fortran", ".External2", ".Call.graphics", ".External.graphics"};

const char *assign_nms[9] = {
    "<-", "=", "<<-", "for", "assign", "delayedAssign", "setMethod", "setGeneric", "makeActiveBinding"};

const char *formals_parallel[7][12] = {
    // parSapply
    {"cl", "X", "FUN", "...", "simplify", "USE.NAMES", "chunk.size", NULL, NULL, NULL, NULL, NULL},
    // parSapplyLB
    {"cl", "X", "FUN", "...", "simplify", "USE.NAMES", "chunk.size", NULL, NULL, NULL, NULL, NULL},
    // parApply
    {"cl", "X", "MARGIN", "FUN", "...", "chunk.size", NULL, NULL, NULL, NULL, NULL, NULL},
    // mclapply
    {"X", "FUN", "...", "mc.preschedule", "mc.set.seed", "mc.silent", "mc.cores", "mc.cleanup", "mc.allow.recursive", "affinity.list", NULL, NULL},
    // mcmapply
    {"FUN", "...", "MoreArgs", "SIMPLIFY", "USE.NAMES", "mc.preschedule", "mc.set.seed", "mc.silent", "mc.cores", "mc.cleanup", "affinity.list", NULL},
    // mcMap
    {"f", "...", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
    // pvec
    {"v", "FUN", "...", "mc.set.seed", "mc.silent", "mc.cores", "mc.cleanup", NULL, NULL, NULL, NULL, NULL}};

/* set bit in array */
static void setbit(R_len_t *array, R_len_t el)
{
    array[el / INTBITS] |= 1 << (el % INTBITS);
}

/* test bit in array */
static int testbit(R_len_t *array, R_len_t el)
{
    return array[el / INTBITS] & (1 << (el % INTBITS));
}

/* partially match argument position based on function formals */
static SEXP matcharg_rho(SEXP op, SEXP call, SEXP actuals, SEXP rho, int argpos)
{
    int nprotect = 1;
    SEXP fun = PROTECT(Rf_findFun(op, rho));

    if (fun != R_UnboundValue && !Rf_isPrimitive(fun))
    {
        SEXP formals = FORMALS(fun);
        int narg = 0;
        int dots = -1;
        while (!Rf_isNull(formals))
        {
            if (!strcmp(CHAR(PRINTNAME(TAG(formals))), "..."))
                dots = narg;
            formals = CDR(formals);
            narg++;
        }
        R_len_t *matched = (R_len_t *)S_alloc(narg / INTBITS + 1, sizeof(R_len_t));
        if (dots >= 0)
            setbit(matched, dots);

        // first pass (partially) match named arguments
        int i = 0, match = 0;
        const char *target = "";
        const char *tag = "";
        SEXP callptr = call;
        SEXP actualj = NULL;
        for (int j = 1; j < Rf_length(actuals); j++)
        {
            callptr = CDR(callptr);
            actualj = STRING_ELT(actuals, j);
            if (LENGTH(actualj))
            {
                tag = CHAR(actualj);
                formals = FORMALS(fun);
                match = -1;
                i = 0;
                while (!Rf_isNull(formals))
                {
                    target = CHAR(PRINTNAME(TAG(formals)));
                    if (strncmp(target, tag, strlen(tag)) == 0)
                    {
                        if (match < 0)
                            match = i;
                        if (strlen(target) == strlen(tag))
                        {
                            match = i;
                            break;
                        }
                    }
                    formals = CDR(formals);
                    i++;
                }
                if (match >= 0)
                {
                    setbit(matched, match);
                    if (match == argpos)
                    {
                        // not reached
                        UNPROTECT(nprotect);
                        return Rf_ScalarInteger(j);
                    }
                }
            }
        }

        // second pass match unnamed arguments
        match = 0;
        callptr = call;
        for (int j = 1; j < Rf_length(actuals); j++)
        {
            callptr = CDR(callptr);
            actualj = STRING_ELT(actuals, j);
            if (!LENGTH(actualj))
            {
                while (testbit(matched, match))
                    match++;
                setbit(matched, match);
                if (match == argpos)
                {
                    UNPROTECT(nprotect);
                    return Rf_ScalarInteger(j);
                }
            }
        }
    }
    // no function
    UNPROTECT(nprotect);
    return R_NilValue;
}

/* partially match argument position based on formals table */
static SEXP matcharg_formals(SEXP call, SEXP actuals, const char **formals, int argpos)
{
    int nprotect = 0, narg = 0, dots = -1;
    while (formals[narg] != NULL)
    {
        if (!strcmp(formals[narg], "..."))
            dots = narg;
        narg++;
    }
    R_len_t *matched = (R_len_t *)S_alloc(narg / INTBITS + 1, sizeof(R_len_t));
    if (dots >= 0)
        setbit(matched, dots);

    // first pass (partially) match named arguments
    int match = 0;
    const char *tag = "";
    SEXP callptr = call;
    SEXP actualj = NULL;
    for (int j = 1; j < Rf_length(actuals); j++)
    {
        callptr = CDR(callptr);
        actualj = STRING_ELT(actuals, j);
        if (LENGTH(actualj))
        {
            tag = CHAR(actualj);
            match = -1;
            for (int i = 0; i < narg; i++)
            {
                if (strncmp(formals[i], tag, strlen(tag)) == 0)
                {
                    if (match < 0)
                        match = i;
                    if (strlen(formals[i]) == strlen(tag))
                    {
                        match = i;
                        break;
                    }
                }
            }
            if (match >= 0)
            {
                setbit(matched, match);
                if (match == argpos)
                {
                    // not reached
                    UNPROTECT(nprotect);
                    return Rf_ScalarInteger(j);
                }
            }
        }
    }

    // second pass match unnamed arguments
    match = 0;
    callptr = call;
    for (int j = 1; j < Rf_length(actuals); j++)
    {
        callptr = CDR(callptr);
        actualj = STRING_ELT(actuals, j);
        if (!LENGTH(actualj))
        {
            while (testbit(matched, match))
                match++;
            setbit(matched, match);
            if (match == argpos)
            {
                UNPROTECT(nprotect);
                return Rf_ScalarInteger(j);
            }
        }
    }
    // no match found
    UNPROTECT(nprotect);
    return R_NilValue;
}

/*----------------------------------------------------------------------

   ddval ("dot-dot-value")
   a function to take a name and determine if it is of the form
   ..x where x is an integer; if so x is returned otherwise 0 is returned.
   Original function definition from R-source ../envir.c.

*/
int ddval(SEXP symbol)
{
    const char *buf;
    char *endp;
    int rval;
    buf = CHAR(PRINTNAME(symbol));
    if (!strncmp(buf, "..", 2) && strlen(buf) > 2)
    {
        buf += 2;
        rval = (int)strtol(buf, &endp, 10);
        if (*endp != '\0')
            // not reached
            return 0;
        else
            return rval;
    }
    return 0;
}

/*----------------------------------------------------------------------

  strmatch

  Matches a target string to a table of reference strings. Returns the
  position of the matching string in the table if an exact match is found,
  or -1 otherwise.

*/
int strmatch(const char *target, const char **table, int len)
{
    int match = -1;
    for (int j = 0; j < len; j++)
    {
        if (strcmp(target, table[j]) == 0)
        {
            match = j;
            break;
        }
    }
    return match;
}

/*----------------------------------------------------------------------

  matcharg_bypos

  Partially match call argument value given its position based on function formals.
  If function is not a CLOSXP, returns call argument by position.

*/
SEXP matcharg_bypos(SEXP op, SEXP call, SEXP rho, int argpos)
{
    SEXP arg = NULL;
    int nprotect = 1;

    // get formals
    SEXP fun = PROTECT(Rf_findFun(op, rho));

    if (fun != R_UnboundValue && !Rf_isPrimitive(fun))
    {
        // get tag at position p
        SEXP formals = FORMALS(fun);
        int i = 0;
        while (i < argpos)
        {
            formals = CDR(formals);
            i++;
        }
        const char *target = CHAR(PRINTNAME(TAG(formals)));

        // match found tag in call
        SEXP actuals = PROTECT(Rf_getAttrib(call, R_NamesSymbol));
        SEXP actualj = NULL;
        R_len_t n = Rf_length(actuals);
        nprotect++;

        if (!Rf_isNull(actuals))
        {
            const char *tag = "";
            SEXP callptr = call;
            int match = 0;
            // fast scan
            for (int j = 1; j < n; j++)
            {
                callptr = CDR(callptr);
                actualj = STRING_ELT(actuals, j);
                if (LENGTH(actualj))
                {
                    tag = CHAR(actualj);
                    if (!strcmp(target, tag))
                    {
                        arg = CAR(callptr);
                        match = 1;
                    }
                }
            }
            if (match)
            {
                UNPROTECT(nprotect);
                return arg;
            }
            else // slow scan
            {
                int narg = 0;
                int dots = -1;
                formals = FORMALS(fun);
                while (!Rf_isNull(formals))
                {
                    if (!strcmp(CHAR(PRINTNAME(TAG(formals))), "..."))
                        dots = narg;
                    formals = CDR(formals);
                    narg++;
                }
                R_len_t *matched = (R_len_t *)S_alloc(narg / INTBITS + 1, sizeof(R_len_t));
                if (dots >= 0)
                    setbit(matched, dots);

                // first pass (partially) match named arguments
                int i = 0;
                callptr = call;
                for (int j = 1; j < n; j++)
                {
                    callptr = CDR(callptr);
                    actualj = STRING_ELT(actuals, j);
                    if (LENGTH(actualj))
                    {
                        tag = CHAR(actualj);
                        formals = FORMALS(fun);
                        match = -1;
                        i = 0;
                        while (!Rf_isNull(formals))
                        {
                            target = CHAR(PRINTNAME(TAG(formals)));
                            if (strncmp(target, tag, strlen(tag)) == 0)
                            {
                                if (match < 0)
                                    match = i;
                                if (strlen(target) == strlen(tag))
                                {
                                    match = i;
                                    break;
                                }
                            }
                            formals = CDR(formals);
                            i++;
                        }
                        if (match >= 0)
                        {
                            setbit(matched, match);
                            if (match == argpos)
                            {
                                arg = CAR(callptr);
                                UNPROTECT(nprotect);
                                return arg;
                            }
                        }
                    }
                }

                // second pass match unnamed arguments
                match = 0;
                callptr = call;
                for (int j = 1; j < n; j++)
                {
                    callptr = CDR(callptr);
                    actualj = STRING_ELT(actuals, j);
                    if (!LENGTH(actualj))
                    {
                        while (testbit(matched, match))
                            match++;
                        setbit(matched, match);
                        if (match == argpos)
                        {
                            arg = CAR(callptr);
                            UNPROTECT(nprotect);
                            return arg;
                        }
                    }
                }
                // no match found
                UNPROTECT(nprotect);
                return R_NilValue;
            }
        }
    }
    // match by position
    int k = -1;
    while (k < argpos && !Rf_isNull(call))
    {
        call = CDR(call);
        arg = CAR(call);
        k++;
    }
    UNPROTECT(nprotect);
    return arg;
}

/*----------------------------------------------------------------------

  matcharg_bynamepos

  Partially match call argument value first by name based on call argument
  names and second by position based on function formals. Returns argument
  position if match is found and NULL otherwise.

*/
SEXP matcharg_bynamepos(SEXP op, SEXP call, SEXP rho, const char **formals, const char *argname, int argpos)
{
    SEXP arg = NULL;
    int nprotect = 1;

    SEXP actuals = PROTECT(Rf_getAttrib(call, R_NamesSymbol));
    SEXP actualj = NULL;
    R_len_t n = Rf_length(actuals);

    if (!Rf_isNull(actuals))
    {
        SEXP callptr = call;
        const char *tag = "";
        int match = 0;
        // fast scan
        for (int j = 1; j < n; j++)
        {
            callptr = CDR(callptr);
            actualj = STRING_ELT(actuals, j);
            if (LENGTH(actualj))
            {
                tag = CHAR(actualj);
                if (strncmp(argname, tag, strlen(tag)) == 0)
                {
                    if (match < 1 || strlen(argname) == strlen(tag))
                    {
                        match = j;
                        if (strlen(argname) == strlen(tag))
                            break;
                    }
                }
            }
        }
        if (match)
        {
            UNPROTECT(nprotect);
            return Rf_ScalarInteger(match);
        }
        else // slow scan
        {
            if (!Rf_isNull(rho))
                arg = PROTECT(matcharg_rho(op, call, actuals, rho, argpos));
            else
                arg = PROTECT(matcharg_formals(call, actuals, formals, argpos));
            UNPROTECT(nprotect + 1);
            return arg;
        }
    }
    // no argument names
    UNPROTECT(nprotect);
    return R_NilValue;
}
