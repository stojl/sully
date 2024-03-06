#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP C_rmpp(SEXP n,
                   SEXP rates,
                   SEXP drates,
                   SEXP probs,
                   SEXP t0,
                   SEXP tn,
                   SEXP y0,
                   SEXP mark_end,
                   SEXP limit,
                   SEXP env);

// REGISTER ROUTINES

static const R_CallMethodDef R_CallDef[] = {
  {"C_rmpp", (DL_FUNC) &C_rmpp, 10},
  {NULL, NULL, 0}
};

void R_init_CompStatR(DllInfo *dll) {
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
}
