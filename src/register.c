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
                   SEXP args,
                   SEXP env);

extern SEXP C_summary_rmpp(SEXP x, SEXP discard_intial);

// REGISTER ROUTINES

static const R_CallMethodDef R_CallDef[] = {
  {"C_rmpp", (DL_FUNC) &C_rmpp, 11},
  {"C_summary_rmpp", (DL_FUNC) &C_summary_rmpp, 2},
  {NULL, NULL, 0}
};

void R_init_sully(DllInfo *dll) {
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, TRUE);
}
