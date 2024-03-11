#include <R.h>
#include <Rinternals.h>

SEXP C_summary_rmpp(SEXP x, SEXP discard_initial) {
  int n = LENGTH(x);
  R_xlen_t total = 0;
  int init = INTEGER(discard_initial)[0];
  for(R_xlen_t i = 0; i < n; ++i) {
    total += LENGTH(VECTOR_ELT(VECTOR_ELT(x, i), 0)) - init;
  }

  R_len_t length = LENGTH(VECTOR_ELT(x, 0));
  SEXP result, id, tt, yy;
  PROTECT(result = allocVector(VECSXP, length + 1));
  PROTECT(id = allocVector(INTSXP, total));
  PROTECT(tt = allocVector(REALSXP, total));
  PROTECT(yy = allocVector(INTSXP, total));

  // Path id
  for(R_len_t i = 0, k = 0; i < n; ++i) {
    SEXP path = VECTOR_ELT(x, i);
    R_len_t s = LENGTH(VECTOR_ELT(path, 0));
    int *l = INTEGER(id);
    double *t = REAL(tt);
    int *y = INTEGER(yy);
    double *tt_ = REAL(VECTOR_ELT(path, 0));
    int *yy_ = INTEGER(VECTOR_ELT(path, 1));
    for(R_len_t j = init; j < s; ++k, ++j) {
      l[k] = i;
      t[k] = tt_[j];
      y[k] = yy_[j];
    }
  }

  SET_VECTOR_ELT(result, 0, id);
  SET_VECTOR_ELT(result, length - 1, tt);
  SET_VECTOR_ELT(result, length, yy);

  for(R_len_t h = 0; h < length - 2; ++h) {
    SEXP prop;
    unsigned int type = TYPEOF(VECTOR_ELT(VECTOR_ELT(x, 0), h + 2));
    PROTECT(prop = allocVector(type, total));

    for(R_len_t k = 0, i = 0; i < n; ++i) {
      SEXP path = VECTOR_ELT(x, i);
      SEXP attr = VECTOR_ELT(path, h + 2);
      R_len_t s = LENGTH(VECTOR_ELT(path, 0));

      for(R_len_t j = init; j < s; ++j, ++k) {
        switch(type) {
        case REALSXP:
          REAL(prop)[k] = REAL(attr)[0];
          break;
        case INTSXP:
          INTEGER(prop)[k] = INTEGER(attr)[0];
        }
      }
    }
    SET_VECTOR_ELT(result, h + 1, prop);
  }
  SEXP names, arg_names;
  PROTECT(names = getAttrib(VECTOR_ELT(x, 0), R_NamesSymbol));
  PROTECT(arg_names = allocVector(STRSXP, length + 1));

  SET_STRING_ELT(arg_names, 0, mkChar("path"));
  SET_STRING_ELT(arg_names, length - 1, mkChar("time"));
  SET_STRING_ELT(arg_names, length, mkChar("mark"));
  for(R_len_t i = 0; i < length - 2; ++i) {
    SET_STRING_ELT(arg_names, i + 1, STRING_ELT(names, i + 2));
  }
  setAttrib(result, R_NamesSymbol, arg_names);

  UNPROTECT(4 + length);
  return result;
}
