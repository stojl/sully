#include <R.h>
#include <Rinternals.h>

SEXP C_summary_rmpp(SEXP x, SEXP discard_initial) {
  int n = LENGTH(x);
  R_xlen_t total = 0;
  for(R_xlen_t i = 0; i < n; ++i) {
    total += LENGTH(VECTOR_ELT(VECTOR_ELT(x, i), 0));
  }

  SEXP paths = PROTECT(allocVector(INTSXP, total));
  SEXP times = PROTECT(allocVector(REALSXP, total));
  SEXP marks = PROTECT(allocVector(INTSXP, total));

  int *l_ = INTEGER(paths);
  int *m_ = INTEGER(marks);
  double *t_ = REAL(times);
  R_xlen_t k = 0;
  for(R_xlen_t i = 0; i < n; ++i) {
    SEXP path = VECTOR_ELT(x, i);
    R_xlen_t s = LENGTH(VECTOR_ELT(path, 0));
    double *tt = REAL(VECTOR_ELT(path, 0));
    int *mm = INTEGER(VECTOR_ELT(path, 1));

    for(R_xlen_t j = 0; j < s; ++j, ++k) {
      l_[k] = i;
      m_[k] = mm[j];
      t_[k] = tt[j];
    }
  }

  SEXP result = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(result, 0, paths);
  SET_VECTOR_ELT(result, 1, times);
  SET_VECTOR_ELT(result, 2, marks);
  UNPROTECT(4);
  return result;
}
