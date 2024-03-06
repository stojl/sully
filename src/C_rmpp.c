#include <R.h>
#include <Rinternals.h>
#include <R_ext/Random.h> // ACCESS TO UNIF NUMBER GENERATOR
#include <R_ext/Utils.h> // ACCESS TO revsort

static void sample(int n, double *p, int *perm, int nans, int *ans)
{
  double rU;
  int i, j;
  int nm1 = n - 1;

  /* record element identities */
  for (i = 0; i < n; i++)
    perm[i] = i + 1;

  /* sort the probabilities into descending order */
  revsort(p, perm, n);

  /* compute cumulative probabilities */
  for (i = 1 ; i < n; i++)
    p[i] += p[i - 1];

  /* compute the sample */
  for (i = 0; i < nans; i++) {
    rU = unif_rand();
    for (j = 0; j < nm1; j++) {
      if (rU <= p[j])
        break;
    }
    ans[i] = perm[j];
  }
}

SEXP call_f(SEXP calls, SEXP env, SEXP ts, SEXP ys, double s, double *ts_, int *ys_, int c) {
  SEXP call_t, call_s;
  call_t = call_s = PROTECT(allocList(5));
  SET_TYPEOF(call_s, LANGSXP);
  SETCAR(call_t, VECTOR_ELT(calls, ys_[c - 1] - 1)); call_t = CDR(call_t);
  SETCAR(call_t, PROTECT(ScalarReal(s))); call_t = CDR(call_t);
  SETCAR(call_t, ts); call_t = CDR(call_t);
  SETCAR(call_t, ys); call_t = CDR(call_t);
  SETCAR(call_t, PROTECT(ScalarInteger(c))); call_t = CDR(call_t);
  SEXP out = eval(call_s, env);
  UNPROTECT(3);
  return out;
}

SEXP C_rmpp(SEXP n,
            SEXP rates,
            SEXP drates,
            SEXP probs,
            SEXP t0,
            SEXP tn,
            SEXP y0,
            SEXP mark_end,
            SEXP limit,
            SEXP env) {
  double t0_ = REAL(t0)[0];
  double tn_ = REAL(tn)[0];
  int n_ = INTEGER(n)[0];
  int y0_ = INTEGER(y0)[0];
  int M = INTEGER(limit)[0];
  int *perms = R_Calloc(LENGTH(rates), int);

  int mark_end_ = (TYPEOF(mark_end) == NILSXP) ? 0 : 1;
  SEXP result = PROTECT(allocVector(VECSXP, n_));
  size_t k = 4;
  SEXP ts, ys;
  PROTECT(ts = allocVector(REALSXP, k));
  PROTECT(ys = allocVector(INTSXP, k));
  double *ts_ = REAL(ts);
  int *ys_ = INTEGER(ys);

  GetRNGstate();
  for(R_xlen_t l = 0; l < n_; ++l) {
    R_xlen_t c = 1;
    ts_[0] = t0_; ys_[0] = y0_;

    double s = t0_;
    int super_break = 0;

    while(super_break == 0) {
      if(c == M) {
        break;
      }
      double dom;
      if(c >= k) {
        k *= 2;
        // Reallocation
        double *ts_tmp = ts_;
        SEXP ts1, ys1;
        ts1 = ts; ys1 = ys;
        int *ys_tmp = ys_;
        size_t g = LENGTH(ts);
        PROTECT(ts = allocVector(REALSXP, k));
        PROTECT(ys = allocVector(INTSXP, k));
        ts_ = REAL(ts); ys_ = INTEGER(ys);
        for(R_xlen_t i = 0; i < g; ++i) {
          ts_[i] = ts_tmp[i];
          ys_[i] = ys_tmp[i];
        }
        UNPROTECT_PTR(ts1); UNPROTECT_PTR(ys1);
        if(k > M)
          k = M;
      }

      dom = REAL(call_f(drates, env, ts, ys, ts_[c - 1], ts_, ys_, c))[0];

      if(dom == 0) {
        break;
      }

      while(1) {
        double u;
        double mu_dot;
        double min_s;
        s += exp_rand() / dom;

        min_s = (s > tn_) ? tn_ : s;

        mu_dot = REAL(call_f(rates, env, ts, ys, min_s, ts_, ys_, c))[0];

        if(mu_dot == 0) {
          super_break = 1;
          break;
        }

        if(s > tn_) {
          if(mark_end_) {
            ts_[c] = tn_;
            ys_[c] = INTEGER(mark_end)[0];
            c += 1;
          }
          super_break = 1;
          break;
        }

        u = unif_rand();

        if(u <= mu_dot / dom) {
          SEXP pp;
          double *p;

          pp = call_f(probs, env, ts, ys, s, ts_, ys_, c);
          p = REAL(pp);

          ts_[c] = s;

          int nn = LENGTH(pp);
          double ss = 0;
          for(int i = 0; i < nn; ++i) {
            ss += p[i];
          }
          for(int j = 0; j < nn; ++j) {
            p[j] /= ss;
          }

          sample(LENGTH(pp), p, perms, 1, ys_ + c);
          c += 1;
          break;
        }
      }
    }

    SEXP out_1 = PROTECT(allocVector(REALSXP, c));
    double *out_1_ = REAL(out_1);

    SEXP out_2 = PROTECT(allocVector(INTSXP, c));
    int *out_2_ = INTEGER(out_2);

    SEXP out = PROTECT(allocVector(VECSXP, 3));

    for(R_xlen_t i = 0; i < c; ++i) {
      out_1_[i] = ts_[i];
      out_2_[i] = ys_[i];
    }

    SET_VECTOR_ELT(out, 0, out_1);
    SET_VECTOR_ELT(out, 1, out_2);
    SET_VECTOR_ELT(out, 2, PROTECT(ScalarInteger(c - 1)));
    SET_VECTOR_ELT(result, l, out);
    UNPROTECT(4);
  }
  PutRNGstate();
  R_Free(perms);
  //UNPROTECT(3 * n_ + 3);
  UNPROTECT(3);
  return result;
}
