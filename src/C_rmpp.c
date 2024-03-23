#include <R.h>
#include <Rinternals.h>
#include <R_ext/Random.h> // ACCESS TO UNIF NUMBER GENERATOR
#include <R_ext/Utils.h> // ACCESS TO revsort
#include <R_ext/Error.h> // ACCESS TO warning

// Taken from the implementation of base::sample.
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

SEXP call_f(SEXP calls,
            SEXP env,
            SEXP ts,
            SEXP ys,
            SEXP arglist,
            SEXP names,
            double s,
            double *ts_,
            int *ys_,
            int c) {
  SEXP call_t, call_s;
  R_len_t arg_length = 0;
  if(TYPEOF(arglist) != NILSXP)
    arg_length = LENGTH(arglist);
  call_t = call_s = PROTECT(allocList(5 + arg_length));
  SET_TYPEOF(call_s, LANGSXP);
  SETCAR(call_t, VECTOR_ELT(calls, ys_[c - 1] - 1)); call_t = CDR(call_t);
  SETCAR(call_t, PROTECT(ScalarReal(s))); call_t = CDR(call_t);
  SETCAR(call_t, ts); call_t = CDR(call_t);
  SETCAR(call_t, ys); call_t = CDR(call_t);
  SETCAR(call_t, PROTECT(ScalarInteger(c))); call_t = CDR(call_t);
  if(TYPEOF(arglist) != NILSXP) {
    for(int i = 0; i < arg_length; ++i) {
      SETCAR(call_t, VECTOR_ELT(arglist, i));
      SET_TAG(call_t, installChar(STRING_ELT(names, i)));
      call_t = CDR(call_t);
    }
  }
  SEXP out = eval(call_s, env);
  UNPROTECT(3);
  return out;
}

void set_args(SEXP *arglist, SEXP *args, R_len_t idx) {
  int n = LENGTH(*arglist);

  for(R_len_t i = 0; i < n; ++i) {
    SEXP entry, arg;
    R_len_t k;

    arg = VECTOR_ELT(*args, i);
    switch(LENGTH(arg)) {
    case 1:
      k = 0;
      break;
    default:
      k = idx;
    }
    switch(TYPEOF(arg)) {
    case REALSXP:
      PROTECT(entry = ScalarReal(REAL(arg)[k]));
      break;
    case INTSXP:
      PROTECT(entry = ScalarInteger(INTEGER(arg)[k]));
      break;
    default:
      PROTECT(entry = allocVector(NILSXP, 1));
    }

    SET_VECTOR_ELT(*arglist, i, entry);
  }
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
            SEXP args,
            SEXP env) {
  double *t0_ = REAL(t0);
  double *tns = REAL(tn);
  int n_ = INTEGER(n)[0];
  int *y0_ = INTEGER(y0);
  int M = INTEGER(limit)[0];
  int *perms = R_Calloc(LENGTH(rates), int);

  /* buffers */
  PROTECT_INDEX t_idx, y_idx;
  SEXP ts, ys;
  R_len_t k = 4;
  PROTECT_WITH_INDEX(ts = allocVector(REALSXP, k), &t_idx);
  PROTECT_WITH_INDEX(ys = allocVector(INTSXP, k), &y_idx);
  double *ts_ = REAL(ts);
  int *ys_ = INTEGER(ys);

  int mark_end_ = (TYPEOF(mark_end) == NILSXP) ? 0 : 1;
  SEXP result = PROTECT(allocVector(VECSXP, n_));


  SEXP names, arglist, arg_names;
  R_len_t arg_length = 0;

  if(TYPEOF(args) != NILSXP) {
    arg_length = LENGTH(args);
    PROTECT(names = getAttrib(args, R_NamesSymbol));
    PROTECT(arglist = allocVector(VECSXP, arg_length));
    PROTECT(arg_names = allocVector(STRSXP, arg_length + 5));
    for(R_len_t i = 0; i < arg_length; ++i) {
      SET_STRING_ELT(arg_names, i + 5, STRING_ELT(names, i));
    }
  } else {
    PROTECT(names = allocVector(NILSXP, 1));
    PROTECT(arglist = allocVector(NILSXP, 1));
    PROTECT(arg_names = allocVector(STRSXP, 5));
  }
  SET_STRING_ELT(arg_names, 0, mkChar("t"));
  SET_STRING_ELT(arg_names, 1, mkChar("y"));
  SET_STRING_ELT(arg_names, 2, mkChar("t0"));
  SET_STRING_ELT(arg_names, 3, mkChar("y0"));
  SET_STRING_ELT(arg_names, 4, mkChar("tn"));

  R_len_t t0_length = LENGTH(t0);
  R_len_t y0_length = LENGTH(y0);
  R_len_t tn_length = LENGTH(tn);

  double exceed;
  int was_warning = 0;

  GetRNGstate();
  for(R_xlen_t l = 0; l < n_; ++l) {
    R_xlen_t c = 1;
    ts_[0] = t0_length == 1 ? t0_[0] : t0_[l];
    ys_[0] = y0_length == 1 ? y0_[0] : y0_[l];
    double tn_ = tn_length == 1 ? tns[0] : tns[l];

    if(TYPEOF(args) != NILSXP)
      set_args(&arglist, &args, l);

    double s = ts_[0];
    int super_break = 0; // Used to exit out of double while loop

    while(super_break == 0) {
      double dom;
      if(c == M) {
        break;
      }

      // Should buffers be reallocated?
      if(c >= k) {
        k *= 2;
        double *ts_tmp = ts_;
        int *ys_tmp = ys_;
        size_t g = LENGTH(ts);
        PROTECT(ts = allocVector(REALSXP, k));
        PROTECT(ys = allocVector(INTSXP, k));
        ts_ = REAL(ts); ys_ = INTEGER(ys);
        for(R_xlen_t i = 0; i < g; ++i) {
          ts_[i] = ts_tmp[i];
          ys_[i] = ys_tmp[i];
        }
        REPROTECT(ts, t_idx);
        REPROTECT(ys, y_idx);
        UNPROTECT(2);
        if(k > M)
          k = M;
      }

      // Call
      dom = REAL(call_f(drates, env, ts, ys, arglist, names, tn_, ts_, ys_, c))[0];

      if(dom == 0) {
        break;
      }

      while(1) {
        double u;
        double mu_dot;
        double min_s;
        double ratio;
        s += exp_rand() / dom;

        min_s = (s > tn_) ? tn_ : s;

        mu_dot = REAL(call_f(rates, env, ts, ys, arglist, names, min_s, ts_, ys_, c))[0];

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
        ratio = mu_dot / dom;
        if(was_warning == 0) {
          if(ratio > 1) {
            was_warning = 1;
            warning("Intensity ratio %f. Intensities were not properly dominated. Simulation results may be misleading.\n", ratio);
          }
        }

        if(u <= ratio) {
          SEXP pp;
          double *p;

          pp = call_f(probs, env, ts, ys, arglist, names, s, ts_, ys_, c);
          p = REAL(pp);

          ts_[c] = s;

          int nn = LENGTH(pp);
          double ss = 0;
          for(int i = 0; i < nn; ++i) {
            ss += p[i];
          }
          if(ss == 0) {
            super_break = 1;
            break;
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

    // Call arguments in arglist need to unprotected
    if(TYPEOF(args) != NILSXP) {
      UNPROTECT(arg_length);
    }

    SEXP out_1 = PROTECT(allocVector(REALSXP, c));
    double *out_1_ = REAL(out_1);

    SEXP out_2 = PROTECT(allocVector(INTSXP, c));
    int *out_2_ = INTEGER(out_2);

    SEXP out = PROTECT(allocVector(VECSXP, 5 + arg_length));

    for(R_xlen_t i = 0; i < c; ++i) {
      out_1_[i] = ts_[i];
      out_2_[i] = ys_[i];
    }

    SET_VECTOR_ELT(out, 0, out_1);
    SET_VECTOR_ELT(out, 1, out_2);
    SET_VECTOR_ELT(out, 2, PROTECT(ScalarReal(ts_[0])));
    SET_VECTOR_ELT(out, 3, PROTECT(ScalarInteger(ys_[0])));
    SET_VECTOR_ELT(out, 4, PROTECT(ScalarReal(tn_)));
    if(TYPEOF(args) != NILSXP) {
      for(R_len_t h = 0; h < arg_length; ++h) {
        SET_VECTOR_ELT(out, 5 + h, VECTOR_ELT(arglist, h));
      }
    }
    setAttrib(out, R_NamesSymbol, arg_names);
    SET_VECTOR_ELT(result, l, out);
    UNPROTECT(6);
  }
  PutRNGstate();
  R_Free(perms);
  UNPROTECT(6);
  return result;
}
