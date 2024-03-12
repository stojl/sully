from_semimarkov <- function(fn) {
  force(fn)
  function(t, ts, ys, idx, ...) {
    fn(t, t - ts[idx], ...)
  }
}

#' rsemimarkov
#'
#' @description This is a wrapper around rmpp for finite multi-state semimarkov
#'   simulations. The rates needs to have the form function(t, u, ...) where ...
#'   could be additional arguments. Eg. function(t, u, v) for some vector 'v'.
#'   Here 'u' is the duration.
#'
#'   If build_rates or build_probs are used make sure to use type = "semimarkov"
#'   to assure correct construction of rates.
#'
#' @param n Number of paths to be simulated
#' @param rates List of functions returning outflow of rates from each state.
#' @param probs List of functions that return probability of jumping to each
#'   state.
#' @param t0 Starting time. Must have length 1 or same length as number of
#'   paths.
#' @param tn Stoping time. Must have length 1 or same length as number of paths.
#' @param y0 Starting marks. Must have length 1 or same length as number of
#'   paths.
#' @param mark_end If end time tn is reached before absorbing state then it is
#'   marked by a mark specified by mark_end.
#' @param drates List of functions that dominates each outflow rate in 'rates'.
#'   Intensities in 'rates' arguments are dominated by default using the
#'   optimize routine of the stats package. This is noticeably slower than
#'   dominating them manually and it is recommended to do so as optimize will
#'   not always obtain an exact upper bound leading to warnings.
#' @param limit Jump limit. If this limit is reached the simulation of the
#'   particular path will stop. This is a safety measure.
#' @param ... Additional arguments to be passed to the rate functions. This
#'   could be baseline parameters such as age, gender etc. They must be
#'   numerics, doubles or integers.
#'
#' @return list of simulated paths
#' @export
rsemimarkov <- function(n,
                        rates,
                        probs,
                        t0,
                        tn,
                        y0 = 1L,
                        mark_end = NULL,
                        drates = NULL,
                        limit = 1e8,
                        ...) {
  rates <- lapply(rates, from_semimarkov)
  probs <- lapply(probs, from_semimarkov)
  if(is.null(drates)) {
    drates <- dominate(rates)
  } else {
    drates <- lapply(drates, from_semimarkov)
  }
  rmpp(
    n,
    rates,
    probs,
    t0,
    tn,
    y0,
    mark_end,
    drates,
    limit,
    ...
  )
}
