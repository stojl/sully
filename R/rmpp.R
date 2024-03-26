#' rmpp
#'
#' @description Simulate paths of marked point process on a finite interval.
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
#'
#' @examples
#' ## Life-Death model simulation on the interval 0 to 10.
#'
#' # Cumulative intensity rates out of each state. In the case of a two state
#' # model each intensity is also the outflow rate.
#'
#' mu_12 <- function(t, ts, ys, idx) exp(0.0015 * t)
#' mu_21 <- function(t, ts, ys, idx) 0
#'
#' mu_12_dom <- function(t, ts, ys, idx) exp(0.0015 * 10)
#'
#' p_12 <- function(t, ts, ys, idx) c(0, 1)
#' p_21 <- function(t, ts, ys, idx) c(0, 0)
#'
#' # mark_end = 0 means that another "jump" will be recorded if a path has not
#' # reached an absorbing state before the time limit specified by tn. An
#' # arbitrary mark can be used to mark these "jumps". If leaft out, no "jump"
#' # will be recorded.
#'
#' sims1 <- rmpp(
#'   10,
#'   rates = list(mu_12, mu_21),
#'   drates = list(mu_12_dom, mu_21),
#'   probs = list(p_12, p_21),
#'   t0 = 0,
#'   tn = 10,
#'   mark_end = 0
#' )
#'
#' # We can output the result as a data.frame
#' sims1_df <- as.data.frame(sims1)
#'
#' ## Simulation from multi-state models
#'
#' # For multi-state model, it can be useful to use build_rates and build_probs
#' # to compute the total intensity outflow of each state together with the jump
#' # probabilities.
#'
#' mu_12 <- function(t, ts, ys, idx) exp(0.0015 * t)
#' mu_13 <- function(t, ts, ys, idx) exp(0.015 * t)
#' mu_21 <- function(t, ts, ys, idx) exp(0.3 * t)
#' mu_23 <- function(t, ts, ys, idx) exp(0.001 * t)
#'
#' # If an off-diagnoal entry is NULL the intensity is 0
#'
#' intensity_matrix <- matrix(list(
#'   NULL, mu_12, mu_13,
#'   mu_21, NULL, mu_23,
#'   NULL, NULL, NULL
#' ), byrow = TRUE, ncol = 3)
#'
#' my_rates <- build_rates(intensity_matrix)
#' my_probs <- build_probs(intensity_matrix)
#'
#' # We could manually specify rates that dominate our rates, but rmpp can do it
#' # automatically using the optimize() function. The simulation will be slower
#' # than if you specify it manually.
#'
#' sims2 <- rmpp(
#'   10,
#'   rates = my_rates,
#'   probs = my_probs,
#'   t0 = 0,
#'   tn = 10,
#'   mark_end = 0
#' )
#'
#' # data.frame output
#'
#' sims2_df <- as.data.frame(sims2)
#'
#' # The intial state is always recorded. If we wish to exclude these we can do
#' # this by setting discard_initial = TRUE.
#'
#' sims2_df_no_first <- as.data.frame(sims2, discard_initial = TRUE)
#'
#' # It is possible to specify different starting times and starting states for
#' # each simulated path.
#'
#' sims3 <- rmpp(
#'   10,
#'   rates = my_rates,
#'   probs = my_probs,
#'   t0 = seq(0, 5, length.out = 10L),
#'   y0 = c(1, 2, 2, 1, 1, 2, 2, 1, 2, 1),
#'   tn = 10,
#'   mark_end = 0
#' )
#'
#' ## Simulating from duration dependent intensities
#'
#' # We specify a 3 state model 1 -> 2 -> 3 where 3 is absorbing. The intensity
#' # from 2 -> 3 is duration dependent. The entire history of the marked point
#' # process is passed through the arguments ts and ys. The idx argument
#' # is the index of the last jump in the ts and ys vectors.
#'
#' # WARNING: Only entries of from 1 to idx of the vectors ts and ys should be
#' # used. If something like sum(ts) is needed it should be done as
#' # sum(ts[1:idx]). Anything beyond index idx is used as a buffer for further
#' # jumps.
#'
#' mu_12 <- function(t, ts, ys, idx) exp(0.0015 * t)
#' mu_23 <- function(t, ts, ys, idx) exp(0.001 * t + 0.15 * (t - ts[idx]))
#'
#' tmatrix <- matrix(list(
#'   NULL, mu_12, NULL,
#'   NULL, NULL, mu_23,
#'   NULL, NULL, NULL
#' ), byrow = TRUE, ncol = 3)
#'
#' my_rates2 <- build_rates(tmatrix)
#' my_probs2 <- build_probs(tmatrix)
#'
#' sims4 <- rmpp(
#'   10,
#'   rates = my_rates2,
#'   probs = my_probs2,
#'   t0 = 0,
#'   y0 = 1,
#'   tn = 10,
#'   mark_end = 0
#' )
#'
#' ## Simulating from intensities with baseline covariates
#'
#' # It is simple to include baseline covariates in simulation process
#'
#' # All intensity functions must have the arguments even if they do not depend
#' # on the baseline covariate w. The covariates must be numerics - either
#' # integers or doubles. More than one covariate can be specified by adding more
#' # arguments.
#'
#' mu_12 <- function(t, ts, ys, idx, w) exp(0.0015 * t + 0.1 * w)
#' mu_13 <- function(t, ts, ys, idx, w) exp(0.02 * t)
#'
#' tmatrix <- matrix(list(
#'   NULL, mu_12, mu_13,
#'   NULL, NULL, NULL,
#'   NULL, NULL, NULL
#' ), byrow = TRUE, ncol = 3)
#'
#' my_rates3 <- build_rates(tmatrix)
#' my_probs3 <- build_probs(tmatrix)
#'
#' # We simulate baseline covariates
#'
#' ws <- rbinom(10, 1, 0.75)
#'
#' # The covariates are passed as additional arguments to rmpp
#'
#' sims4 <- rmpp(
#'   10,
#'   rates = my_rates3,
#'   probs = my_probs3,
#'   t0 = 0,
#'   y0 = 1,
#'   tn = 10,
#'   mark_end = 0,
#'   w = ws
#' )
#'
#' # The covariates are included in the data.frame output
#'
#' sims4_df <- as.data.frame(sims4)
rmpp <- function(n,
                 rates,
                 probs,
                 t0,
                 tn,
                 y0 = 1L,
                 mark_end = NULL,
                 drates = NULL,
                 limit = 1e8,
                 ...) {
  if(is.null(drates)) drates <- dominate(rates)
  n <- as.integer(n)
  t0 <- as.double(t0)
  tn <- as.double(tn)
  if(is.null(y0)) {
    y0 <- 1L
  } else {
    if(!(length(t0) %in% c(1L, n)))
      stop("y0 must have length 1 or n")
  }
  y0 <- as.integer(y0)
  if(!is.null(mark_end)) mark_end <- as.integer(mark_end)

  args <- list(...)
  if(length(args) == 0) {
    args <- NULL
  } else {
    arg_lengths <- lengths(args)
    if(!(min(arg_lengths) %in% c(1L, n)) | !(max(arg_lengths) %in% c(1L, n)))
      stop("Arguments in ... must have length 1 or n")
  }
  limit <- as.integer(limit)
  if(!(length(t0) %in% c(1L, n)))
    stop("t0 must have length 1 or n")
  if(length(rates) == 0)
    stop("Must have non-zero amount of states.")
  if(length(rates) != length(drates))
    stop("Argument 'rates', 'drates' and 'probs' must all be of same length.")
  if(length(rates) != length(probs))
    stop("Argument 'rates', 'drates' and 'probs' must all be of same length.")

  sim <- .Call("C_rmpp", n, rates, drates, probs, t0, tn, y0, mark_end, limit, args, new.env())
  structure(
    sim,
    class = "mpp_sim",
    tn = tn,
    marked_end = mark_end
  )
}

#' print method for rmpp simulations.
#'
#' @param x Simulation of Marked Point Process by rmpp.
#' @param n Number of paths to be displayed
#' @param digits Amount of digits to be displayed
#' @param ... not used.
#'
#' @return Text display
#' @export
print.mpp_sim <- function(x, ..., n = 1, digits = 2) {
  n <- min(n, length(x))
  if(length(x) == 1) {
    paths <- "path."
  } else {
    paths <- "paths."
  }
  if(is.null(attr(x, "marked_end"))) {
    me <- "None"
  } else {
    me <- attr(x, "marked_end")
  }

  out <- vector("list", n * 3 + 7)
  out[[1]] <- "Marked Point Procces Simulation containing"
  out[[2]] <- length(x)
  out[[3]] <- paths
  out[[4]] <- "\n"
  out[[5]] <- "End mark:"
  out[[6]] <- me
  out[[7]] <- "\n\n"
  for(i in 1:n) {
    p <- x[[i]]
    pT <- p[[1]]
    pY <- p[[2]]
    if(length(pT) > 10) {
      tT <- paste0(
        format(round(utils::head(pT, 5), digits), nsmall = digits),
        collapse = ", "
      )
      tT <- paste0(tT, ", ..., ")
      tT <- paste0(tT, paste0(format(round(utils::tail(pT, 3), digits), nsmall = digits), collapse = ", "))
      tY <- paste0(utils::head(pY, 5), collapse = ", ")
      tY <- paste0(tY, ", ..., ")
      tY <- paste0(tY, paste0(utils::tail(pY, 3), collapse = ", "))
    } else {
      tT <- paste0(format(round(pT, digits), nsmall = digits), collapse = ", ")
      tY <- paste0(pY, collapse = ", ")
    }

    out[[3 * (i - 1) + 8]] <- paste0("Path ", i, ":\n")
    out[[3 * (i - 1) + 1 + 8]] <- paste0("Jump times: ", tT, "\n")
    out[[3 * (i - 1) + 2 + 8]] <- paste0("Jump marks: ", tY, "\n\n")
  }

  do.call(cat, out)
}

#' as.data.frame implementation for mmp_sim
#'
#' @param x Simulation of Marked Point Process by rmpp.
#' @param discard_initial Should inital state and start time be discarded.
#' @param ... not used.
#'
#' @return data.frame
#' @export
as.data.frame.mpp_sim <- function(x, ..., discard_initial = FALSE) {
  data.frame(.Call("C_summary_rmpp", x, discard_initial))
}
