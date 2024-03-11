#' rmpp Simulate paths of marked point process on a finite interval.
#'
#' @param n Number of paths to be simulated
#' @param rates List of functions returning outflow of rates from each state.
#' @param drates List of functions that dominates each outflow rate in 'rates'.
#' @param probs List of functions that return probability of jumping to each
#'   state.
#' @param t0 Starting time. Must have length 1 or same length as number of
#'   paths.
#' @param tn Stoping time. Must have length 1 or same length as number of paths.
#' @param y0 Starting marks. Must have length 1 or same length as number of
#'   paths.
#' @param mark_end If end time tn is reached before absorbing state then it is
#'   marked by a mark specified by mark_end.
#' @param limit Jump limit. If this limit is reached the simulation of the
#'   particular path will stop. This is a safety measure.
#' @param ... Additional arguments to be passed to the rate functions. This
#'   could be baseline parameters such as age, gender etc. They must be
#'   numerics, doubles or integers.
#'
#' @return list of simulated paths
#' @export
rmpp <- function(n, rates, drates, probs, t0, tn, y0 = NULL, mark_end = NULL, limit = 1e8, ...) {
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

  structure(
    .Call("C_rmpp", n, rates, drates, probs, t0, tn, y0, mark_end, limit, args, new.env()),
    class = "mpp_sim",
    tn = tn,
    marked_end = mark_end
  )
  # rmpp2(n, rates, drates, probs, t0, tn, y0, limit)
}

#' print method for rmpp simulations.
#'
#' @param x Simulation of Marked Point Process by rmpp.
#' @param digits Amount of digits to be displayed.
#' @param ... not used.
#'
#' @return Text display
#' @export
print.mpp_sim <- function(x, ..., digits = 2) {
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
  p <- x[[1]]
  pT <- p[[1]]
  pY <- p[[2]]
  if(length(pT) > 10) {
    tT <- paste0(format(round(utils::head(pT, 5), digits), nsmall = digits), collapse = ", ")
    tT <- paste0(tT, ", ..., ")
    tT <- paste0(tT, paste0(format(round(utils::tail(pT, 3), digits), nsmall = digits), collapse = ", "))
    tY <- paste0(utils::head(pY, 5), collapse = ", ")
    tY <- paste0(tY, ", ..., ")
    tY <- paste0(tY, paste0(utils::tail(pY, 3), collapse = ", "))
  } else {
    tT <- paste0(format(round(pT, digits), nsmall = digits), collapse = ", ")
    tY <- paste0(pY, collapse = ", ")
  }
  cat("Marked Point Procces Simulation containing", length(x), paths, "\n",
      "Time limit:", attr(x, "tn"), "\n",
      "End mark:", me, "\n\n",
      "Path 1:\n",
      "Jump times:", tT, "\n",
      "Jump marks:", tY)
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
