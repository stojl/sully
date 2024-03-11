#' latrix
#'
#' A wrapper around matrix in order to create a list matrix.
#'
#' @param data data
#' @param nrow number of rows
#' @param ncol number of columns
#' @param byrow should data be arranged by row instead of by column?
#' @param dimnames A dimnames attribute for the matrix.
#'
#' @return matrix
#' @export
#'
latrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) {
  if(identical(is.na(data), TRUE)) data <- list()
  matrix(data, nrow, ncol, byrow, dimnames)
}

