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
#' @examples
#' A 2 x 2 empty list mat
#' latrix(nrow = 2, ncol = 2)
latrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) {
  if(is.na(data)) data <- list()
  matrix(data, nrow, ncol, byrow, dimnames)
}

