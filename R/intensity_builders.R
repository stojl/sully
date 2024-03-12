#' build_rates
#'
#' Constructs rates from a matrix of intensities to be used with rmpp.
#'
#' @param x matrix
#'
#' @return list of functions
#' @export
build_rates <- function(x) {
  n <- dim(x)[1]
  not_null <- x[!sapply(x, is.null)]
  if(length(not_null) > 0) {
    args <- names(formals(not_null[[1]]))
  } else {
    args <- c("t", "ts", "ys", "idx")
  }
  args <- paste0(args, collapse = ", ")

  rates <- lapply(1:n, function(i) {
    null_rates <- sapply(x[i, ], is.null)
    if(all(null_rates)) {
      func <- paste0("function(", args, ") 0")
      func <- parse(text = func)
      return(eval(func))
    }

    subargs <- paste0("x", 1:(sum(!null_rates)))
    subexpr <- paste0(subargs, collapse = " + ")
    subexpr <- paste0("function(", args, ") {", subexpr, "}")
    subexpr <- parse(text = subexpr)
    subvars <- lapply(x[i, !null_rates], body)
    names(subvars) <- subargs

    f <- do.call(substitute, list(subexpr[[1]], subvars))
    eval(f)
  })
  rates
}

#' build_probs
#'
#' Build transitions probabilities from matrix of intensities to be used with
#' rmpp.
#'
#' @param x matrix of intensities.
#'
#' @return list of functions
#' @export
build_probs <- function(x) {
  n <- dim(x)[1]
  not_null <- x[!sapply(x, is.null)]
  if(length(not_null) > 0) {
    args <- names(formals(not_null[[1]]))
  } else {
    args <- c("t", "ts", "ys", "idx")
  }
  args <- paste0(args, collapse = ", ")
  rates <- lapply(1:n, function(i) {
    null_rates <- sapply(x[i, ], is.null)
    if(all(null_rates)) {
      func <- paste0("function(", args, ") rep(0, ", n, ")")
      func <- parse(text = func)
      return(eval(func))
    }

    subargs <- paste0("x", 1:n)
    subexpr <- paste0(subargs, collapse = ", ")
    subexpr <- paste0("function(", args,") {c(", subexpr, ")}")
    subexpr <- parse(text = subexpr)
    subvars <- lapply(x[i, ], function(s) if(is.null(s)) 0 else body(s))
    names(subvars) <- subargs

    f <- do.call(substitute, list(subexpr[[1]], subvars))
    eval(f)
  })
  rates
}

#' dominate
#'
#' Automatic domination of intensities used in rmpp.
#'
#' @param fns A list of functions
#'
#' @return A list of functions
#' @export
dominate <- function(fns) {
  fn <- fns[[1]]
  args <- paste0(
    names(formals(fn))[-1],
    " = ",
    names(formals(fn))[-1],
    collapse = ", "
  )
  args2 <- names(formals(fn))
  args2 <- paste0(args2, collapse = ", ")
  lapply(fns, function(farg_) {
    force(farg_)
    parse_tree <- deparse(farg_)
    if(identical(parse_tree[2], "{")) {
      text_body <- parse_tree[3]
    } else {
      text_body <- parse_tree[2]
    }
    if(identical(text_body, "0") | identical(text_body, "    0")) {
      callex <- paste0("function(", args2, ") 0")
    } else {
      callex <- paste0(
        "function(", args2, ") {",
        "stats::optimize(farg_, c(ts[idx], t),
      ", args,
        ", maximum=TRUE)$objective}"
      )
    }
    callex <- parse(text = callex)
    eval(callex)
  })
}