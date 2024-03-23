kfide <- function(x, t0, tn, u, h, keep_last = FALSE) {
  x_dims <- dim(x)
  p <- x_dims[1]

  n <- as.integer((tn - t0) / h)
  k <- as.integer(u / h) + 1L

  ps <- lapply(x, function(i) {
    s <- numeric((n + 1) * (n + k))
    dim(s) <- c(n + k, n + 1)
    s
  })

  dim(ps) <- c(p, p)

  # Initialize boundary values. The other entries are already 0's.
  for(i in 1:p) {
    ps[[i, i]][k, 1] <- 1
  }

  s <- t0
  # We use an Euler-scheme to solve the differential equations.
  # The first iteration deals with a point mass in the integrals
  for(i in 1:p) {
    # The second integral in KFIDE
    for(j in 1:p) {
      if(i == j) {
        ps[[i, j]][k + 1, 2] <- ps[[i, j]][k, 1] - h * x[[i, j]](s, u)
      } else {
        ps[[i, j]][2:(k + 1), 2] <- ps[[i, j]][k, 1] + h * x[[i, j]](s, u)
      }
    }
  }

  k <- k + 1L
  s <- s + h
  # Main loop
  for(m in 2:n) {
    for(i in 1:p) {
      for(j in 1:p) {
        # We first calculate the second integral of KFIDE
        int_2 <- 0
        for(l in (1:p)[-j]) {
          mus <- x[[l, j]](s, seq(0, u + s - t0, h))
          if(length(mus) == 1L) {
            # Intensity is independent of duration
            int_2 <- int_2 + mus * ps[[i, l]][k, m]
          } else {
            # Trapezoid rule
            probs <- ps[[i, l]][1:k, m]
            probs <- probs[2:k] - probs[1:(k - 1L)]
            mus <- mus[2:k] - mus[1:(k - 1L)]
            int_2 <- int_2 + sum(probs * mus / 2)
          }
        }

        # We calculate first integral
        mus <- x[[j, j]](s, seq(0, u + s - t0, h))
        if(length(mus) == 1L) {
          # Intensity is independent of duration
          int_1 <- -mus * ps[[i, j]][1:k, m]
        } else {
          # Trapezoid rule
          probs <- ps[[i, j]][1:k, m]
          probs <- probs[2:k] - probs[1:(k - 1L)]
          mus <- mus[2:k] - mus[1:(k - 1L)]
          int_1 <- -cumsum(probs * mus / 2)
        }
        ps[[i, j]][2:(k + 1), m + 1] <- ps[[i, j]][1:k, m] + (int_1 + int_2) * h
      }
    }
    s <- s + h
    k <- k + 1L
  }
  if(keep_last) {
    ps <- lapply(ps, function(p) p[, n + 1])
    dim(ps) <- c(p, p)
    ps
  } else {
    ps
  }
}
