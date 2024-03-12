test_that("Automatic domination in rmpp works", {
  mu_1 <- function(t, ts, ys, idx) 2 + ts[idx] / 10
  mu_2 <- function(t, ts, ys, idx) 4 + ts[idx] / 10

  p_1 <- function(t, ts, ys, idx) c(0, 1)
  p_2 <- function(t, ts, ys, idx) c(1, 0)

  x <- rmpp(
    n = 5,
    rates = list(mu_1, mu_2),
    probs = list(p_1, p_2),
    t0 = 0,
    tn = 5,
    y0 = 1,
    mark_end = 0
  )

  testthat::expect_type(x, "list")
})
