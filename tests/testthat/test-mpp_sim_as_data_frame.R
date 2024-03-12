test_that("as.data.frame generic works for rmpp output", {
  set.seed(2316)
  mu_1 <- function(t, ts, ys, idx) 2 + ts[idx] / 10
  mu_2 <- function(t, ts, ys, idx) 4 + ts[idx] / 10

  p_1 <- function(t, ts, ys, idx) c(0, 1)
  p_2 <- function(t, ts, ys, idx) c(1, 0)

  x <- rmpp(
    n = 5,
    rates = list(mu_1, mu_2),
    drates = list(mu_1, mu_2),
    probs = list(p_1, p_2),
    t0 = 0,
    tn = 5,
    y0 = 1,
    mark_end = 0
  )
  testthat::expect_s3_class(as.data.frame(x), "data.frame")

  df1 <- as.data.frame(x)
  df2 <- as.data.frame(x, discard_initial = TRUE)
  testthat::expect_equal(
    nrow(df1) - nrow(df2),
    5L,
    info = "Testing that discard intial works."
  )
  testthat::expect_equal(
    df2$time[1] == 0,
    FALSE,
    info = "Testing that discard intial at least removes first row."
  )
  testthat::expect_equal(
    df1$time[1] == 0,
    TRUE,
    info = "Testing that as.data.frame does not remove first row."
  )
})
