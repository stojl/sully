test_that("rmpp runs with no errors", {
  set.seed(2316)
  mu_1 <- function(t, ts, ys, idx) 2 + ts[idx] / 10
  mu_2 <- function(t, ts, ys, idx) 4 + ts[idx] / 10

  p_1 <- function(t, ts, ys, idx) c(0, 1)
  p_2 <- function(t, ts, ys, idx) c(1, 0)

  testthat::expect_type(
    rmpp(
      5,
      list(mu_1, mu_2),
      list(mu_1, mu_2),
      list(p_1, p_2),
      t0 = 0,
      tn = 5,
      y0 = 1
    ),
    type = "list"
  )

  testthat::expect_no_error(
    rmpp(
      5,
      list(mu_1, mu_2),
      list(mu_1, mu_2),
      list(p_1, p_2),
      t0 = 0,
      tn = 5,
      y0 = 1
    ),
    message = "rmpp works without errors."
  )

  testthat::expect_no_error(
    rmpp(
      5,
      list(mu_1, mu_2),
      list(mu_1, mu_2),
      list(p_1, p_2),
      t0 = 0,
      tn = 5,
      y0 = 1,
      mark_end = 0
    ),
    message = "rmpp works without errors using mark_end argument."
  )

  testthat::expect_no_error(
    rmpp(
      5,
      list(mu_1, mu_2),
      list(mu_1, mu_2),
      list(p_1, p_2),
      t0 = 0,
      tn = 0,
      y0 = 1,
      mark_end = 0
    ),
    message = "rmpp works without errors using mark_end argument with 0 end time."
  )

  testthat::expect_no_error(
    rmpp(
      5,
      list(mu_1, mu_2),
      list(mu_1, mu_2),
      list(p_1, p_2),
      t0 = 0,
      tn = 0,
      y0 = 1
    ),
    message = "rmpp works without errors with 0 end time."
  )
})
