test_that("rmpp mark_end argument works correctly", {
  set.seed(2316)
  mu_1 <- function(t, ts, ys, idx) 2 + ts[idx] / 10
  mu_2 <- function(t, ts, ys, idx) 4 + ts[idx] / 10

  p_1 <- function(t, ts, ys, idx) c(0, 1)
  p_2 <- function(t, ts, ys, idx) c(1, 0)

  x <- rmpp(
    5,
    list(mu_1, mu_2),
    list(mu_1, mu_2),
    list(p_1, p_2),
    t0 = 0,
    tn = 5,
    y0 = 1,
    mark_end = 0
  )

  x <- x[[1]][[2]]
  expect_equal(x[length(x)], 0L, info = "Correct end mark with positive end time.")

  x <- rmpp(
    5,
    list(mu_1, mu_2),
    list(mu_1, mu_2),
    list(p_1, p_2),
    t0 = 0,
    tn = 0,
    y0 = 1,
    mark_end = 0
  )

  x <- x[[1]][[2]]
  expect_equal(x[length(x)], 0L, info = "Correct end mark with 0 end time.")
  expect_equal(length(x), 2L, info = "Correct sample length with 0 end time.")

  x <- rmpp(
    5,
    list(mu_1, mu_2),
    list(mu_1, mu_2),
    list(p_1, p_2),
    t0 = 0,
    tn = 0,
    y0 = 1
  )

  x <- x[[1]][[2]]
  expect_equal(x[length(x)], 1L, info = "No end mark for mark_end = 0 with 0 end time.")
  expect_equal(length(x), 1L, info = "Correct sample length with 0 end time and mark_end = 0.")
})
