test_that("latrix produces a list with empty data argument", {
  testthat::expect_type(latrix(ncol = 2, nrow = 2), "list")
})
