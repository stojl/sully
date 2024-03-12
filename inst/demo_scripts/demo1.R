library(sully)
library(magrittr)
library(dplyr)

mu_12 <- function(t, ts, ys, idx, w, v) {
  exp(-3 + 0.15 * t + w)
}

mu_1d <- function(t, ts, ys, idx, w, v) {
  (w / 10) * exp(0.1 * v)
}

mu12_dom <- function(t, ts, ys, idx, w, v) {
  exp(-3 + 0.15 * 10 + w)
}

microbenchmark::microbenchmark(
  test = mu12_dom(0.3, 0.3, 0.3, 1, 0.2, 0.4)
)

intens <- matrix(list(
  NULL, mu_12, mu_1d,
  NULL, NULL, NULL,
  NULL, NULL, NULL
), byrow = TRUE, ncol = 3)

intens_dom <- intens
intens_dom[[1, 2]] <- mu12_dom

v_quantile <- function(p, rate, t) {
  - log(1 - (1 - exp(-rate * t)) * p) / rate
}

n <- 50000
w <- rbinom(n, 1, 0.5)
v <- v_quantile(runif(n), 1 / 4, 10)


rts <- build_rates(intens)
drts <- build_rates(intens_dom)
prbs <- build_probs(intens)

rmpp(
  n,
  rates = rts,
  probs = prbs,
  t0 = v,
  tn = 10,
  y0 = 1,
  mark_end = 0,
  w = w,
  v = v
)

test %>%
  as.data.frame(discard_initial = TRUE) %>%
  dplyr::group_by(mark) %>%
  dplyr::summarise(prob = dplyr::n() / {n})