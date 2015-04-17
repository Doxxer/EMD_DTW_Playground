library(scales)
x <- c(0, 1, 2, 3, 0, 1, 2, 0, 1)
rescale(rank(x, ties.method = "min"))

nthroot <- function(a, b) ifelse(b %% 2 == 1 | a >= 0,sign(a)*abs(a)^(1/b), NaN)

nthroot(c(-2, 2)^2, 5)
