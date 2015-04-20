library(emdist)
library(rjson)
library(scales)

data <- fromJSON(file = "data/10_5_M.json")
ges.as.matrix <- function(l, scale = TRUE, uniform.time = TRUE) {
  l$y <- -l$y
  if (scale) {
    l$x <- rescale(l$x)
    l$y <- rescale(l$y)
  }  
  if (uniform.time) {
    l$t <- rescale(rank(l$t, ties.method = "min"))
  }  
  x <- l$x; y <- l$y; t <- l$t
  w <- rep(1.0 / length(l$t), length(l$t))
  
  cbind(w, x, y, t)
}
ds <- lapply(data, ges.as.matrix)

dist.f <- function(x, y) {
  sqrt(sum((x[1:2] - y[1:2])^2)) + 100 * abs(x[3] - y[3]) ^ 3
}

emd(ds[[1]], ds[[50]], dist=dist.f)

ges <- sapply(data, function(x) x$tag)
