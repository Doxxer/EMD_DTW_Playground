library(emdist)
library(rjson)
library(scales)

data <- fromJSON(file = "data/11_10_SM.json")
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
  
  sid <- l$id;
  
  cbind(w, x, y, t, sid)
}
ds <- lapply(data, ges.as.matrix)

dist.f <- function(x, y) {
  if (x[4] != y[4]) {
    return(-1000);
  } else {
    return(sqrt(sum((x[1:2] - y[1:2])^2)) + 100 * abs(x[3] - y[3]) ^ 3);
  }  
}

error = 0
for (i in seq_len(10)+0) {
  for (j in seq_len(10)+0) {
    if (i == j) next;
    dist <- emd(ds[[i]], ds[[j]], dist=dist.f)
    error <- error + dist
    print(c(i, j, dist))
  }
}
print(error)