library(dtw)
library(rjson)
library(scales)

data <- fromJSON(file = "data/h.json")
ges.as.matrix <- function(l, scale = TRUE, uniform.time = TRUE) {
  l$y <- -l$y # mirror y
  
  if (scale) {
    l$x <- rescale(l$x)
    l$y <- rescale(l$y)
  }  
  if (uniform.time) {
    l$t <- rescale(rank(l$t, ties.method = "min"))
  }  
  x <- l$x; y <- l$y; t <- l$t
  cbind(x, y, t)
}
ds <- lapply(data, ges.as.matrix)

calc.all <- function(a, b) {
  plot(a, type='o', col='blue', xlim=range(b), ylim=range(b))
  lines(b, col='red', type='o')
  
  alignment<-dtw(a, b, keep=T, distance.only = F, step.pattern = symmetric1, dist.method = "Euclidean")
  
  for (k in seq_along(alignment$index1)) {
    p1 <- a[alignment$index1[k], ]
    p2 <- b[alignment$index2[k], ]  
    lines(c(p1[1], p2[1]), c(p1[2], p2[2]), col = 'black', lty=3)
  }

  alignment$distance
}

error <- 0
for (i in seq_len(6)+0) {
  for (j in seq_len(6)+6) {
    if (i == j) next;
    dist <- calc.all(ds[[i]], ds[[j]])
    error <- error + dist
    print(c(i, j, dist))
  }
}
print(error)
