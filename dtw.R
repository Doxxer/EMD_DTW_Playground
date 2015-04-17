library(dtw)
library(rjson)
library(scales)

data <- fromJSON(file = "full_onetouch.json")
ges.as.matrix <- function(l, scale = TRUE, uniform.time = TRUE, mirror.y = TRUE) {
  if (mirror.y) {
    l$y <- -l$y
  }
  
  if (scale) {
    l$x <- rescale(l$x)
    l$y <- rescale(l$y)
  }
  
  if (uniform.time) {
    l$t <- seq(from = 0, to = 1, length.out = length(l$t))
  }
  
  w <- rep(1, length(l$t))
  w <- w / sum(w)
  
  x <- l$x; y <- l$y; t <- l$t
  cbind(x, y, t, w)
}
ds <- lapply(data, ges.as.matrix)

calc.all <- function(a, b) {
  plot(a, type='o', col='blue', xlim=range(b), ylim=range(b))
  lines(b, col='red', type='o')
  
  alignment<-dtw(a, b, keep=T, distance.only = F, step=symmetric1, dist.method = "Euclidean")
  
  for (k in seq_along(alignment$index1)) {
    p1 <- a[alignment$index1[k], ]
    p2 <- b[alignment$index2[k], ]  
    lines(c(p1[1], p2[1]), c(p1[2], p2[2]), col = 'black', lty=3)
  }

  alignment$distance  
}

error <- 0

for (i in seq_len(10)+30) {
  for (j in seq_len(10)+90) {
    if (i == j) next;
    dist <- calc.all(ds[[i]], ds[[j]])
    error <- error + dist
    print(c(i, j, dist))
  }
}

print(error)
