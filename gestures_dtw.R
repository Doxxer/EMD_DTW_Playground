library(dtw)
library(rjson)
library(parallel)
library(scales)

data <- fromJSON(file = "data.json")
# data <- fromJSON(file = "multi.json")

ges.as.matrix <- function(l, scale = TRUE, uniform.time = TRUE, mirror.y = T) {
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
  
  cbind(w, l$x, l$y, l$t)
}

ds <- lapply(data, ges.as.matrix)

ncores <- 8
cl <- makeCluster(ncores)
dist <- matrix(NA_real_, length(ds), length(ds))

clusterExport(cl, c("ds"))
invisible(clusterEvalQ(cl, library(dtw)))

for (i in seq_along(ds)) {
  if (i %% 5 == 0) print(i)
  if (i == 1) next
  
  dist[seq_len(i - 1), i] <-
    dist[i, seq_len(i - 1)] <- 
    parSapply(cl,
              seq_len(i - 1),
              function(j, i) dtw(ds[[i]], ds[[j]], step.pattern = symmetric1, distance.only = T, dist.method = "Euclidean")$distance,
              i = i)
}
stopCluster(cl)
diag(dist) <- Inf

ges <- sapply(data, function(x) x$tag)

max.in.col <- max.col(-dist, ties.method = "first")
error <- 0
for (i in seq_along(data)) {
  error = error + dist[i, ][max.in.col[i]]
}

pred <- ges[max.in.col]
actual <- ges

print(mean(pred == actual))
print(table(actual = actual, predicted = pred))
print(error)
