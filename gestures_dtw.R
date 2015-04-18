library(proxy)
library(dtw)
library(rjson)
library(parallel)
library(scales)

data <- fromJSON(file = "data/20_10_SM.json")
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
  cbind(x, y)
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
              function(j, i) dtw(ds[[i]], ds[[j]], distance.only = T, dist.method = "Euclidean")$normalizedDistance,
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

print(table(actual = actual, predicted = pred))
print(c(mean(pred == actual) * 100, error))