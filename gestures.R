library(emdist)
library(e1071)
library(MASS)
library(rjson)
library(parallel)
library(scales)

data <- fromJSON(file = "data/10_5_S.json")

ges.as.matrix <- function(l, scale = TRUE, uniform.time = TRUE, mirror.y = TRUE) {
  if (mirror.y) {
    l$y <- -l$y
  }
  
  if (scale) {
    l$x <- rescale(l$x)
    l$y <- rescale(l$y)
  }

  if (uniform.time) {
    l$t <- rescale(rank(l$t, ties.method = "min"))
  }
  
  w <- rep(1, length(l$t))
  w <- w / sum(w)
  
  cbind(w, l$x, l$y, l$t)
}

ds <- lapply(data, ges.as.matrix)

ncores <- 8
cl <- makeCluster(ncores)
dist <- matrix(NA_real_, length(ds), length(ds))

dist.f <- function(x, y) {
  sqrt(sum((x[1:2] - y[1:2])^2)) + 100 * abs(x[3] - y[3]) ^ 3
}

clusterExport(cl, c("ds"))
clusterExport(cl, c("dist.f"))
invisible(clusterEvalQ(cl, library(emdist)))

for (i in seq_along(ds)) {
  if (i %% 5 == 0) print(i)
  if (i == 1) next
  
  dist[seq_len(i - 1), i] <- dist[i, seq_len(i - 1)] <- parSapply(cl, seq_len(i - 1),
                                                                  function(j, i) emd(ds[[i]], ds[[j]], dist = dist.f),
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
acc <- pred == actual

print(mean(acc))
print(table(actual = actual, predicted = pred))
print(error)
