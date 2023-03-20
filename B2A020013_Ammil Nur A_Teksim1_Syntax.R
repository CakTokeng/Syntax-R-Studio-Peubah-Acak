# fungsi Additive_RNG
Additive_RNG <- function(a, z0, c, m, n) {
  xi <- matrix(NA, n, 3)
  colnames(xi) <- c("aZ(i-1)+c", "Xi", "Ui")
  for (i in 1:n) {
    xi[i, 1] <- (a * z0 + c)
    xi[i, 2] <- xi[i, 1] %% m
    xi[i, 3] <- xi[i, 2] / m
    z0 <- xi[i, 2]
  }
  hist(xj[,3])
  View(xi)
  return(xi)
}

# menghitung nilai Y dengan rumus perulangan
n <- nrow(xi)
p <- 0.65
Y <- rep(0, n)
for (i in 1:n) {
  X <- xi[i, 2]
  Y[i] <- (X <= p) + 0
}
tabel <- table(Y) / length(Y)
print(tabel)