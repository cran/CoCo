times <- read.table("times.dat")
times <- times[,-(5:6)]
o <- order(times[, 2])
x <- times[o, 2]
names(x) <- times[o, 1]
x <- cbind(x, cumsum(x))
x <- cbind(x, 100 * x[, (1:2)]/x[nrow(x), 2])
x <- cbind(x, 100 * x[, (1:2)]/x[nrow(x)-1, 2])
times[o,]
round(x * 1000) / 1000
q()

