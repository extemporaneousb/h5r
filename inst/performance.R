##
## Investigate performance profiles of various access patterns.
##
require(h5r)

h5 <- H5File(Sys.glob("~/local/big.h5"))

chunks <- c("1e3", "1e4", "1e5", "1e6")
names(chunks) <- chunks <- c("1e3", "1e4", "1e5", "1e6")
cDtas <- lapply(paste("cdata", chunks, sep = "_"), function(n) getH5Dataset(h5, n, inMemory = FALSE))
names(cDtas) <- chunks

f <- function(d, n = 100, mu = 1000) {
  start <- runif(n, 1, length(d))
  end   <- start + round(rexp(n, 1/mu))
  end   <- ifelse(end > length(d), start, end)
  width <- end - start + 1
  
  lapply(seq.int(1, n), function(i) {
    readSlab(d, start[i], width[i])
    return(NULL)
  })
  return(TRUE)
}

times <- replicate(100, {
  system.time(f(cDtas[[1]], n = 1000))[3]
})

pyTimes <- scan("pyres.txt")

png("numpyVR.png")
par(mar = c(8, 6, 5, 1), cex.lab = 2, cex.axis = 2, cex.main = 2)
boxplot(list("Python" = pyTimes, "R" = times), ylim = c(.075, .15),
        las = 2, main = "Random 1k dataset slice")
dev.off()


Rprof("rprof")
levelplot(x <- do.call(rbind, lapply(cDtas, function(a) {
  set.seed(10)
  sapply(chunks, function(n) {
    n <- as.numeric(n)
    system.time(f(a, N, mu = n))[3]/(n)
  })
})))
Rprof(NULL)
summaryRprof("rprof")

Rprof("rprof")
x <- f(cDtas[[1]], n = 1000, mu = 1e4)
Rprof(NULL)
summaryRprof("rprof")



##
## Different Datasets.
##
cD <- getH5Dataset(h5, "cdata_1e4", inMemory = FALSE)
uD <- getH5Dataset(h5, "udata", inMemory = FALSE)
zD <- getH5Dataset(h5, "zdata", inMemory = FALSE)
mD <- getH5Dataset(h5, "cdata", inMemory = TRUE)

par(mar=c(10, 5, 3, 3))
boxplot(as.data.frame(do.call(rbind, lapply(1:K, function(i) {
  sapply(list("chunked" = cD, "unchunked" = uD, "zipped" = zD, "memory" = mD), function(a) {
    set.seed(i)
    system.time(f(a))[3]/N
  })
}))), las = 2)

  


