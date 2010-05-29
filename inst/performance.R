##
## Investigate performance profiles of various access patterns.
##
require(h5r)

h5 <- H5File(Sys.glob("~/local/big.h5"))

chunks <- c("1e3", "1e4", "1e5", "1e6")
names(chunks) <- chunks <- c("1e3", "1e4", "1e5", "1e6")
cDtas <- lapply(paste("cdata", chunks, sep = "_"), function(n) getH5Dataset(h5, n, inMemory = FALSE))
names(cDtas) <- chunks

N <- 1000
K <- 1000

f <- function(d, n = N, mu = 1000) {
  start <- runif(n, 1, length(d))
  end  <- start + round(rexp(n, 1/mu))
  end <- ifelse(end > length(d), start, end)
  
  mapply(function(s,e) {
    z <- d[s:e]
  }, start, end)

  return(TRUE)
}

g <- function(d, n = N, mu = 1000) {
  start <- runif(n, 1, length(d))
  end  <- start + round(rexp(n, 1/mu))
  end <- ifelse(end > length(d), start, end)
  
  mapply(function(s,e) {
    z <- d[matrix(c(s,e), ncol = 2)]
  }, start, end)

  return(TRUE)
}

system.time(f(cDtas[[1]], n = 100000))
system.time(g(cDtas[[1]], n = 100000))



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

  


